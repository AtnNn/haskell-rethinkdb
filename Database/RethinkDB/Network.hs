{-# LANGUAGE RecordWildCards, OverloadedStrings, DeriveDataTypeable, NamedFieldPuns, PatternGuards #-}

-- TODO: the code sends an extra query after getting SUCCESS_ATOM when doing e.g. (expr 1)

module Database.RethinkDB.Network (
  RethinkDBHandle(..),
  connect,
  close,
  use,
  runQLQuery,
  Cursor(..),
  makeCursor,
  next,
  nextBatch,
  collect,
  collect',
  nextResponse,
  Response(..),
  Profile,
  ErrorCode(..),
  RethinkDBError(..),
  RethinkDBConnectionError(..),
  More,
  noReplyWait,
  each
  ) where

import Control.Monad (when, forever, forM_)
import Data.Typeable (Typeable)
import Network (HostName)
import Network.Socket (
  socket, Family(AF_INET), SocketType(Stream), sClose,
  SockAddr(SockAddrInet), setSocketOption, SocketOption(NoDelay),
  Socket)
import qualified Network.Socket as Socket
import Network.BSD (getProtocolNumber, getHostByName, hostAddress)
import Network.Socket.ByteString.Lazy (sendAll)
import Network.Socket.ByteString (recv)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.UTF8 as BS (fromString)
import qualified Data.ByteString as BS
import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (
  writeChan, Chan, forkIO, readChan, myThreadId, ThreadId, newChan, killThread,
  MVar, newMVar, modifyMVar, newEmptyMVar, putMVar, mkWeakMVar, takeMVar, tryTakeMVar)
import Control.Exception (catch, Exception, throwIO, SomeException(..), bracketOnError)
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef, writeIORef)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe, isNothing)
import Control.Monad.Fix (fix)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Mem.Weak (finalize)
import Data.Binary.Get (runGet, getWord32le, getWord64le)
import Data.Binary.Put (runPut, putWord32le, putWord64le, putLazyByteString)
import Data.Word (Word64, Word32, Word16)
import qualified Data.HashMap.Strict as HM

import Database.RethinkDB.Wire
import Database.RethinkDB.Wire.Response
import Database.RethinkDB.Wire.Query
import Database.RethinkDB.Wire.VersionDummy as Protocol
import Database.RethinkDB.Types
import Database.RethinkDB.Datum
import Database.RethinkDB.ReQL (
  Term, Backtrace, convertBacktrace, WireQuery(..),
  WireBacktrace(..), Term(..), Frame(..),
  TermAttribute(..))
import Data.Foldable (toList)

-- $setup
--
-- Get the doctests ready
--
-- >>> import qualified Database.RethinkDB as R
-- >>> import Database.RethinkDB.NoClash
-- >>> h' <- unsafeInterleaveIO $ connect "localhost" 28015 def
-- >>> let h = use "doctests" h'

type Token = Word64

-- | A connection to the database server
data RethinkDBHandle = RethinkDBHandle {
  rdbSocket :: Socket,
  rdbWriteLock :: MVar (Maybe SomeException),
  rdbToken :: IORef Token, -- ^ The next token to use
  rdbDatabase :: Database,  -- ^ The default database
  rdbWait :: IORef (Map Token (Chan (Response, Maybe Profile), Term, IO ())),
  rdbThread :: ThreadId
  }

data Cursor a = Cursor {
  cursorMBox :: MVar Response,
  cursorPBox :: MVar (Maybe Profile),
  cursorBuffer :: MVar (Either RethinkDBError ([Datum], Bool)),
  cursorMap :: Datum -> IO a }

instance Functor Cursor where
  fmap f Cursor{ .. } = Cursor { cursorMap = fmap f . cursorMap, .. }

instance Show RethinkDBHandle where
  show RethinkDBHandle{ rdbSocket } = "RethinkDB Connection " ++ show rdbSocket

newToken :: RethinkDBHandle -> IO Token
newToken RethinkDBHandle{rdbToken} =
  atomicModifyIORef' rdbToken $ \x -> (x+1, x)

data RethinkDBConnectionError =
  RethinkDBConnectionError String
  deriving (Show, Typeable)
instance Exception RethinkDBConnectionError

connectTo :: HostName -> Word16 -> IO Socket
connectTo host port = do
  proto <- getProtocolNumber "tcp"
  bracketOnError (socket AF_INET Stream proto) sClose $ \sock -> do
    -- TODO: ipv6
    he <- getHostByName host
    Socket.connect sock (SockAddrInet (fromIntegral port) (hostAddress he))
    setSocketOption sock NoDelay 1
    return sock

-- | Create a new connection to the database server
--
-- /Example:/ connect using the default port with no passphrase
--
-- >>> h <- connect "localhost" 28015 Nothing

connect :: HostName -> Integer -> Maybe String -> IO RethinkDBHandle
connect host port mauth = do
  let auth = B.fromChunks . return . BS.fromString $ fromMaybe "" mauth
  s <- connectTo host (fromInteger port)
  sendAll s $ runPut $ do
    putWord32le magicNumber
    putWord32le (fromIntegral $ B.length auth)
    putLazyByteString auth
    putWord32le $ fromIntegral $ toWire Protocol.JSON
  res <- sGetNullTerminatedString s
  when (res /= "SUCCESS") $ throwIO (RethinkDBConnectionError $ show res)
  r <- newIORef 1
  let db' = Database "test"
  wlock <- newMVar Nothing
  waits <- newIORef M.empty
  let rdb = RethinkDBHandle s wlock r db' waits
  tid <- forkIO $ readResponses rdb
  return $ rdb tid

recvAll :: Socket -> Int -> IO ByteString
recvAll s n_ = go [] n_ where
  go acc 0 = return $ B.fromChunks $ reverse acc
  go acc n = do
    d <- recv s n
    if BS.null d
      then throwIO $ RethinkDBConnectionError "Connection closed unexpectedly"
      else go (d : acc) (n - BS.length d)

sGetNullTerminatedString :: Socket -> IO ByteString
sGetNullTerminatedString s = go [] where
  go acc = do
    c <- recv s 1
    if BS.null c || c == BS.pack [0]
      then return (B.fromChunks (reverse acc))
      else go (c : acc)

magicNumber :: Word32
magicNumber = fromIntegral $ toWire V0_3

withSocket :: RethinkDBHandle -> (Socket -> IO a) -> IO a
withSocket RethinkDBHandle{ rdbSocket, rdbWriteLock } f =
  modifyMVar rdbWriteLock $ \mex ->
  case mex of
    Nothing -> do
      a <- f rdbSocket
      return (Nothing, a)
    Just ex -> throwIO ex

data RethinkDBError = RethinkDBError {
  errorCode :: ErrorCode,
  errorTerm :: Term,
  errorMessage :: String,
  errorBacktrace :: Backtrace
  } deriving (Typeable)

instance Exception RethinkDBError

instance Show RethinkDBError where
  show (RethinkDBError code term message backtrace) =
    show code ++ ": " ++ show message ++
    if term == Datum Null
       then ""
       else "\n" ++ indent ("in " ++ show (annotate backtrace term))
    where
      indent = (\x -> case x of [] -> []; _ -> init x) . unlines . map ("  "++) . lines
      annotate :: Backtrace -> Term -> Term
      annotate (x : xs) t | Just new <- inside x t (annotate xs) = new
      annotate _ t = Note "HERE" t
      inside (FramePos n) (Term tt a oa) f
        | n < length a = Just $ Term tt (take n a ++ [f (a!!n)] ++ drop (n+1) a) oa
      inside (FrameOpt k) (Term tt a oa) f
        | Just (before, v, after) <- extract k oa =
          Just $ Term tt a $ before ++ [TermAttribute k (f v)] ++ after
      inside _ _ _ = Nothing
      extract _ [] = Nothing
      extract k (TermAttribute kk v : xs) | k == kk = Just ([], v, xs)
      extract k (x:xs) =
        case extract k xs of
          Nothing -> Nothing
          Just (a,b,c) -> Just (x:a,b,c)

data Response =
  ResponseError RethinkDBError |
  ResponseSingle Datum |
  ResponseBatch (Maybe More) [Datum]

type Profile = Datum

data More = More {
  _moreFeed :: Bool,
  _moreHandle :: RethinkDBHandle,
  _moreToken :: Token
  }

data ErrorCode =
  ErrorBrokenClient |
  ErrorBadQuery |
  ErrorRuntime |
  ErrorUnexpectedResponse

instance Show ErrorCode where
  show ErrorBrokenClient = "RethinkDB: Broken client error"
  show ErrorBadQuery = "RethinkDB: Malformed query error"
  show ErrorRuntime = "RethinkDB: Runtime error"
  show ErrorUnexpectedResponse = "RethinkDB: Unexpected response"

instance Show Response where
  show (ResponseError RethinkDBError {..}) =
    show errorCode ++ ": " ++
    show errorMessage ++ " (" ++
    show errorBacktrace ++ ")"
  show (ResponseSingle datum) = show datum
  show (ResponseBatch _more batch) = show batch

newtype WireResponse = WireResponse { _responseDatum :: Datum }

convertResponse :: RethinkDBHandle -> Term -> Token -> WireResponse -> (Response, Maybe Profile)
convertResponse h q t (WireResponse (Object o)) = let
  type_    = o .? "t" >>= fromWire
  results :: Maybe [Datum]
  results  = o .? "r"
  bt       = o .? "b" --> maybe [] (convertBacktrace . WireBacktrace)
  profile :: Maybe Datum
  profile = o .? "p"
  atom :: Maybe Datum
  atom     = case results of Just [single] -> Just single; _ -> Nothing
  m .? k   = HM.lookup k m >>= resultToMaybe . fromDatum
  (-->) = flip ($)
  e = fromMaybe "" $ resultToMaybe . fromDatum =<< listToMaybe =<< results
  _ <!< Nothing = ResponseError $ RethinkDBError ErrorUnexpectedResponse q e bt
  f <!< (Just a) = f a
  in case type_ of
  Just SUCCESS_ATOM -> (ResponseSingle <!< atom, profile)
  Just SUCCESS_PARTIAL -> (ResponseBatch (Just $ More False h t) <!< results, profile)
  Just SUCCESS_FEED -> (ResponseBatch (Just $ More True h t) <!< results, profile)
  Just SUCCESS_SEQUENCE -> (ResponseBatch Nothing <!< results, profile)
  Just CLIENT_ERROR -> (ResponseError $ RethinkDBError ErrorBrokenClient q e bt, Nothing)
  Just COMPILE_ERROR -> (ResponseError $ RethinkDBError ErrorBadQuery q e bt, Nothing)
  Just RUNTIME_ERROR -> (ResponseError $ RethinkDBError ErrorRuntime q e bt, Nothing)
  Just WAIT_COMPLETE -> (ResponseSingle (toDatum True), profile)
  Nothing -> (ResponseError $ RethinkDBError ErrorUnexpectedResponse q e bt, Nothing)

convertResponse _ q _ (WireResponse json) =
  (ResponseError $
  RethinkDBError ErrorUnexpectedResponse q ("Response is not a JSON object: " ++ show json) [],
  Nothing)

runQLQuery :: RethinkDBHandle -> WireQuery -> Term -> IO (MVar Response, MVar (Maybe Profile))
runQLQuery h query term = do
  tok <- newToken h
  let noReply = isNoReplyQuery query
  (mbox, pbox) <- if noReply
                  then (,) <$> newEmptyMVar <*> newEmptyMVar
                  else addMBox h tok term
  sendQLQuery h tok query
  when noReply $ do
    putMVar mbox $ ResponseSingle Null
    putMVar pbox Nothing
  return (mbox, pbox)

isNoReplyQuery :: WireQuery -> Bool
isNoReplyQuery (WireQuery (Array v)) |
  [_type, _term, (Object optargs)] <- toList v,
  Just (Bool True) <- HM.lookup "noreply" optargs =
    True
isNoReplyQuery _ = False

addMBox :: RethinkDBHandle -> Token -> Term -> IO (MVar Response, MVar (Maybe Profile))
addMBox h tok term = do
  chan <- newChan
  pbox <- newEmptyMVar
  mbox <- newEmptyMVar
  mWeak <- mkWeakMVar mbox $ do
    closeToken h tok -- TODO: don't close if already closed
    atomicModifyIORef' (rdbWait h) $ \mboxes ->
      (M.delete tok mboxes, ())
  atomicModifyIORef' (rdbWait h) $ \mboxes ->
    (M.insert tok (chan, term, finalize mWeak) mboxes, ())
  _ <- forkIO $ fix $ \loop -> do
    (response, profile) <- readChan chan
    putMVar mbox response
    putMVar pbox profile
    when (not $ isLastResponse response) $ do
      nextResponse response
      loop
  return (mbox, pbox)

sendQLQuery :: RethinkDBHandle -> Token -> WireQuery -> IO ()
sendQLQuery h tok query = do
  let queryS = encode $ queryJSON query
  withSocket h $ \s -> do
    sendAll s $ runPut $ do
      putWord64le tok
      putWord32le (fromIntegral $ B.length queryS)
      putLazyByteString queryS

data RethinkDBReadError =
  RethinkDBReadError SomeException
  deriving (Show, Typeable)
instance Exception RethinkDBReadError

readResponses :: (ThreadId -> RethinkDBHandle) -> IO ()
readResponses h' = do
  tid <- myThreadId
  let h = h' tid
  let handler e@SomeException{} = do
        sClose $ rdbSocket h
        modifyMVar (rdbWriteLock h) $ \_ -> return (Just e, ())
        writeIORef (rdbWait h) M.empty
  flip catch handler $ forever $ readSingleResponse h

readSingleResponse :: RethinkDBHandle -> IO ()
readSingleResponse h = do
  tokenString <- recvAll (rdbSocket h) 8
  when (B.length tokenString /= 8) $
    throwIO $ RethinkDBConnectionError "RethinkDB connection closed unexpectedly"
  let token = runGet getWord64le tokenString
  header <- recvAll (rdbSocket h) 4
  when (B.length header /= 4) $
    throwIO $ RethinkDBConnectionError "RethinkDB connection closed unexpectedly"
  let replyLength = runGet getWord32le header
  rawResponse <- recvAll (rdbSocket h) (fromIntegral replyLength)
  let parsedResponse = eitherDecode rawResponse
  case parsedResponse of
    Left errMsg -> do
      -- TODO: don't give up on the connection, only share the error message with the MVar
      fail errMsg
    Right response -> dispatch token $ WireResponse response

  where
  dispatch tok response = do
    mboxes <- readIORef $ rdbWait h
    case M.lookup tok mboxes of
      Nothing -> return ()
      Just (mbox, term, closetok) -> do
        let (convertedResponse, profile) = convertResponse h term tok response
        writeChan mbox (convertedResponse, profile)
        when (isLastResponse convertedResponse) $ closetok

isLastResponse :: Response -> Bool
isLastResponse ResponseError{} = True
isLastResponse ResponseSingle{} = True
isLastResponse (ResponseBatch (Just _) _) = False
isLastResponse (ResponseBatch Nothing _) = True

-- | Set the default database
--
-- The new handle is an alias for the old one. Calling close on either one
-- will close both.
use :: Database -> RethinkDBHandle -> RethinkDBHandle
use db' h = h { rdbDatabase = db' }

-- | Close an open connection
close :: RethinkDBHandle -> IO ()
close h@RethinkDBHandle{ rdbSocket, rdbThread } = do
  noReplyWait h
  killThread rdbThread
  sClose rdbSocket

closeToken :: RethinkDBHandle -> Token -> IO ()
closeToken h tok = do
  let query = WireQuery $ toDatum [toWire STOP]
  sendQLQuery h tok query

nextResponse :: Response -> IO ()
nextResponse (ResponseBatch (Just (More _ h tok)) _) = do
  let query = WireQuery $ toDatum [toWire CONTINUE]
  sendQLQuery h tok query
nextResponse _ = return ()

makeCursor :: MVar Response -> MVar (Maybe Profile) -> IO (Cursor Datum, Maybe Profile)
makeCursor cursorMBox cursorPBox = do
  profile <- takeMVar cursorPBox
  cursorBuffer <- newMVar (Right ([], False))
  return (Cursor{..}, profile)
  where cursorMap = return . id

-- | Get the next value from a cursor
next :: Cursor a -> IO (Maybe a)
next c@Cursor{ .. } = modifyMVar cursorBuffer $ fix $ \loop mbuffer ->
  case mbuffer of
    Left err -> throwIO err
    Right ([], True) -> return (Right ([], True), Nothing)
    Right (x:xs, end) -> do x' <- cursorMap x; return (Right (xs, end), Just x')
    Right ([], False) -> cursorFetchBatch c >>= loop

-- | Get the next batch from a cursor
nextBatch :: Cursor a -> IO [a]
nextBatch c@Cursor{ .. } = modifyMVar cursorBuffer $ fix $ \loop mbuffer ->
  case mbuffer of
    Left err -> throwIO err
    Right ([], True) -> return (Right ([], True), [])
    Right (xs@(_:_), end) -> do
      xs' <- mapM cursorMap xs
      return $ (Right ([], end), xs')
    Right ([], False) -> cursorFetchBatch c >>= loop

cursorFetchBatch :: Cursor a -> IO (Either RethinkDBError ([Datum], Bool))
cursorFetchBatch c = do
  response <- takeMVar (cursorMBox c)
  _ <- tryTakeMVar (cursorPBox c)
  case response of
    ResponseError e -> return $ Left e
    ResponseBatch more datums -> return $ Right (datums, isNothing more)
    ResponseSingle(Array a) -> return $ Right (toList a, True)
    ResponseSingle _ ->
      return . Left $ RethinkDBError ErrorUnexpectedResponse (Datum Null)
      "Expected a stream or an array but got a datum" []

-- | A lazy stream of all the elements in the cursor
collect :: Cursor a -> IO [a]
collect c = fix $ \loop -> do
    b <- nextBatch c
    case b of
      [] -> return []
      xs -> do
        ys <- unsafeInterleaveIO $ loop
        return $ xs ++ ys

-- | A strict version of collect
collect' :: Cursor a -> IO [a]
collect' c = fix $ \loop -> do
    b <- nextBatch c
    case b of
      [] -> return []
      xs -> do
        ys <- loop
        return $ xs ++ ys

-- | Wait for NoReply queries to complete on the server
--
-- >>> ((), Nothing) <- runOpts h [NoReply] $ table "users" # get "bob" # update (\row -> merge row ["occupation" := "teacher"])
-- >>> noReplyWait h
noReplyWait :: RethinkDBHandle -> IO ()
noReplyWait h = do
  (r, m) <- runQLQuery h (WireQuery $ toDatum [toWire NOREPLY_WAIT]) (Datum Null)
  _ <- takeMVar r
  _ <- takeMVar m
  return ()

each :: Cursor a -> (a -> IO b) -> IO ()
each cursor f = do
  batch <- nextBatch cursor
  if null batch
    then return ()
    else do
      forM_ batch f
      each cursor f
