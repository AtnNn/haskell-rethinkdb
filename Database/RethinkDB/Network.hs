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
  ErrorCode(..),
  RethinkDBError(..),
  RethinkDBConnectionError(..),
  More,
  noReplyWait,
  each,
  closeCursor
  ) where

import Control.Monad (when, forever, forM_)
import Data.Typeable (Typeable)
import Network (HostName, connectTo, PortID(PortNumber))
import System.IO (Handle, hClose, hIsEOF)
import Data.ByteString.Lazy (hPut, hGet, ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.UTF8 as BS (fromString)
import Control.Concurrent (
  writeChan, MVar, Chan, modifyMVar, takeMVar, forkIO, readChan,
  myThreadId, newMVar, ThreadId, newChan, killThread,
  newEmptyMVar, putMVar, mkWeakMVar)
import Data.Monoid((<>))
import Control.Exception (catch, Exception, throwIO, SomeException(..))
import Data.Aeson (toJSON, Value)
import qualified Data.Aeson as J
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef, writeIORef)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe, isNothing)
import Control.Monad.Fix (fix)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Mem.Weak (finalize)
import Data.Binary.Get (runGet, getWord32le, getWord64le)
import Data.Binary.Put (runPut, putWord32le, putWord64le, putLazyByteString)
import Data.Word (Word64, Word32)
import qualified Data.HashMap.Strict as HM

import Database.RethinkDB.Wire
import Database.RethinkDB.Wire.Response
import Database.RethinkDB.Wire.Query
import Database.RethinkDB.Wire.VersionDummy as Protocol
import Database.RethinkDB.Objects
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
  rdbHandle :: Handle,
  rdbWriteLock :: MVar (Maybe SomeException),
  rdbToken :: IORef Token, -- ^ The next token to use
  rdbDatabase :: Database,  -- ^ The default database
  rdbWait :: IORef (Map Token (Chan Response, Term, IO ())),
  rdbThread :: ThreadId
  }

data Cursor a = Cursor {
  cursorMBox :: MVar Response,
  cursorBuffer :: MVar (Either RethinkDBError ([Datum], Bool)),
  cursorMap :: Datum -> IO a }

instance Functor Cursor where
  fmap f Cursor{ .. } = Cursor { cursorMap = fmap f . cursorMap, .. }

instance Show RethinkDBHandle where
  show RethinkDBHandle{ rdbHandle } = "RethinkDB Connection " ++ show rdbHandle

newToken :: RethinkDBHandle -> IO Token
newToken RethinkDBHandle{rdbToken} =
  atomicModifyIORef' rdbToken $ \x -> (x+1, x)

data RethinkDBConnectionError =
  RethinkDBConnectionError String
  deriving (Show, Typeable)
instance Exception RethinkDBConnectionError

-- | Create a new connection to the database server
--
-- /Example:/ connect using the default port with no passphrase
--
-- >>> h <- connect "localhost" 28015 Nothing

connect :: HostName -> Integer -> Maybe String -> IO RethinkDBHandle
connect host port mauth = do
  let auth = B.fromChunks . return . BS.fromString $ fromMaybe "" mauth
  h <- connectTo host (PortNumber (fromInteger port))
  hPut h $ runPut $ do
    putWord32le magicNumber
    putWord32le (fromIntegral $ B.length auth)
    putLazyByteString auth
    putWord32le $ fromIntegral $ toWire Protocol.JSON
  res <- hGetNullTerminatedString h
  when (res /= "SUCCESS") $ throwIO (RethinkDBConnectionError $ show res)
  r <- newIORef 1
  let db' = Database "test"
  wlock <- newMVar Nothing
  waits <- newIORef M.empty
  let rdb = RethinkDBHandle h wlock r db' waits
  tid <- forkIO $ readResponses rdb
  return $ rdb tid

hGetNullTerminatedString :: Handle -> IO ByteString
hGetNullTerminatedString h = go "" where
  go acc = do
    end <- hIsEOF h
    if end then return acc else do
      c <- B.hGet h 1
      if c == B.pack [0] then return acc else
        go (acc <> c)

magicNumber :: Word32
magicNumber = fromIntegral $ toWire V0_3

withHandle :: RethinkDBHandle -> (Handle -> IO a) -> IO a
withHandle RethinkDBHandle{ rdbHandle, rdbWriteLock } f =
  modifyMVar rdbWriteLock $ \mex ->
  case mex of
    Nothing -> do
      a <- f rdbHandle
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
    show code ++ ": " ++ show message ++ "\n" ++
    indent ("in " ++ show (annotate backtrace term))
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

-- | The response to a query
data Response =
  ResponseError RethinkDBError |
  ResponseSingle Datum |
  ResponseBatch (Maybe More) [Datum]

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
  show ErrorBrokenClient = "broken client error"
  show ErrorBadQuery = "malformed query error"
  show ErrorRuntime = "runtime error"
  show ErrorUnexpectedResponse = "unexpected response"

instance Show Response where
  show (ResponseError RethinkDBError {..}) =
    show errorCode ++ ": " ++
    show errorMessage ++ " (" ++
    show errorBacktrace ++ ")"
  show (ResponseSingle datum) = show datum
  show (ResponseBatch _more batch) = show batch

newtype WireResponse = WireResponse { _responseJSON :: Value }

convertResponse :: RethinkDBHandle -> Term -> Token -> WireResponse -> Response
convertResponse h q t (WireResponse (J.Object o)) = let
  type_    = o .? "t" >>= fromWire
  results :: Maybe [Datum]
  results  = o .? "r"
  bt       = o .? "b" --> maybe [] (convertBacktrace . WireBacktrace)
  -- _profile = o .? "p" -- TODO
  atom :: Maybe Datum
  atom     = case results of Just [single] -> Just single; _ -> Nothing
  m .? k   = HM.lookup k m >>= resultToMaybe . J.fromJSON
  resultToMaybe (J.Success a) = Just a
  resultToMaybe (J.Error _) = Nothing
  (-->) = flip ($)
  e = fromMaybe "" $ resultToMaybe . J.fromJSON =<< listToMaybe =<< results
  _ <!< Nothing = ResponseError $ RethinkDBError ErrorUnexpectedResponse q e bt
  f <!< (Just a) = f a
  in case type_ of
  Just SUCCESS_ATOM -> ResponseSingle <!< atom
  Just SUCCESS_PARTIAL -> ResponseBatch (Just $ More False h t) <!< results
  Just SUCCESS_FEED -> ResponseBatch (Just $ More True h t) <!< results
  Just SUCCESS_SEQUENCE -> ResponseBatch Nothing <!< results
  Just CLIENT_ERROR -> ResponseError $ RethinkDBError ErrorBrokenClient q e bt
  Just COMPILE_ERROR -> ResponseError $ RethinkDBError ErrorBadQuery q e bt
  Just RUNTIME_ERROR -> ResponseError $ RethinkDBError ErrorRuntime q e bt
  Just WAIT_COMPLETE -> ResponseSingle (toJSON True)
  Nothing -> ResponseError $ RethinkDBError ErrorUnexpectedResponse q e bt

convertResponse _ q _ (WireResponse json) =
  ResponseError $
  RethinkDBError ErrorUnexpectedResponse q ("Response is not a JSON object: " ++ show json) []

runQLQuery :: RethinkDBHandle -> WireQuery -> Term -> IO (MVar Response)
runQLQuery h query term = do
  tok <- newToken h
  let noReply = isNoReplyQuery query
  mbox <- if noReply
          then newEmptyMVar
          else addMBox h tok term
  sendQLQuery h tok query
  when noReply $ putMVar mbox $ ResponseSingle J.Null
  return mbox

isNoReplyQuery :: WireQuery -> Bool
isNoReplyQuery (WireQuery (J.Array v)) |
  [_type, _term, (J.Object optargs)] <- toList v,
  Just (J.Bool True) <- HM.lookup "noreply" optargs =
    True
isNoReplyQuery _ = False

addMBox :: RethinkDBHandle -> Token -> Term -> IO (MVar Response)
addMBox h tok term = do
  chan <- newChan
  mbox <- newEmptyMVar
  weak <- mkWeakMVar mbox $ do
    closeToken h tok
    atomicModifyIORef' (rdbWait h) $ \mboxes ->
      (M.delete tok mboxes, ())
  atomicModifyIORef' (rdbWait h) $ \mboxes ->
    (M.insert tok (chan, term, finalize weak) mboxes, ())
  _ <- forkIO $ fix $ \loop -> do
    response <- readChan chan
    putMVar mbox response
    when (not $ isLastResponse response) $ do
      nextResponse response
      loop
  return mbox

sendQLQuery :: RethinkDBHandle -> Token -> WireQuery -> IO ()
sendQLQuery h tok query = do
  let queryS = J.encode $ queryJSON query
  withHandle h $ \s ->
    hPut s $ runPut $ do
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
        hClose $ rdbHandle h
        modifyMVar (rdbWriteLock h) $ \_ -> return (Just e, ())
        writeIORef (rdbWait h) M.empty
  flip catch handler $ forever $ readSingleResponse h

readSingleResponse :: RethinkDBHandle -> IO ()
readSingleResponse h = do
  tokenString <- hGet (rdbHandle h) 8
  when (B.length tokenString /= 8) $
    throwIO $ RethinkDBConnectionError "RethinkDB connection closed unexpectedly"
  let token = runGet getWord64le tokenString
  header <- hGet (rdbHandle h) 4
  when (B.length header /= 4) $
    throwIO $ RethinkDBConnectionError "RethinkDB connection closed unexpectedly"
  let replyLength = runGet getWord32le header
  rawResponse <- hGet (rdbHandle h) (fromIntegral replyLength)
  let parsedResponse = J.eitherDecode rawResponse
  case parsedResponse of
    Left errMsg -> fail errMsg
    Right response -> dispatch token $ WireResponse response

  where
  dispatch tok response = do
    mboxes <- readIORef $ rdbWait h
    case M.lookup tok mboxes of
      Nothing -> return ()
      Just (mbox, term, closetok) -> do
        let convertedResponse = convertResponse h term tok response
        writeChan mbox convertedResponse
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
close h@RethinkDBHandle{ rdbHandle, rdbThread } = do
  noReplyWait h
  killThread rdbThread
  hClose rdbHandle

closeToken :: RethinkDBHandle -> Token -> IO ()
closeToken h tok = do
  let query = WireQuery $ toJSON [toWire STOP]
  sendQLQuery h tok query

nextResponse :: Response -> IO ()
nextResponse (ResponseBatch (Just (More _ h tok)) _) = do
  let query = WireQuery $ toJSON [toWire CONTINUE]
  sendQLQuery h tok query
nextResponse _ = return ()

makeCursor :: MVar Response -> IO (Cursor Datum)
makeCursor cursorMBox = do
  cursorBuffer <- newMVar (Right ([], False))
  return Cursor{..}
  where cursorMap = return . id

-- | Get the next value from a cursor
next :: Cursor a -> IO (Maybe a)
next c@Cursor{ .. } = modifyMVar cursorBuffer $ fix $ \loop mbuffer ->
  case mbuffer of
    Left err -> throwIO err
    Right ([], True) -> return (Right ([], True), Nothing)
    Right (x:xs, end) -> do x' <- cursorMap x; return $ (Right (xs, end), Just x')
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
  case response of
    ResponseError e -> return $ Left e
    ResponseBatch more datums -> return $ Right (datums, isNothing more)
    ResponseSingle (J.Array a) -> return $ Right (toList a, True)
    ResponseSingle _ ->
      return $ Left $ RethinkDBError ErrorUnexpectedResponse (Datum J.Null)
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
-- >>> () <- runOpts h [NoReply] $ table "users" # get "bob" # update (\row -> merge row ["occupation" := "teacher"])
-- >>> noReplyWait h
noReplyWait :: RethinkDBHandle -> IO ()
noReplyWait h = do
  m <- runQLQuery h (WireQuery $ toJSON [toWire NOREPLY_WAIT]) (Datum J.Null)
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

closeCursor :: Cursor a -> IO ()
closeCursor = undefined