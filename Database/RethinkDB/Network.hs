{-# LANGUAGE RecordWildCards, OverloadedStrings, DeriveDataTypeable, NamedFieldPuns #-}

-- TODO: the code sends an extra query after getting SUCCESS_ATOM when doing e.g. (expr 1)

module Database.RethinkDB.Network (
  RethinkDBHandle(..),
  connect,
  close,
  use,
  runQLQuery,
  Cursor,
  makeCursor,
  next,
  collect,
  nextResponse,
  Response(..),
  SuccessCode(..),
  ErrorCode(..),
  RethinkDBError(..),
  RethinkDBConnectionError(..),
  ) where

import Control.Monad (when, forever)
import Data.Typeable (Typeable)
import Network (HostName, connectTo, PortID(PortNumber))
import System.IO (Handle, hClose, hIsEOF)
import Data.ByteString.Lazy (pack, unpack, hPut, hGet, ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.UTF8 as BS (fromString)
import qualified Data.Text as T
import Control.Concurrent (
  writeChan, MVar, Chan, modifyMVar, takeMVar, forkIO, readChan,
  myThreadId, newMVar, ThreadId, newChan, killThread,
  newEmptyMVar, putMVar, mkWeakMVar)
import Data.Bits (shiftL, (.|.), shiftR)
import Data.Monoid ((<>), mconcat)
import Control.Applicative ((<$>))
import Data.Foldable hiding (forM_)
import Control.Exception (catch, Exception, throwIO, SomeException(..))
import Data.Aeson (toJSON, object, (.=), (.:), Value(Null, Bool), decode)
import qualified Data.Aeson as J
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef, writeIORef)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe)
import Control.Monad.Fix (fix)
import Data.Int (Int64)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Mem.Weak (finalize)
import Data.Binary.Get (Get, runGet, getWord32be, getWord64be, getBytes)
import Data.Binary.Put (Put, runPut, putWord32be, putWord64be, putLazyByteString)
import Data.Word (Word64, Word32)
import qualified Data.HashMap.Strict as HM

import Database.RethinkDB.Wire
import Database.RethinkDB.Wire.Response
import Database.RethinkDB.Wire.Query
import Database.RethinkDB.Wire.VersionDummy
import Database.RethinkDB.Objects
import Database.RethinkDB.ReQL (
  Term, Backtrace, convertBacktrace, WireQuery(..))

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
  cursorMap :: Datum -> a }

instance Functor Cursor where
  fmap f Cursor{ .. } = Cursor { cursorMap = f . cursorMap, .. }

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
    putWord32be magicNumber
    putWord32be (fromIntegral $ B.length auth)
    putLazyByteString auth
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

-- TODO: withHandle is only used for writes. why not reads?
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
  } deriving (Typeable, Show)

instance Exception RethinkDBError

-- | The raw response to a query
data Response = ErrorResponse {
  errorResponse :: RethinkDBError
  } | SuccessResponse {
  successCode :: SuccessCode,
  successResult:: QueryResult
  }

data QueryResult = Atom Datum | Sequence [Datum] deriving Show

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

data SuccessCode =
  SuccessPartial RethinkDBHandle Int64 |
  Success
  deriving Show

instance Show Response where
  show (ErrorResponse RethinkDBError {..}) =
    show errorCode ++ ": " ++
    show errorMessage ++ " (" ++
    show errorBacktrace ++ ")"
  show SuccessResponse {..} = show successCode ++ ": " ++ show successResult

newtype WireResponse = WireResponse { responseJSON :: Value }

convertResponse :: RethinkDBHandle -> Term -> Int64 -> WireResponse -> Response
convertResponse h q t (WireResponse (J.Object o)) = let
  type_    = o .? "t" >>= fromWire
  results  = o .? "r"
  bt       = o .? "b" --> maybe [] convertBacktrace
  _profile = o .? "p" -- TODO
  m (.?) k = HM.lookup k m >>= resultToMaybe . J.fromJSON
  resultToMaybe (J.Success a) = Just a
  resultToMaybe (J.Error _) = Nothing
  (-->) = flip ($)
  e = fromMaybe "" $ listToMaybe results
  in case type_ of
  Just SUCCESS_ATOM | [atom] <- results -> SuccessResponse Success atom
  Just SUCCESS_PARTIAL -> SuccessResponse (SuccessPartial h t) results
  Just SUCCESS_SEQUENCE -> SuccessResponse Success results
  Just CLIENT_ERROR -> ErrorResponse $ RethinkDBError ErrorBrokenClient q e bt
  Just COMPILE_ERROR -> ErrorResponse $ RethinkDBError ErrorBadQuery q e bt
  Just RUNTIME_ERROR -> ErrorResponse $ RethinkDBError ErrorRuntime q e bt
  Just WAIT_COMPLETE -> SuccessResponse Success []
  Nothing -> ErrorResponse $ RethinkDBError ErrorUnexpectedResponse q e bt
      -- TODO: nicer backtrace

runQLQuery :: RethinkDBHandle -> WireQuery -> Term -> IO (MVar Response)
runQLQuery h query term = do
  tok <- newToken h
  mbox <- addMBox h tok term
  sendQLQuery h tok query
  return mbox

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
      putWord64be tok
      putWord32be (fromIntegral $ B.length queryS)
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
  let token = runGet getWord64be tokenString
  header <- hGet (rdbHandle h) 4
  when (B.length header /= 4) $
    throwIO $ RethinkDBConnectionError "RethinkDB connection closed unexpectedly"
  let replyLength = runGet getWord32be header
  rawResponse <- hGet (rdbHandle h) replyLength
  let parsedResponse = J.decode rawResponse
  case parsedResponse of
    Left errMsg -> fail errMsg
    Right (response, rest)
      | B.null rest -> dispatch token response
      | otherwise -> fail "readSingleResponse: invalid reply length"

  where
  dispatch Nothing _ = return ()
  dispatch (Just tok) response = do
    mboxes <- readIORef $ rdbWait h
    case M.lookup tok mboxes of
      Nothing -> return ()
      Just (mbox, term, closetok) -> do
        let convertedResponse = convertResponse h term tok response
        writeChan mbox convertedResponse
        when (isLastResponse convertedResponse) $ closetok

isLastResponse :: Response -> Bool
isLastResponse ErrorResponse{} = True
isLastResponse SuccessResponse{ successCode = Success } = True
isLastResponse SuccessResponse{ successCode = SuccessPartial{} } = False

-- | Set the default database
--
-- The new handle is an alias for the old one. Calling close on either one
-- will close both.
use :: RethinkDBHandle -> Database -> RethinkDBHandle
use h db' = h { rdbDatabase = db' }

-- | Close an open connection
close :: RethinkDBHandle -> IO ()
close RethinkDBHandle{ rdbHandle, rdbThread } = do
  killThread rdbThread
  hClose rdbHandle

closeToken :: RethinkDBHandle -> Token -> IO ()
closeToken h tok = do
  let query = WireQuery $ toJSON [toWire STOP]
  sendQLQuery h tok query

nextResponse :: Response -> IO ()
nextResponse SuccessResponse { successCode = SuccessPartial h tok } = do
  let query = WireQuery $ toJSON [toWire CONTINUE]
  sendQLQuery h tok query
nextResponse _ = return ()

makeCursor :: MVar Response -> IO (Cursor Datum)
makeCursor cursorMBox = do
  cursorBuffer <- newMVar (Right ([], False))
  return Cursor{..}
  where cursorMap = id

-- | Get the next value from a cursor
next :: Cursor a -> IO (Maybe a)
next c@Cursor{ .. } = modifyMVar cursorBuffer $ fix $ \loop mbuffer ->
  case mbuffer of
    Left err -> throwIO err
    Right ([], True) -> return (Right ([], True), Nothing)
    Right (x:xs, end) -> return $ (Right (xs, end), Just (cursorMap x))
    Right ([], False) -> cursorFetchBatch c >>= loop

cursorFetchBatch :: Cursor a -> IO (Either RethinkDBError ([Datum], Bool))
cursorFetchBatch c = do
  response <- takeMVar (cursorMBox c)
  case response of
    ErrorResponse e -> return $ Left e
    SuccessResponse Success datums -> return $ Right (datums, True)
    SuccessResponse SuccessPartial{} datums -> return $ Right (datums, False)

-- | A lazy stream of all the elements in the cursor
collect :: Cursor a -> IO [a]
collect c = fix $ \loop -> do
    n <- next c
    case n of
      Nothing -> return []
      Just x -> do
        xs <- unsafeInterleaveIO $ loop
        return $ x : xs
