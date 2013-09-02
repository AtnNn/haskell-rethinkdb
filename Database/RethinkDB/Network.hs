{-# LANGUAGE RecordWildCards, OverloadedStrings, DeriveDataTypeable, NamedFieldPuns #-}

module Database.RethinkDB.Network (
  RethinkDBHandle
  ) where

import Control.Monad (when, forever)
import Data.Typeable (Typeable)
import Network (HostName, connectTo, PortID(PortNumber))
import System.IO (Handle, hClose, hIsEOF)
import Data.ByteString.Lazy (pack, unpack, hPut, hGet, ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS (fromString)
import qualified Data.Text as T
import Control.Concurrent (
  writeChan, MVar, Chan, modifyMVar, readMVar, forkIO, readChan,
  myThreadId, newMVar, ThreadId, withMVar, newChan,
  newEmptyMVar, putMVar)
import Data.Bits (shiftL, (.|.), shiftR)
import Data.Monoid ((<>))
import Data.Foldable hiding (forM_)
import Control.Exception (catches, Exception, throwIO)
import Data.Aeson (toJSON, object, (.=), Value(Null, Bool))
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Control.Monad.Fix (fix)
import Data.Int (Int64)

import Text.ProtocolBuffers.Basic (uToString)
import Text.ProtocolBuffers (messagePut, defaultValue, messageGet)

import Database.RethinkDB.Protobuf.Ql2.Query2 as Query
import Database.RethinkDB.Protobuf.Ql2.Query2.QueryType
import Database.RethinkDB.Protobuf.Ql2.Response2 as Response
import Database.RethinkDB.Protobuf.Ql2.Response2.ResponseType
import Database.RethinkDB.Protobuf.Ql2.Datum as Datum
import Database.RethinkDB.Protobuf.Ql2.Datum.DatumType
import Database.RethinkDB.Protobuf.Ql2.Datum.AssocPair
import Database.RethinkDB.Protobuf.Ql2.VersionDummy.Version

import Database.RethinkDB.Objects as O
import Database.RethinkDB.Term (
  BaseTerm, Backtrace, convertBacktrace)

type Token = Int64

-- | A connection to the database server
data RethinkDBHandle = RethinkDBHandle {
  rdbHandle :: Handle,
  rdbWriteLock :: MVar (),
  rdbToken :: IORef Token, -- ^ The next token to use
  rdbDatabase :: Database,  -- ^ The default database
  rdbWait :: IORef (Map Token (Chan Response, BaseTerm)),
  rdbThread :: ThreadId
  }

data Cursor a = Cursor {
  cursorMBox :: MVar Response,
  cursorBuffer :: MVar (Either RethinkDBError ([O.Datum], Bool)),
  cursorMap :: O.Datum -> a }

instance Functor Cursor where
  fmap f Cursor{ .. } = Cursor { cursorMap = f . cursorMap, .. }

instance Show RethinkDBHandle where
  show RethinkDBHandle{ rdbHandle } = "RethinkDB Connection " ++ show rdbHandle

newToken :: RethinkDBHandle -> IO Int64
newToken RethinkDBHandle{rdbToken} =
  atomicModifyIORef' rdbToken $ \x -> (x+1, x)

data RethinkDBConnectionError =
  RethinkDBConnectionError ByteString
  deriving (Show, Typeable)
instance Exception RethinkDBConnectionError

-- | Create a new connection to the database server
--
-- /Example:/ connect using the default port
--
-- >>> h <- openConnection "localhost" 28015

openConnection :: HostName -> Integer -> Maybe String -> IO RethinkDBHandle
openConnection host port mauth = do
  h <- connectTo host (PortNumber (fromInteger port))
  hPut h magicNumber
  let auth = B.fromChunks . return . BS.fromString $ fromMaybe "" mauth
  hPut h $ packUInt (fromIntegral $ B.length auth)
  hPut h auth
  res <- hGetNullTerminatedString h
  when (res /= "SUCCESS") $ throwIO (RethinkDBConnectionError res)
  r <- newIORef 1
  let db' = Database "test"
  wlock <- newMVar ()
  waits <- newIORef M.empty
  let rdb = RethinkDBHandle h wlock r db' waits undefined
  tid <- forkIO $ readResponses rdb
  return rdb{ rdbThread = tid }

hGetNullTerminatedString :: Handle -> IO ByteString
hGetNullTerminatedString h = go "" where
  go acc = do
    end <- hIsEOF h
    if end then return acc else do
      c <- B.hGet h 1
      if c == B.pack [0] then return acc else
        go (acc <> c)

magicNumber :: ByteString
magicNumber = packUInt $ fromEnum V0_1

-- | Convert a 4-byte byestring to an int
unpackUInt :: ByteString -> Int
unpackUInt s = case unpack s of
  [a,b,c,d] -> fromIntegral a .|.
               fromIntegral b `shiftL` 8 .|.
               fromIntegral c `shiftL` 16 .|.
               fromIntegral d `shiftL` 24
  _ -> error "unpackUInt: lengh is not 4"

-- | Convert an int to a 4-byte bytestring
packUInt :: Int -> B.ByteString
packUInt n = pack $ map fromIntegral $
               [n `mod` 256, (n `shiftR` 8) `mod` 256,
                (n `shiftR` 16) `mod` 256, (n `shiftR` 24) `mod` 256]

withHandle :: RethinkDBHandle -> (Handle -> IO a) -> IO a
withHandle RethinkDBHandle{ rdbHandle, rdbWriteLock } f =
  withMVar rdbWriteLock $ \_ -> f rdbHandle

data RethinkDBError = RethinkDBError {
  errorCode :: ErrorCode,
  errorTerm :: BaseTerm,
  errorMessage :: String,
  errorBacktrace :: Backtrace
  } deriving (Typeable, Show)

instance Exception RethinkDBError

-- | The raw response to a query
data Response = ErrorResponse {
  errorResponse :: RethinkDBError
  } | SuccessResponse {
  successCode :: SuccessCode,
  successDatums :: [O.Datum]
  }

data ErrorCode =
  ErrorBrokenClient |
  ErrorBadQuery |
  ErrorRuntime

instance Show ErrorCode where
  show ErrorBrokenClient = "broken client error"
  show ErrorBadQuery = "malformed query error"
  show ErrorRuntime = "runtime error"

data SuccessCode =
  SuccessPartial RethinkDBHandle Int64 |
  Success
  deriving Show

instance Show Response where
  show (ErrorResponse RethinkDBError {..}) =
    show errorCode ++ ": " ++
    show errorMessage ++ " (" ++
    show errorBacktrace ++ ")"
  show SuccessResponse {..} = show successCode ++ ": " ++ show successDatums

convertResponse :: RethinkDBHandle -> BaseTerm -> Int64 -> Response2 -> Response
convertResponse h q t Response2{ .. } = case type' of
  SUCCESS_ATOM -> SuccessResponse Success (map convertDatum r)
  SUCCESS_PARTIAL -> SuccessResponse (SuccessPartial h t) (map convertDatum r)
  SUCCESS_SEQUENCE -> SuccessResponse Success (map convertDatum r)
  CLIENT_ERROR -> ErrorResponse $ RethinkDBError ErrorBrokenClient q e bt
  COMPILE_ERROR -> ErrorResponse $ RethinkDBError ErrorBadQuery q e bt
  RUNTIME_ERROR -> ErrorResponse $ RethinkDBError ErrorRuntime q e bt
  where
    bt = maybe [] convertBacktrace backtrace
    r = toList response
    e = show response -- TODO: nice error with backtrace

runQLQuery :: RethinkDBHandle -> Query2 -> BaseTerm -> IO (MVar Response)
runQLQuery h query term = do
  tok <- newToken h
  mbox <- addMBox h tok term
  sendQLQuery h query{ Query.token = tok }
  return mbox

addMBox :: RethinkDBHandle -> Token -> BaseTerm -> IO (MVar Response)
addMBox h tok term = do
  chan <- newChan
  atomicModifyIORef' (rdbWait h) $ \mboxes -> 
    (M.insert tok (chan, term) mboxes, ())
  mbox <- newEmptyMVar
  forkIO $ fix $ \loop -> do
    response <- readChan chan
    putMVar mbox response
    when (not $ isLastResponse response) loop
  return mbox

sendQLQuery :: RethinkDBHandle -> Query2 -> IO ()
sendQLQuery h query = do
  let queryS = messagePut query
  withHandle h $ \s ->
    hPut s $ packUInt (fromIntegral $ B.length queryS) <> queryS

data RethinkDBReadError =
  RethinkDBReadError String
  deriving (Show, Typeable)
instance Exception RethinkDBReadError

readResponses :: RethinkDBHandle -> IO ()
readResponses h' = do
  tid <- myThreadId
  forever $ flip catches handlers $
    readSingleResponse h'{ rdbThread = tid }
  where
    handlers = []

readSingleResponse :: RethinkDBHandle -> IO ()
readSingleResponse h = do
  header <- hGet (rdbHandle h) 4
  rawResponse <- hGet (rdbHandle h) (unpackUInt header)
  let parsedResponse = messageGet rawResponse
  case parsedResponse of
    Left errMsg -> throwIO $ RethinkDBReadError errMsg
    Right (response, rest)
      | B.null rest -> dispatch (Response.token response) response
      | otherwise -> throwIO $ RethinkDBReadError "RethinkDB: readResponses: invalid reply length"

  where
  dispatch tok response = do
    mboxes <- readIORef $ rdbWait h
    case M.lookup tok mboxes of
      Nothing -> return ()
      Just (mbox, term) -> do
        let convertedResponse = convertResponse h term tok response
        writeChan mbox convertedResponse
        when (isLastResponse convertedResponse) $
          atomicModifyIORef' (rdbWait h) $ \m -> (M.delete tok m, ())

isLastResponse :: Response -> Bool
isLastResponse ErrorResponse{} = True
isLastResponse SuccessResponse{ successCode = Success } = True
isLastResponse SuccessResponse{ successCode = SuccessPartial{} } = False

-- | Set the default database
--
-- The new handle is an alias for the old one. Calling closeConnection on either one
-- will close both.
--
-- >>> let h' = use h (db "players")

use :: RethinkDBHandle -> Database -> RethinkDBHandle
use h db' = h { rdbDatabase = db' }

-- | Close an open connection
--
-- >>> closeConnection h

closeConnection :: RethinkDBHandle -> IO ()
closeConnection RethinkDBHandle{ rdbHandle } = hClose rdbHandle

convertDatum :: Datum.Datum -> O.Datum
convertDatum Datum { type' = Just R_NULL } = Null
convertDatum Datum { type' = Just R_BOOL, r_bool = Just b } = Bool b
convertDatum Datum { type' = Just R_ARRAY, r_array = a } = toJSON (map convertDatum $ toList a)
convertDatum Datum { type' = Just R_OBJECT, r_object = o } = object $ map pair $ toList o
    where pair (AssocPair (Just k) (Just v))  = (T.pack $ uToString $ k) .= convertDatum v
convertDatum Datum { type' = Just R_STR, r_str = Just s } = toJSON (uToString s)
convertDatum Datum { type' = Just R_NUM, r_num = Just n } = toJSON n
convertDatum d = error ("Invalid Datum: " ++ show d)

stopResponse :: Response -> IO ()
stopResponse SuccessResponse { successCode = SuccessPartial h tok } = do
  let query = defaultValue { Query.type' = STOP, Query.token = tok}
  sendQLQuery h query
stopResponse _ = return ()

nextResponse :: Response -> IO ()
nextResponse SuccessResponse { successCode = SuccessPartial h tok } = do
  let query = defaultValue { Query.type' = CONTINUE, Query.token = tok}
  sendQLQuery h query
nextResponse _ = return ()

makeCursor :: MVar Response -> IO (Cursor O.Datum)
makeCursor cursorMBox = do
  cursorBuffer <- newMVar (Right ([], False))
  return Cursor{..}
  where cursorMap = id

next :: Cursor a -> IO (Maybe a)
next c@Cursor{ .. } = modifyMVar cursorBuffer $ fix $ \loop mbuffer ->
  case mbuffer of
    Left err -> throwIO err
    Right ([], True) -> return (Right ([], True), Nothing)
    Right (x:xs, end) -> return $ (Right (xs, end), Just (cursorMap x))
    Right ([], False) -> cursorFetchBatch c >>= loop

cursorFetchBatch :: Cursor a -> IO (Either RethinkDBError ([O.Datum], Bool))
cursorFetchBatch c = do
  response <- readMVar (cursorMBox c)
  case response of
    ErrorResponse e -> return $ Left e
    SuccessResponse Success datums -> return $ Right (datums, True)
    SuccessResponse SuccessPartial{} datums -> return $ Right (datums, False)

cursorAll :: Cursor a -> IO [a]
cursorAll c = fmap reverse $ ($ []) $ fix $ \loop acc -> do
  ma <- next c
  return $ case ma of Nothing -> acc; Just a -> a : acc