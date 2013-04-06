{-# LANGUAGE RecordWildCards, OverloadedStrings, DeriveDataTypeable #-}

module Database.RethinkDB.Network where

import Control.Monad
import Data.Typeable
import Network
import System.IO (Handle, hClose)
import Data.ByteString.Lazy (pack, unpack, hPut, hGet)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Data.Int
import Control.Concurrent.MVar
import Control.Concurrent
import Data.Bits
import Data.Monoid
import Data.Foldable hiding (forM_)
import Control.Exception.Base
import Data.Aeson hiding (Success)

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers

import Database.RethinkDB.Protobuf.Ql2.Query2 as Query
import Database.RethinkDB.Protobuf.Ql2.Query2.QueryType
import Database.RethinkDB.Protobuf.Ql2.Response2 as Response
import Database.RethinkDB.Protobuf.Ql2.Response2.ResponseType
import Database.RethinkDB.Protobuf.Ql2.Datum as Datum
import Database.RethinkDB.Protobuf.Ql2.Datum.DatumType
import Database.RethinkDB.Protobuf.Ql2.Datum.AssocPair
import Database.RethinkDB.Protobuf.Ql2.VersionDummy.Version

import Database.RethinkDB.Objects as O
import Database.RethinkDB.Term

-- | A connection to the database server
data RethinkDBHandle = RethinkDBHandle {
  rdbHandle :: Handle,
  rdbToken :: MVar Int64, -- ^ The next token to use
  rdbDatabase :: Database  -- ^ The default database
  }

instance Show RethinkDBHandle where
  show (RethinkDBHandle h _ _) = "RethinkDB Connection " ++ show h

withNewToken :: RethinkDBHandle -> (Int64 -> IO a) -> IO a
withNewToken (RethinkDBHandle _ v _) io =
    mask $ \restore -> do
      t <- takeMVar v
      let t' = t + 1
      a <- restore (io t') `onException` putMVar v t'
      putMVar v t'
      return a

withConnection :: RethinkDBHandle -> IO a -> IO a
withConnection (RethinkDBHandle _ v _) io = withMVar v (\_ -> io)

-- | Create a new connection to the database server
--
-- /Example:/ connect using the default port
--
-- >>> h <- openConnection "localhost" 28015

openConnection :: HostName -> Integer -> IO RethinkDBHandle
openConnection host port = do
  h <- connectTo host (PortNumber (fromInteger port))
  hPut h magicNumber
  r <- newMVar 1
  let db' = Database "test"
  return (RethinkDBHandle h r db')

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

data ErrorCode = ErrorBrokenClient
               | ErrorBadQuery
               | ErrorRuntime
               | ErrorNetwork

instance Show ErrorCode where
  show ErrorBrokenClient = "broken client error"
  show ErrorBadQuery = "malformed query error"
  show ErrorRuntime = "runtime error"
  show ErrorNetwork = "error talking to server"

data SuccessCode = SuccessPartial RethinkDBHandle BaseTerm Int64
                 | Success
                 deriving Show

instance Show Response where
  show (ErrorResponse RethinkDBError {..}) = show errorCode ++ ": " ++
                            show errorMessage ++ " (" ++
                            show errorBacktrace ++ ")"
  show SuccessResponse {..} = show successCode ++ ": " ++ show successDatums

-- | Receive a fixed amount of data
recvAll :: RethinkDBHandle -> Int -> IO ByteString
recvAll (RethinkDBHandle h _ _) n = hGet h n

-- | Send a bytestring
sendAll :: RethinkDBHandle -> ByteString -> IO ()
sendAll (RethinkDBHandle h _ _) s = hPut h s

convertResponse :: RethinkDBHandle -> BaseTerm -> Int64 -> Either String Response2 -> Response
convertResponse _ q _ (Left s) = ErrorResponse (RethinkDBError ErrorNetwork q s [])
convertResponse h q t (Right Response2 {..}) = case type' of
  SUCCESS_ATOM     -> SuccessResponse Success                (map convertDatum r)
  SUCCESS_PARTIAL  -> SuccessResponse (SuccessPartial h q t) (map convertDatum r)
  SUCCESS_SEQUENCE -> SuccessResponse Success                (map convertDatum r)
  CLIENT_ERROR     -> ErrorResponse $ RethinkDBError ErrorBrokenClient q e bt
  COMPILE_ERROR    -> ErrorResponse $ RethinkDBError ErrorBadQuery     q e bt
  RUNTIME_ERROR    -> ErrorResponse $ RethinkDBError ErrorRuntime      q e bt
  where bt = maybe [] convertBacktrace backtrace
        r = toList response
        e = show response -- TODO: nice error with backtrace

runQLQuery :: RethinkDBHandle -> Query2 -> BaseTerm -> IO Response
runQLQuery h query term = do
  let queryS = messagePut query
  sendAll h $ packUInt (fromIntegral $ B.length queryS) <> queryS
  fmap (convertResponse h term (Query.token query)) $ readResponse (Query.token query)

  where readResponse t = do
          header <- recvAll h 4
          responseS <- recvAll h (unpackUInt header)
          let eResponse = messageGet responseS
          case eResponse of
            Left errMsg -> return $ Left errMsg
            Right (response, rest)
              | B.null rest ->
                (case Response.token response of
                  n | n == t -> return $ Right response
                    | n > t -> return $ Left "RethinkDB: runQLQuery: invalid response token"
                    | otherwise -> readResponse t)
              | otherwise -> return $ Left "RethinkDB: runQLQuery: invalid reply length"

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
closeConnection (RethinkDBHandle h _ _) = hClose h

convertDatum :: Datum.Datum -> O.Datum
convertDatum Datum { type' = R_NULL } = Null
convertDatum Datum { type' = R_BOOL, r_bool = Just b } = Bool b
convertDatum Datum { type' = R_ARRAY, r_array = a } = toJSON (map convertDatum $ toList a)
convertDatum Datum { type' = R_OBJECT, r_object = o } = object $ map pair $ toList o
    where pair (AssocPair k v)  = (T.pack $ uToString k) .= convertDatum v
convertDatum Datum { type' = R_STR, r_str = Just s } = toJSON (uToString s)
convertDatum Datum { type' = R_NUM, r_num = Just n } = toJSON n
convertDatum d = error ("Invalid Datum: " ++ show d)

data Cursor a = Cursor {
  cursorStop :: IO (),
  cursorNext :: IO (Either (Maybe RethinkDBError) a) }

instance Functor Cursor where
  fmap f (Cursor s c) = Cursor s $ fmap (fmap f) c

stopResponse :: Response -> IO ()
stopResponse SuccessResponse { successCode = SuccessPartial h _ tok } = do
  let queryS = messagePut defaultValue { Query.type' = STOP, Query.token = tok}
  sendAll h $ packUInt (fromIntegral $ B.length queryS) <> queryS
stopResponse _ = return ()

nextResponse :: Response -> IO (Maybe Response)
nextResponse SuccessResponse { successCode = SuccessPartial h bt tok } = do
  let query = defaultValue { Query.type' = CONTINUE, Query.token = tok}
  r <- runQLQuery h query bt
  return $ Just r
nextResponse _ = return Nothing

makeCursor :: Response -> IO (Cursor O.Datum)
makeCursor resp = do
  v <- newEmptyMVar
  _ <- forkFinally (go v resp) (const $ stopResponse resp)
  return $ Cursor (stopResponse resp) (takeMVar v)
  where go v r@SuccessResponse {successDatums = datums} = do
          forM_ datums $ \d -> putMVar v (Right d)
          nr <- nextResponse r
          maybe (forever $ putMVar v (Left Nothing)) (go v) nr
        go v ErrorResponse { errorResponse = e } =
          forever $ putMVar v (Left (Just e))

cursorAll :: Cursor a -> IO [a]
cursorAll (Cursor _ next) = go []
  where go acc = do
          x <- next
          case x of
            Left Nothing -> return (reverse acc)
            Left (Just err) -> throw err
            Right a -> go (a : acc)

cursorTake :: Int -> Cursor a -> IO [a]
cursorTake n (Cursor _ next) = go n []
  where go 0 acc = return (reverse acc)
        go i acc = do
          x <- next
          case x of
            Left Nothing -> return (reverse acc)
            Left (Just err) -> throw err
            Right a -> go (i - 1) (a : acc)

cursorHead :: Cursor a -> IO a
cursorHead = fmap head . cursorTake 1
