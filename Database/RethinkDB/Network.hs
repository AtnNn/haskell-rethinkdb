{-# LANGUAGE RecordWildCards #-}

module Database.RethinkDB.Network where

import Network
import System.IO (Handle, hClose)
import Data.ByteString.Lazy (pack, unpack, hPut, hGet, ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Int
import Data.IORef
import Data.Bits
import Data.List
import Data.Monoid
import Data.Sequence
import Data.Foldable
import Data.Word

import Text.ProtocolBuffers

import Database.RethinkDB.Protobuf.Ql2.Query2 as Query
import Database.RethinkDB.Protobuf.Ql2.Query2.QueryType
import Database.RethinkDB.Protobuf.Ql2.Response2 as Response
import Database.RethinkDB.Protobuf.Ql2.Response2.ResponseType
import Database.RethinkDB.Protobuf.Ql2.Backtrace
import Database.RethinkDB.Protobuf.Ql2.Datum
import Database.RethinkDB.Protobuf.Ql2.VersionDummy.Version

import Database.RethinkDB.Crud

-- | A connection to the database server
data RethinkDBHandle = RethinkDBHandle {
  rdbHandle :: Handle,
  rdbToken :: IORef Int64, -- ^ The next token to use
  rdbDatabase :: Database  -- ^ The default database
  }

-- | Create a new connection to the database server
--
-- /Example:/ connect using the default port
--
-- >>> h <- openConnection "localhost" 28015

openConnection :: HostName -> Integer -> IO RethinkDBHandle
openConnection host port = do
  h <- connectTo host (PortNumber (fromInteger port))
  hPut h magicNumber
  r <- newIORef 1
  let db' = Database "test"
  return (RethinkDBHandle h r db')

magicNumber = packUInt $ fromEnum V0_1

-- | Convert a 4-bte byestring to an int
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

-- | The raw response to a query
data Response = ErrorResponse {
  errorCode :: ErrorCode,
  errorMessage :: String,
  errorBacktrace :: Maybe Backtrace
  } | SuccessResponse {
  successCode :: SuccessCode,
  successDatums :: [Datum]
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

data SuccessCode = SuccessEmpty
                 | SuccessJson
                 | SuccessPartial
                 | SuccessStream
                 deriving Show

instance Show Response where
  show ErrorResponse {..} = show errorCode ++ ": " ++
                            show errorMessage ++ " (" ++
                            show errorBacktrace ++ ")"
  show SuccessResponse {..} = show successCode ++ ": " ++ show successDatums

-- | Receive a fixed amoutn of data
recvAll :: RethinkDBHandle -> Int -> IO ByteString
recvAll (RethinkDBHandle h _ _) n = hGet h n

-- | Send a bytestring
sendAll :: RethinkDBHandle -> ByteString -> IO ()
sendAll (RethinkDBHandle h _ _) s = hPut h s

convertResponse :: Either String Response2 -> Response
convertResponse (Left s) = ErrorResponse ErrorNetwork s defaultValue
convertResponse (Right Response2 {..}) = case type' of
  SUCCESS_ATOM     -> SuccessResponse SuccessJson    r
  SUCCESS_PARTIAL  -> SuccessResponse SuccessPartial r
  SUCCESS_SEQUENCE -> SuccessResponse SuccessStream  r
  CLIENT_ERROR     -> ErrorResponse ErrorBrokenClient e bt
  COMPILE_ERROR    -> ErrorResponse ErrorBadQuery     e bt
  RUNTIME_ERROR    -> ErrorResponse ErrorRuntime      e bt
  where bt = backtrace
        r = toList response
        e = show response -- TODO: nice error with backtrace

runQLQuery :: RethinkDBHandle -> Query2 -> IO Response
runQLQuery h query = do
  let queryS = messagePut query
  sendAll h $ packUInt (fromIntegral $ B.length queryS) <> queryS
  fmap convertResponse $ readResponse (Query.token query)

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
