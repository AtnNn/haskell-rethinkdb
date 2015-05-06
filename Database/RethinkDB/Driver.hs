{-# LANGUAGE OverloadedStrings, FlexibleInstances, DefaultSignatures, GADTs, CPP #-}

module Database.RethinkDB.Driver (
  run,
  run',
  Result(..),
  runOpts,
  RunFlag(..),
  WriteResponse(..),
  Change(..)
  ) where

import qualified Data.Aeson as J
import Control.Monad
import Control.Concurrent.MVar (MVar, takeMVar)
import Data.Text (Text)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif
import Data.List
import Data.Maybe
import Control.Exception (throwIO)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time
import qualified Data.Text as ST
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Int
import Data.Word
import qualified Data.HashMap.Strict as HM
import Data.Ratio
import qualified Data.Vector as V

import Database.RethinkDB.Datum hiding (Result)
import Database.RethinkDB.Network
import Database.RethinkDB.ReQL

-- $setup
--
-- Get the doctests ready
--
-- >>> :load Database.RethinkDB.NoClash
-- >>> import qualified Database.RethinkDB as R
-- >>> :set -XOverloadedStrings
-- >>> h <- fmap (use "doctests") $ connect "localhost" 28015 def


-- | Per-query settings
data RunFlag =
  UseOutdated |
  NoReply |
  Durability Durability |
  Profile |
  ArrayLimit Int

data Durability = Hard | Soft

renderOption :: RunFlag -> (Text, Datum)
renderOption UseOutdated = "user_outdated" .= True
renderOption NoReply = "noreply" .= True
renderOption (Durability Soft) = "durability" .= ("soft" :: String)
renderOption (Durability Hard) = "durability" .= ("hard" :: String)
renderOption Profile = "profile" .= True
renderOption (ArrayLimit n) = "array_limit" .= n

-- | Run a query with the given options
runOpts :: (Expr query, Result r) => RethinkDBHandle -> [RunFlag] -> query -> IO r
runOpts h opts t = do
  let (q, bt) = buildQuery (expr t) 0 (rdbDatabase h) (map renderOption opts)
  r <- runQLQuery h q bt
  convertResult r

-- | Run a given query and return a Result
--
-- >>> run h $ num 1 :: IO Int
-- 1
--
-- >>> run h $ str "foo" :: IO (Either RethinkDBError Int)
-- Left RethinkDB: Unexpected response: "when expecting a Int, encountered String instead"
--
-- >>> run h $ str "foo" :: IO (Maybe Int)
-- Nothing
--
-- >>> run h $ str "foo" :: IO Int
-- *** Exception: RethinkDB: Unexpected response: "when expecting a Int, encountered String instead"
--
-- >>> c <- run h $ table "users" # orderBy [asc "name"] # (!"name"):: IO (Cursor Datum)
-- >>> next c
-- Just "bill"
-- >>> collect c
-- ["nancy","sabrina"]
run :: (Expr query, Result r) => RethinkDBHandle -> query -> IO r
run h = runOpts h []

-- | Run a given query and return a Datum
run' :: Expr query => RethinkDBHandle -> query -> IO Datum
run' = run

-- | Convert the raw query response into useful values
class Result r where
  convertResult :: MVar Response -> IO r
  default convertResult :: FromDatum r => MVar Response -> IO r
  convertResult = unsafeFromDatum <=< convertResult

instance Result Response where
  convertResult = takeMVar

instance FromDatum a => Result (Cursor a) where
  convertResult r = do
    c <- makeCursor r 
    return c { cursorMap = unsafeFromDatum }

unsafeFromDatum :: FromDatum a => Datum -> IO a
unsafeFromDatum val = case fromDatum val of
  Error e -> throwIO (RethinkDBError ErrorUnexpectedResponse (Datum Null) e [])
  Success a -> return a

instance FromDatum a => Result [a] where
  convertResult = collect <=< convertResult

instance (FromDatum b, a ~ RethinkDBError) => Result (Either a b) where
  convertResult v = do
    r <- takeMVar v
    ed <- case r of
               ResponseSingle Null -> return $ Right Null
               ResponseSingle b -> return $ Right b
               ResponseError a -> return $ Left a
               ResponseBatch Nothing batch -> return $ Right $ toDatum batch
               ResponseBatch (Just _more) batch -> do
                      rest <- collect' =<< convertResult v
                      return $ Right $ toDatum $ batch ++ rest
    case ed of
      Left a -> return $ Left a
      Right d -> case fromDatum d of
        Success b -> return $ Right b
        Error a -> return $ Left $ RethinkDBError ErrorUnexpectedResponse (Datum Null) a []

instance FromDatum a => Result (Maybe a) where
  convertResult v = do 
      ed <- convertResult v
      case ed of
        Left _ -> return Nothing
        Right Null -> return Nothing
        Right d -> case fromDatum d of
                     Success a -> return $ Just a
                     Error _ -> return $ Nothing

instance Result Int
instance Result Double
instance Result Bool

instance Result () where
  convertResult m = do
    _ <- takeMVar m
    return ()

instance Result J.Value
instance Result Char
instance Result Float
instance Result Int8
instance Result Int16
instance Result Int32
instance Result Int64
instance Result Word
instance Result Word8
instance Result Word16
instance Result Word32
instance Result Word64
instance Result Integer
instance Result LB.ByteString
instance Result SB.ByteString
instance Result LT.Text
instance Result ST.Text
instance Result ZonedTime
instance Result UTCTime
instance (Ord a, FromDatum a) => Result (Set.Set a)
instance FromDatum a => Result (V.Vector a)
instance FromDatum a => Result (HM.HashMap [Char] a)
instance FromDatum a => Result (HM.HashMap ST.Text a)
instance FromDatum a => Result (Map.Map [Char] a)
instance FromDatum a => Result (Map.Map ST.Text a)
instance Result (Ratio Integer)
instance Result LonLat

nextFail :: FromDatum a => Cursor Datum -> IO a
nextFail c = do
  x <- next c
  case x of
    Nothing -> throwIO $ RethinkDBError ErrorUnexpectedResponse (Datum Null) "Not enough data" []
    Just a -> case fromDatum a of
      Success b -> return b
      Error e -> throwIO $ RethinkDBError ErrorUnexpectedResponse (Datum Null) e []

assertEnd :: Cursor a -> IO ()
assertEnd c = do
  x <- next c
  case x of
    Nothing -> return ()
    Just _ -> throwIO $ RethinkDBError ErrorUnexpectedResponse (Datum Null) "Too much data" []

instance (FromDatum a, FromDatum b) => Result (a, b) where
  convertResult r = do
    c <- convertResult r
    a <- nextFail c
    b <- nextFail c
    assertEnd c
    return (a, b)

instance (FromDatum a, FromDatum b, FromDatum c) => Result (a, b, c) where
  convertResult r = do
    c <- convertResult r
    a <- nextFail c
    b <- nextFail c
    c_ <- nextFail c
    assertEnd c
    return (a, b, c_)

instance (FromDatum a, FromDatum b, FromDatum c, FromDatum d) => Result (a, b, c, d) where
  convertResult r = do
    c <- convertResult r
    a <- nextFail c
    b <- nextFail c
    c_ <- nextFail c
    d <- nextFail c
    assertEnd c
    return (a, b, c_, d)

instance (FromDatum a, FromDatum b, FromDatum c, FromDatum d, FromDatum e) => Result (a, b, c, d, e) where
  convertResult r = do
    c <- convertResult r
    a <- nextFail c
    b <- nextFail c
    c_ <- nextFail c
    d <- nextFail c
    e <- nextFail c
    assertEnd c
    return (a, b, c_, d, e)

instance Result Datum where
  convertResult v = do
    r <- takeMVar v
    case r of
      ResponseSingle datum -> return datum
      ResponseError e -> throwIO e
      ResponseBatch Nothing batch -> return $ toDatum batch
      ResponseBatch (Just _more) batch -> do
        rest <- collect' =<< convertResult v
        return . toDatum $ batch ++ rest

instance Result WriteResponse

data WriteResponse = WriteResponse {
  writeResponseInserted :: Int,
  writeResponseDeleted :: Int,
  writeResponseReplaced :: Int,
  writeResponseUnchanged :: Int,
  writeResponseSkipped :: Int,
  writeResponseErrors :: Int,
  writeResponseFirstError :: Maybe Text,
  writeResponseGeneratedKeys :: Maybe [Text],
  writeResponseChanges :: Maybe [Change]
  }

data Change = Change { oldVal, newVal :: Datum }

instance Show Change where
  show (Change old new) = "{\"old_val\":" ++ show old ++ ",\"new_val\":" ++ show new ++ "}"

instance FromDatum Change where
  parseDatum (Object o) =
    Change <$> o .: "old_val" <*> o .: "new_val"
  parseDatum _ = mzero

instance FromDatum WriteResponse where
  parseDatum (Object o) =
    WriteResponse
    <$> o .: "inserted"
    <*> o .: "deleted"
    <*> o .: "replaced"
    <*> o .: "unchanged"
    <*> o .: "skipped"
    <*> o .: "errors"
    <*> o .:? "first_error"
    <*> o .:? "generated_keys"
    <*> o .:? "changes"
  parseDatum _ = mzero

instance Show WriteResponse where
  show wr = "{" ++
            intercalate "," (catMaybes [
              zero "inserted" writeResponseInserted,
              zero "deleted" writeResponseDeleted,
              zero "replaced" writeResponseReplaced,
              zero "unchanged" writeResponseUnchanged,
              zero "skipped" writeResponseSkipped,
              zero "errors" writeResponseErrors,
              nothing "first_error" writeResponseFirstError,
              nothing "generated_keys" writeResponseGeneratedKeys,
              nothing "changes" writeResponseChanges ]) ++
            "}"
    where
      go :: Show a => String -> a -> Maybe String
      go k v = Just $ k ++ ":" ++ show v
      zero k f = if f wr == 0 then Nothing else go k (f wr)
      nothing :: Show a => String -> (WriteResponse -> Maybe a) -> Maybe String
      nothing k f = maybe Nothing (go k) (f wr)

-- TODO: profile
