{-# LANGUAGE OverloadedStrings #-}

module Database.RethinkDB.Driver (
  run,
  run',
  Result(..),
  runOpts,
  RunOptions(..),
  WriteResponse(..),
  JSON(..)
  ) where

import Data.Aeson (Value(..), FromJSON(..), fromJSON, (.:), (.:?), encode)
import Data.Aeson.Encode (fromValue)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Aeson (Result(Error, Success))
import Control.Monad
import Control.Concurrent.MVar (MVar, takeMVar)
import Data.Sequence ((|>))
import Data.Text (Text)
import Control.Applicative ((<$>), (<*>))

import Database.RethinkDB.Protobuf.Ql2.Query (Query(..))
import Database.RethinkDB.Protobuf.Ql2.Query.AssocPair (AssocPair(..))
import Database.RethinkDB.Protobuf.Ql2.Term as Term (Term(..))
import Database.RethinkDB.Protobuf.Ql2.Term.TermType (TermType(DATUM))
import Database.RethinkDB.Protobuf.Ql2.Datum as Datum
import Database.RethinkDB.Protobuf.Ql2.Datum.DatumType
import Text.ProtocolBuffers.Basic (uFromString, defaultValue)

import Database.RethinkDB.Network
import Database.RethinkDB.ReQL

-- | Per-query settings
data RunOptions =
  UseOutdated |
  NoReply |
  SoftDurability Bool

applyOption :: RunOptions -> Query -> Query
applyOption UseOutdated q = addQueryOption q "user_outdated" True
applyOption NoReply q = addQueryOption q "noreply" True
applyOption (SoftDurability b) q = addQueryOption q "soft_durability" b

addQueryOption :: Query -> String -> Bool -> Query
addQueryOption q k v = q {
  global_optargs = global_optargs q |> AssocPair (Just $ uFromString k) (Just boolTrue) }
  where
    boolTrue = defaultValue{
      Term.type' = Just DATUM, datum = Just defaultValue{
         Datum.type' = Just R_BOOL, r_bool = Just v } }

-- | Run a query with the given options
runOpts :: (Expr query, Result r) => RethinkDBHandle -> [RunOptions] -> query -> IO r
runOpts h opts t = do
  let (q, bt) = buildQuery (expr t) 0 (rdbDatabase h)
  let q' = foldr (fmap . applyOption) id opts q
  r <- runQLQuery h q' bt
  convertResult r

-- | Run a given query and return a Result
run :: (Expr query, Result r) => RethinkDBHandle -> query -> IO r
run h = runOpts h []

-- | Run a given query and return a JSON
run' :: Expr query => RethinkDBHandle -> query -> IO [Value]
run' h t = do
  c <- run h t
  liftM (map unwrapValue) (collect c)
  where
    unwrapValue (JSON val) = val

-- | Convert the raw query response into useful values
class Result r where
  convertResult :: MVar Response -> IO r

instance Result Response where
    convertResult = takeMVar

instance FromJSON a => Result (Cursor a) where
  convertResult r = fmap (fmap $ unsafe . fromJSON) $ makeCursor r
    where
      unsafe (Data.Aeson.Error e) = error e
      unsafe (Data.Aeson.Success a) = a

instance FromJSON a => Result [a] where
  convertResult = collect <=< convertResult

instance FromJSON a => Result (Maybe a) where
  convertResult r = do
    c <- convertResult r
    car <- next c
    case car of
      Nothing -> return Nothing
      Just a -> do
        cadr <- next c
        case cadr of
          Nothing -> return $ Just a
          Just _ -> return Nothing

data WriteResponse = WriteResponse {
  writeResponseInserted :: Int,
  writeResponseDeleted :: Int,
  writeResponseReplaced :: Int,
  writeResponseUnchanged :: Int,
  writeResponseSkipped :: Int,
  writeResponseErrors :: Int,
  writeResponseFirstError :: Maybe Text,
  writeResponseGeneratedKeys :: Maybe [Text],
  writeResponseOldVal :: Maybe Value,
  writeResponseNewVal :: Maybe Value
  } deriving Show

instance FromJSON WriteResponse where
  parseJSON (Data.Aeson.Object o) =
    WriteResponse
    <$> o .: "inserted"
    <*> o .: "deleted"
    <*> o .: "replaced"
    <*> o .: "unchanged"
    <*> o .: "skipped"
    <*> o .: "errors"
    <*> o .:? "first_error"
    <*> o .:? "generated_keys"
    <*> o .:? "old_val"
    <*> o .:? "new_val"
  parseJSON _ = mzero

data JSON = JSON Value

instance Show JSON where
  show (JSON a) = unpack . toLazyText . fromValue $ a

instance FromJSON JSON where
  parseJSON = fmap JSON . parseJSON
