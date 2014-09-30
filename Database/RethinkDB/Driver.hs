{-# LANGUAGE OverloadedStrings #-}

module Database.RethinkDB.Driver (
  run,
  run',
  Result(..),
  runOpts,
  RunFlag(..),
  WriteResponse(..),
  JSON(..)
  ) where

import Data.Aeson (Value(..), FromJSON(..), fromJSON, (.:), (.:?), (.=))
import Data.Aeson.Encode (encodeToTextBuilder)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Aeson (Result(Error, Success))
import Control.Monad
import Control.Concurrent.MVar (MVar, takeMVar)
import Data.Text (Text)
import Control.Applicative ((<$>), (<*>))
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.HashMap.Strict as HM

import Database.RethinkDB.Network
import Database.RethinkDB.ReQL hiding (Object)

-- | Per-query settings
data RunFlag =
  UseOutdated |
  NoReply |
  Durability Durability |
  Profile |
  ArrayLimit Int

data Durability = Hard | Soft

renderOption :: RunFlag -> (Text, Value)
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
run :: (Expr query, Result r) => RethinkDBHandle -> query -> IO r
run h = runOpts h []

-- | Run a given query and return a JSON
run' :: Expr query => RethinkDBHandle -> query -> IO [JSON]
run' h t = do
  c <- run h t
  collect c

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
  -- TODO: just "vals" in 1.12
  writeResponseOldVal :: Maybe Value,
  writeResponseNewVal :: Maybe Value
  }

instance FromJSON WriteResponse where
  parseJSON (Object o) =
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
              nothing "old_val" writeResponseOldVal,
              nothing "new_val" writeResponseNewVal ]) ++
            "}"
    where
      go k v = Just $ k ++ ":" ++ show v
      zero k f = if f wr == 0 then Nothing else go k (f wr)
      nothing k f = maybe Nothing (go k) (f wr)

data JSON = JSON Value deriving Eq

instance Show JSON where
  show (JSON a) = unpack . toLazyText . encodeToTextBuilder $ a

instance FromJSON JSON where
  parseJSON = fmap JSON . parseJSON

instance Ord JSON where
  compare (JSON x) (JSON y) = x <=> y
    where
      Object a <=> Object b =
        compare (HM.keys a) (HM.keys b) <>
        mconcat (map (\k -> (a HM.! k) <=> (b HM.! k) ) (HM.keys a))
      Object _ <=> _ = LT
      _ <=> Object _ = GT
      Array a <=> Array b = compare (fmap JSON a) (fmap JSON b)
      Array _ <=> _ = LT
      _ <=> Array _ = GT
      String a <=> String b = compare a b
      String _ <=> _ = LT
      _ <=> String _ = GT
      Number a <=> Number b = compare a b
      Number _ <=> _ = LT
      _ <=> Number _ = GT
      Bool a <=> Bool b = compare a b
      Bool _ <=> _ = LT
      _ <=> Bool _ = GT
      Null <=> Null = EQ

-- TODO: profile