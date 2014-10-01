{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Database.RethinkDB.Driver (
  run,
  run',
  Result(..),
  runOpts,
  RunFlag(..),
  WriteResponse(..),
  JSON(..),
  Change(..),
  getSingle
  ) where

import Data.Aeson (Value(..), FromJSON(..), fromJSON, (.:), (.:?), (.=), toJSON)
import qualified Data.Aeson (Result(Error, Success))
import Control.Monad
import Control.Concurrent.MVar (MVar, takeMVar)
import Data.Text (Text)
import Control.Applicative ((<$>), (<*>))
import Data.List
import Data.Maybe
import Control.Exception (throw, throwIO)
import Data.Map (Map)

import Database.RethinkDB.Network
import Database.RethinkDB.Objects
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
renderOption NoReply = "noreply" .= True -- TODO: handle no-reply in Network.hs
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
run' :: Expr query => RethinkDBHandle -> query -> IO JSON
run' h t = run h t

-- | Convert the raw query response into useful values
class Result r where
  convertResult :: MVar Response -> IO r

instance Result Response where
  convertResult = takeMVar

instance FromJSON a => Result (Cursor a) where
  convertResult r = fmap (fmap unsafeFromJSON) $ makeCursor r

unsafeFromJSON :: FromJSON a => Data.Aeson.Value -> a
unsafeFromJSON val = case fromJSON val of
  Data.Aeson.Error e -> throw (RethinkDBError ErrorUnexpectedResponse (Datum Null) e [])
  Data.Aeson.Success a -> a

instance FromJSON a => Result [a] where
  convertResult = collect <=< convertResult

instance FromJSON a => Result (Maybe a) where
  convertResult v = do
    r <- takeMVar v
    case r of
      ResponseSingle Data.Aeson.Null -> return Nothing
      ResponseSingle a -> return $ Just $ unsafeFromJSON a
      ResponseError e -> throwIO e
      ResponseBatch Nothing batch -> return . Just . unsafeFromJSON $ toJSON batch
      ResponseBatch (Just _more) batch -> do
        rest <- collect' =<< convertResult v
        return . Just . unsafeFromJSON . toJSON $ batch ++ rest

instance Result Int where
  convertResult = fmap unsafeFromJSON . getSingle

instance Result Double where
  convertResult = fmap unsafeFromJSON . getSingle

instance Result Bool where
  convertResult = fmap unsafeFromJSON . getSingle

instance FromJSON a => Result (Map String a) where
  convertResult = fmap unsafeFromJSON . getSingle

instance Result String where
  convertResult = fmap unsafeFromJSON . getSingle

instance Result Text where
  convertResult = fmap unsafeFromJSON . getSingle

getSingle :: MVar Response -> IO Value
getSingle v = do
    r <- takeMVar v
    case r of
      ResponseSingle datum -> return datum
      ResponseError e -> throwIO e
      ResponseBatch Nothing [datum] -> return datum
      ResponseBatch _ batch ->
        throwIO $ RethinkDBError ErrorUnexpectedResponse (Datum Null)
        ("Expected a single datum but got: " ++ show batch) []

instance Result Value where
  convertResult v = do
    r <- takeMVar v
    case r of
      ResponseSingle datum -> return datum
      ResponseError e -> throwIO e
      ResponseBatch Nothing batch -> return $ toJSON batch
      ResponseBatch (Just _more) batch -> do
        rest <- collect' =<< convertResult v
        return . toJSON $ batch ++ rest

instance Result JSON where
  convertResult = fmap JSON . convertResult

instance Result WriteResponse where
  convertResult = fmap unsafeFromJSON . convertResult

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

data Change = Change { newVal, oldVal :: Value }
            deriving Show

instance FromJSON Change where
  parseJSON (Object o) =
    Change <$> o .: "new_val" <*> o .: "old_val"
  parseJSON _ = mzero

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
    <*> o .:? "changes"
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
              nothing "changes" writeResponseChanges ]) ++
            "}"
    where
      go k v = Just $ k ++ ":" ++ show v
      zero k f = if f wr == 0 then Nothing else go k (f wr)
      nothing k f = maybe Nothing (go k) (f wr)

-- TODO: profile