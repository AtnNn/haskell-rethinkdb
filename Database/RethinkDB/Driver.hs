{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Database.RethinkDB.Driver (
  run,
  run',
  Result(..),
  runOpts,
  RunFlag(..),
  WriteResponse(..),
  Change(..),
  getSingle
  ) where

import qualified Data.Aeson as J
import Control.Monad
import Control.Concurrent.MVar (MVar, takeMVar)
import Data.Text (Text)
import Control.Applicative ((<$>), (<*>))
import Data.List
import Data.Maybe
import Control.Exception (throwIO)
import Data.Map (Map)

import Database.RethinkDB.Datum hiding (Result)
import Database.RethinkDB.Network
import Database.RethinkDB.ReQL

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
run :: (Expr query, Result r) => RethinkDBHandle -> query -> IO r
run h = runOpts h []

-- | Run a given query and return a Datum
run' :: Expr query => RethinkDBHandle -> query -> IO Datum
run' = run

-- | Convert the raw query response into useful values
class Result r where
  convertResult :: MVar Response -> IO r

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

instance FromDatum a => Result (Maybe a) where
  convertResult v = do
    r <- takeMVar v
    case r of
      ResponseSingle Null -> return Nothing
      ResponseSingle a -> fmap Just $ unsafeFromDatum a
      ResponseError e -> throwIO e
      ResponseBatch Nothing batch -> fmap Just $ unsafeFromDatum $ toDatum batch
      ResponseBatch (Just _more) batch -> do
        rest <- collect' =<< convertResult v
        fmap Just $ unsafeFromDatum $ toDatum $ batch ++ rest

instance Result Int where
  convertResult = unsafeFromDatum <=< getSingle

instance Result Double where
  convertResult = unsafeFromDatum <=< getSingle

instance Result Bool where
  convertResult = unsafeFromDatum <=< getSingle

instance Result String where
  convertResult = unsafeFromDatum <=< getSingle

instance Result Text where
  convertResult = unsafeFromDatum <=< getSingle

instance FromDatum a => Result (Map String a) where
  convertResult = unsafeFromDatum <=< getSingle


instance Result () where
  convertResult _ = return ()

instance Result J.Value where
  convertResult = unsafeFromDatum <=< getSingle

getSingle :: MVar Response -> IO Datum
getSingle v = do
    r <- takeMVar v
    case r of
      ResponseSingle datum -> return datum
      ResponseError e -> throwIO e
      ResponseBatch Nothing [datum] -> return datum
      ResponseBatch _ batch ->
        throwIO $ RethinkDBError ErrorUnexpectedResponse (Datum Null)
        ("Expected a single datum but got: " ++ show batch) []

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

instance Result WriteResponse where
  convertResult = unsafeFromDatum <=< convertResult

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
      go k v = Just $ k ++ ":" ++ show v
      zero k f = if f wr == 0 then Nothing else go k (f wr)
      nothing k f = maybe Nothing (go k) (f wr)

-- TODO: profile
