module Database.RethinkDB.Driver (run, run', Result) where

import Data.Aeson (Value, FromJSON, fromJSON)
import qualified Data.Aeson (Result(Error, Success))
import Control.Monad
import Data.Default (Default(def))
import Control.Concurrent.MVar (MVar, takeMVar)
import Data.Int (Int64)
import Control.Monad.Fix (fix)
import System.IO.Unsafe (unsafeInterleaveIO)

import Database.RethinkDB.Network
import Database.RethinkDB.Term

run :: Result r => RethinkDBHandle -> Term -> IO r
run h t = do
  let (q, bt) = buildQuery t 0 (rdbDatabase h)
  r <- runQLQuery h q bt
  convertResult r

run' :: RethinkDBHandle -> Term -> IO [Value]
run' h t = do
  c <- run h t
  fix $ \loop -> do
    n <- next c
    case n of
      Nothing -> return []
      Just x -> do
        xs <- unsafeInterleaveIO $ loop
        return $ x : xs

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