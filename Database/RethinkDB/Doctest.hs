{-# LANGUAGE OverloadedStrings #-}

module Database.RethinkDB.Doctest (
  module Export,
  module Database.RethinkDB.Doctest
) where

-- default (Datum, ReQL, String, Int, Double)
-- import qualified Database.RethinkDB as R

import Database.RethinkDB.NoClash as Export
import Prelude as Export
import Data.Text as Export (Text)
import Data.Maybe as Export

import Control.Exception
import qualified Data.Vector as V
import Data.List (sort)

try' :: IO a -> IO ()
try' x = (try x `asTypeOf` return (Left (undefined :: SomeException))) >> return ()

doctestConnect :: IO RethinkDBHandle
doctestConnect = fmap (use "doctests") $ connect "localhost" 28015 def

sorted :: IO Datum -> IO Datum
sorted m = fmap s m where
  s (Array a) = Array $ fmap s $ V.fromList $ sort $ V.toList a
  s (Object o) = Object $ fmap s o
  s d = d
