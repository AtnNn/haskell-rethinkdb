-- | This module exports all of Database.RethinkDB except for the
-- names that clash with Prelude or Data.Time

module Database.RethinkDB.NoClash (
  module Database.RethinkDB,
  -- module Prelude -- Uncomment to let GHC detect clashes
  ) where

import Database.RethinkDB hiding (
  (*), (+), (-), (/),
  sum, map, mod, concatMap, (&&),
  not, (||), (/=), (<), (<=), (>), (>=), error, (==), filter,
  max, min,
  zip, zipWith)
