-- | This module exports all of Database.RethinkDB except for the
-- names that clash with Prelude or Data.Time

module Database.RethinkDB.NoClash (
  module Database.RethinkDB,
  -- module Prelude, module Data.Time -- Uncomment to let GHC detect clashes
  ) where

import Data.Time

import Database.RethinkDB hiding (
  UTCTime, ZonedTime,
  (*), (+), (-), (/),
  sum, (++), (.), map, mod, (!!), concatMap, drop, length, take, (&&),
  not, (||), (/=), (<), (<=), (>), (>=), error, (==), filter)