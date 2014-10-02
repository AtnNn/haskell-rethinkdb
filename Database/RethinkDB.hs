-- | Haskell client driver for RethinkDB 
--
-- Based upon the official Javascript, Python and Ruby API: <http://www.rethinkdb.com/api/>
--
-- /How to use/
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import qualified Database.RethinkDB as R
-- > import qualified Database.RethinkDB.NoClash

module Database.RethinkDB (
  -- * Accessing RethinkDB

  RethinkDBHandle,
  connect,
  close,
  use,
  run, run', runOpts,
  next, collect,
  RunFlag(..),
  Cursor,
  Response,
  Result(..),
  RethinkDBError(..),
  ErrorCode(..),
  ReQL,
  JSON(..),

  -- * Manipulating databases

  Database(..),
  db, dbCreate, dbDrop, dbList,

  -- * Manipulating Tables

  Table(..), TableCreateOptions(..), IndexCreateOptions(..),
  table, tableCreate, tableDrop, tableList,
  indexCreate, indexDrop, indexList,

  -- * Writing data

  WriteResponse(..),
  Change(..),
  insert, upsert,
  update, replace, delete,
  returnVals, nonAtomic,

  -- * Selecting data

  Bound(..),
  get, filter, between, getAll,

  -- * Joins

  innerJoin, outerJoin, eqJoin, mergeLeftRight,

  -- * Transformations

  map, withFields, concatMap, drop, take,
  (!!), slice,
  orderBy,  Order(..),
  indexesOf, isEmpty, (++), sample,

  -- * Aggregation

  reduce, reduce1, mapReduce, nub, groupBy, elem,

  -- * Aggregators

  length, sum, average,
  min, max, argmin, argmax,

  -- * Document manipulation

  pluck, without,
  merge, literal, remove,
  append,
  prepend, (\\),
  setInsert, setUnion, setIntersection, setDifference,
  (!), (!?), hasFields,
  insertAt, spliceAt, deleteAt, changeAt, keys,

  -- * Math and logic

  (+), (-), (*), (/), mod, (&&), (||),
  (==), (/=), (>), (<), (<=), (>=), not,

  -- * String manipulation
  
  (=~), toUpper, toLower,
  split, splitOn, splitMax,
  
  -- * Dates and times
  
  UTCTime(..), ZonedTime(..),
  now, time, epochTime, iso8601, inTimezone, during,
  timezone, date, timeOfDay, year, month, day, dayOfWeek, dayOfYear, hours, minutes, seconds,
  toIso8601, toEpochTime,
  
  -- * Control structures

  apply, js, if', forEach, error,
  handle, Expr(..), coerceTo,
  asArray, asString, asNumber, asObject, asBool,
  typeOf, info, json,
  
  -- * Helpers

  obj, Object, Attribute(..), str, num, (.), (#),
  def

  ) where

import Prelude ()

import Database.RethinkDB.ReQL
import Database.RethinkDB.Network
import Database.RethinkDB.Objects
import Database.RethinkDB.Driver
import Database.RethinkDB.Functions
import Database.RethinkDB.Time
import Data.Default
