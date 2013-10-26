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
  RunOptions(..),
  Cursor,
  Response,
  Result(..),
  RethinkDBError(..),
  SuccessCode(..),
  ErrorCode(..),
  ReQL,

  -- * Manipulating databases

  Database(..),
  db, dbCreate, dbDrop, dbList,

  -- * Manipulating Tables

  Table(..), TableCreateOptions(..),
  table, tableCreate, tableDrop, tableList,
  indexCreate, indexDrop, indexList,

  -- * Writing data

  WriteResponse(..),
  insert, upsert,
  update, replace, delete,
  returnVals, nonAtomic,

  -- * Selecting data

  get, filter, between, getAll,

  -- * Joins

  innerJoin, outerJoin, eqJoin, mergeRightLeft,

  -- * Transformations

  map, withFields, concatMap, drop, take,
  (!!), slice,
  orderBy,  Order(..),
  indexesOf, isEmpty, (++), sample,

  -- * Aggregation

  reduce, reduce1, distinct, groupBy, member,

  -- * Aggregators

  length, sum, avg,

  -- * Document manipulation

  pluck, without,
  merge, append,
  prepend, (\\),
  setInsert, setUnion, setIntersection, setDifference,
  (!), hasFields,
  insertAt, spliceAt, deleteAt, changeAt, keys,

  -- * Math and logic

  (+), (-), (*), (/), mod, (&&), (||),
  (==), (/=), (>), (<), (<=), (>=), not,

  -- * String manipulation
  
  (=~),
  
  -- * Dates and times
  
  UTCTime(..), ZonedTime(..),
  now, time, epochTime, iso8601, inTimezone, during,
  timezone, date, timeOfDay, year, month, day, dayOfWeek, dayOfYear, hours, minutes, seconds,
  toIso8601, toEpochTime,
  
  -- * Control structures

  apply, Javascript(js), if', forEach, error,
  handle, Expr(..), coerceTo,
  asArray, asString, asNumber, asObject, asBool,
  typeOf, info, json,
  
  -- * Helpers

  Obj(..), Object, Attribute(..), str, num, (.), (#),
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
