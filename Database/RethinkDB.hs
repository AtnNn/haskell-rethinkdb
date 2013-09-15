-- | Haskell client driver for RethinkDB 
--
-- Based upon the official Javascript, Python and Ruby API: <http://www.rethinkdb.com/api/>
--
-- /How to use/
--
-- >>> import Database.RethinkDB as R

module Database.RethinkDB (
  -- * Accessing RethinkDB

  RethinkDBHandle,
  connect,
  close,
  use,
  run, run', runOpts,
  next, collect,
  Response,
  Result(..),
  RethinkDBError(..),
  SuccessCode(..),
  ErrorCode(..),

  -- * Manipulating databases

  Database(..),
  db, dbCreate, dbDrop, dbList,

  -- * Manipulating Tables

  Table(..), TableCreateOptions(..),
  table, tableCreate, tableDrop, tableList,

  -- * Writing data

  insert, upsert,
  update, replace, delete,

  -- * Selecting data

  get, filter, between,

  -- * Joins

  innerJoin, outerJoin, eqJoin, mergeRightLeft,

  -- * Transformations

  map, concatMap, drop, take,
  slice, (!!), pluck, without, (++),
  orderBy,  Order(..),

  -- * Aggregation

  reduce, distinct, groupBy,

  -- * Reductions

  count, sum, avg,

  -- * Document manipulation

  merge, append, (!),

  -- * Math and logic

  (+), (-), (*), (/), mod, (&&), (||),
  (==), (!=), (>), (<), (<=), (>=), not,

  -- * Control structures

  Javascript(js), if', forEach, error,

  -- * Sequence conversion

  coerceTo,

  -- * Short constructors

  obj, Object, Attribute(..), str,

  -- * Other
  
  member,

  ) where

import Prelude ()

import Database.RethinkDB.ReQL
import Database.RethinkDB.Network
import Database.RethinkDB.Objects
import Database.RethinkDB.Driver
import Database.RethinkDB.Functions
