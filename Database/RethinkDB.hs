-- | RethinkDB client library for Haskell
--
-- Modelled upon the official Javascript, Python and Ruby API: <http://www.rethinkdb.com/api/>
--
-- /How to use/
--
-- >>> import Database.RethinkDB
-- >>> import qualified Database.RethinkDB.Functions as R

module Database.RethinkDB (
  -- * Accessing RethinkDB

  RethinkDBHandle,
  connect,
  close,
  use,
  run, run',
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

  map, concatMap, orderBy, drop, take,
  slice, (!!), pluck, without, (++),

  -- * Aggregation

  reduce, distinct, groupBy,

  -- * Reductions

  count, sum, avg,

  -- * Document manipulation

  merge, append, (!),

  -- * Operators

  (+), (-), (*), (/), mod, (&&), (||),
  (==), (!=), (>), (<), (<=), (>=), not,

  -- * Control structures

  Javascript(js), if', forEach, error,

  -- * Sequence conversion

  coerceTo,

  -- * Short constructors

  obj, Object(..), Attribute(..), str,

  -- * Types and type classes

  Order(..),
  
  -- * Other
  
  member,

  ) where

import Prelude ()

import Database.RethinkDB.Term
import Database.RethinkDB.Network
import Database.RethinkDB.Objects
import Database.RethinkDB.Driver
import Database.RethinkDB.Functions
