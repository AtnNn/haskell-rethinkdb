-- | RethinkDB client library for Haskell
--
-- Modelled upon the official Javascript, Python and Ruby API: <http://www.rethinkdb.com/api/>
--
-- /How to use/
--
-- >>> import Database.RethinkDB
-- >>> import qualified Database.RethinkDB.Functions as R

module Database.RethinkDB (
  module Database.RethinkDB.Term,
  module Database.RethinkDB.Network,
  module Database.RethinkDB.Objects,
  module Database.RethinkDB.Driver,
  module Database.RethinkDB.Functions
{-
  -- * Accessing RethinkDB

  RethinkDBHandle,
  openConnection,
  closeConnection,
  use,
  run, runMaybe, runEither, runRaw, runJSON, runBatch,
  next, collect, resultsError,

  -- * Manipulating databases

  Database(..),
  db, dbCreate, dbDrop, dbList,

  -- * Manipulating Tables

  Table(..), tablePrimaryAttr, TableCreateOptions(..),
  table, tableCreate, tableDrop, tableList,

  -- * Writing data

  Document(..),
  insert, insertMany, upsert, upsertMany,
  update, replace, delete,

  -- * Selecting data

  get, filter', between,

  -- * Joins

  innerJoin, outerJoin, eqJoin, zip',

  -- * Transformations

  map', concatMap', orderBy, skip, limit,
  slice, nth, pluck, without, union',

  -- * Aggregation

  reduce, count, distinct, groupedMapReduce, groupBy',

  -- * Reductions

  count', sum', avg,

  -- * Document manipulation

  pick, unpick, merge, append, (!), (!?),

  -- * Operators

  concat,
  add, sub, mul, div', mod, and', or',
  eq, ne, gt, ge, lt, le, not',

  -- * Control structures

  bind, let', var, if', forEach, error', js, jsfun,

  -- * Sequence conversion

  streamToArray, arrayToStream, asArray,

  -- * Short constructors

  Obj, Attribute(..), obj, str, nil,

  -- * Types and type classes

  Query, ToQuery(..),
  ValueTypeKind(..), ExprTypeKind(..),
  ExprIsView, ExprValueType, Expr(..), ToExpr(..), ToValue(..),
  HasValueType, HaveValueType,
  NumberExpr, BoolExpr, ObjectExpr, ArrayExpr, StringExpr, ValueExpr,
  NumberStream, BoolStream, ObjectStream, ArrayStream, StringStream, Selection,
  CanCompare,
  Order(..), ToOrder(..),
  Sequence(..),
  Mapping(..), ToMapping(..),
  Results
-}
  ) where

import Prelude ()

import Database.RethinkDB.Term
import Database.RethinkDB.Network
import Database.RethinkDB.Objects
import Database.RethinkDB.Driver
import Database.RethinkDB.Functions
