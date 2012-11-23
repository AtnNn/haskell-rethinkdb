
-- | RethinkDB client library for Haskell
-- 
-- Modelled upon the official Javascript and Python API: <http://www.rethinkdb.com/api/>
-- 
-- /Notes on the Haskell API/
-- 
-- The API is statically typed. It is sometimes necessary to add type annotations.
-- 
-- The Functions sub-module defines many aliases that are more natural in haskell, such
-- as @R.+@ for @add@, @R.drop@ for @skip@ and @R.fold@ for @reduce@, with arguments
-- in the same order as @foldl@.
-- 
-- The official API has nicer backtraces when an error happens.
-- 
-- /How to use/
--  
-- >>> import Database.RethinkDB
-- >>> import qualified Database.RethinkDB.Functions as R

module Database.RethinkDB (
  
  -- * Accessing RethinkDB
  
  RethinkDBHandle,
  openConnection,
  closeConnection, 
  use,
  run, runMaybe, runEither, runRaw, runJSON, runBatch,
  next, collect,
  
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
  
  -- * Types
  
  Query, ToQuery(..),
  ValueTypeKind(..), ExprTypeKind(..),
  ExprIsView, ExprValueType, Expr(..), ToExpr(..), ToValue(..),
  HasValueType, HaveValueType,
  NumberExpr, BoolExpr, ObjectExpr, ArrayExpr, StringExpr, ValueExpr,
  CanCompare,
  Sequence(..),
  Mapping(..), ToMapping(..),
  Results
  ) where

import Prelude ()

import Database.RethinkDB.Types
import Database.RethinkDB.Driver
import Database.RethinkDB.Functions