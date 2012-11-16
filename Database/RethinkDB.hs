
-- | RethinkDB client library for Haskell

module Database.RethinkDB (
  module Database.RethinkDB.Types,
  module Database.RethinkDB.Driver,
  module Database.RethinkDB.Functions
  ) where

import Database.RethinkDB.Types (
  ValueTypeKind(..), ExprTypeKind(..)
  )

import Database.RethinkDB.Driver (
  RethinkDBHandle,
  openConnection, use, closeConnection, recvAll, sendAll,
  Database(..), Table(..), Document(..), TableCreateOptions(..),
  db, dbCreate, dbDrop, dbList, 
  table, tableCreate, tableDrop, tableList,
  get, insert, insertMany, upsert, upsertMany,
  update, replace, delete,
  Query, ToQuery(..),
  run, runMaybe, runEither,
  ExprIsView, ExprValueType, Expr(..), ToExpr(..), ToValue(..),
  HasValueType, HaveValueType,
  NumberExpr, BoolExpr, ObjectExpr, ArrayExpr, StringExpr, ValueExpr,
  CanCompare,
  Sequence(..),
  Obj, obj,
  streamToValue, arrayToStream,
  Mapping(..), ToMapping(..)
  )

import Database.RethinkDB.Functions (
  signum, plus, minus, times, divide, mod',
  or', and', eq, neq, ge, le, gt, lt, not',
  
  count, concat, map', filter', between,
  concatMap', slice, append, innerJoin,
  outerJoin, eqJoin, skip, drop', limit,
  trim, nth, asArray, nil, union', reduce,
  distinct, groupedMapReduce, forEach, zip',
  orderBy, groupBy',
  
  (!), pick, (!?), pluck, without, unpick, merge,
  
  js, bind, let', var, if', jsfun, error'
  )