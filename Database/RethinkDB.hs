
-- | RethinkDB client library for Haskell

module Database.RethinkDB (
  module Database.RethinkDB.Driver,
  module Database.RethinkDB.Functions
  ) where

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
  ExprWritable, ExprValueType, Expr(..), ToExpr(..), ToValue(..),
  HasValueType, HaveValueType,
  NumberExpr, BoolExpr, ObjectExpr, ArrayExpr, StringExpr, ValueExpr,
  CanCompare,
  Sequence(..),
  Obj, obj,
  streamToValue, valueToStream,
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
  
  (!), hasattr, pick, pickFrom, attr, (!?),
  pluck, without, unpick, merge,
  
  js, bind, let', var, branch, jsfun, error'
  )