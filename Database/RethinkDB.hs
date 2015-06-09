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

  connect,
  RethinkDBHandle,
  close,
  use,
  run, run', runOpts,
  ReQL,
  Datum(..),
  ToDatum(..), FromDatum(..), fromDatum,
  RunFlag(..),
  noReplyWait,
  RethinkDBError(..),
  ErrorCode(..),
  Response,
  Result(..),
  
  -- * Cursors
  
  next, collect, collect', each,
  Cursor,
  
  -- * Manipulating databases

  Database(..),
  dbCreate, dbDrop, dbList,

  -- * Manipulating Tables

  Table(..),
  tableCreate, tableDrop, tableList,
  indexCreate, indexDrop, indexList,
  indexRename, indexStatus, indexWait,
  changes,

  -- * Writing data

  WriteResponse(..),
  Change(..),
  insert,
  update, replace, delete,
  sync,
  returnChanges, nonAtomic,
  durability, Durability,
  conflict, ConflictResolution(..),

  -- * Selecting data

  db, table,
  get, getAll,
  filter, between, minval, maxval,
  Bound(..),

  -- * Joins

  innerJoin, outerJoin, eqJoin, zip,
  Index(..),

  -- * Transformations

  map, zipWith, zipWithN,
  withFields, concatMap,
  orderBy, asc, desc,
  skip, limit, slice,
  indexesOf, isEmpty, union, sample,

  -- * Aggregation

  group,
  reduce, reduce0,
  distinct, contains,
  mapReduce,

  -- * Aggregators

  count, sum, avg,
  min, max, argmin, argmax,

  -- * Document manipulation

  pluck, without,
  merge,
  append, prepend,
  difference,
  setInsert, setUnion, setIntersection, setDifference,
  (!), (!?),
  hasFields,
  insertAt, spliceAt, deleteAt, changeAt, keys,  
  literal, remove,
  Attribute(..),

  -- * String manipulation
  
  match, upcase, downcase,
  split, splitOn, splitMax,
  
  -- * Math and logic

  (+), (-), (*), (/), mod,
  (&&), (||),
  (==), (/=), (>), (>=), (<), (<=),
  not,
  random, randomTo, randomFromTo,
  
  -- * Dates and times
  
  now, time, epochTime, iso8601, inTimezone, during,
  timezone, date, timeOfDay, year, month, day, dayOfWeek,
  dayOfYear, hours, minutes, seconds,
  toIso8601, toEpochTime,
  
  -- * Control structures

  args, apply, js, branch, forEach,
  range, rangeFromTo, rangeAll,
  error,
  handle, Expr(..), coerceTo,
  asArray, asString, asNumber, asObject, asBool,
  typeOf, info, json, toJSON, uuid,
  http,
  HttpOptions(..), HttpResultFormat(..),
  HttpMethod(..), PaginationStrategy(..),
  
  -- * Geospatial commands
  
  circle, distance, fill, geoJSON,
  toGeoJSON, getIntersecting,
  getNearest, includes, intersects,
  line, point, polygon, polygonSub,
  LonLat(..), GeoLine(..), GeoPolygon(..),
  maxResults, maxDist, unit, numVertices,
  Unit(..),
  
  -- * Administration
  
  config, rebalance, reconfigure,
  status, wait,
  
  -- * Helpers

  ex, str, num, (#), note, empty,
  def
  
  ) where

import Prelude ()

import Database.RethinkDB.ReQL
import Database.RethinkDB.Network
import Database.RethinkDB.Types
import Database.RethinkDB.Driver
import Database.RethinkDB.Functions
import Database.RethinkDB.Time
import Database.RethinkDB.Geospatial
import Database.RethinkDB.Datum hiding (Result)
import Data.Default
