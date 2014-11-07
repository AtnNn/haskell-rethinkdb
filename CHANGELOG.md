# Changelog

## 1.15.2.1
- Fix `FromDatum Rational` instance
- Increase upper bound for `network`
- Replace "mempty" with a better error message in `FromDatum` instances
- Removed broken `FromJSON` and `ToJSON` instances for `LonLat`
- Support `Line` and `Polygon` in `FromDatum (Vector a)`
- Added `FromDatum` and `ToDatum` for `LonLat`

## 1.15.2.0
- group and mapReduce generate more efficient code
- Set NoDelay socket option, which significantly improves performance.
- Fix broken indexStatus and indexWait commands.
- Fix and improve benchmark script.

## 1.15.1.0
- Fix compile error with GHC 7.8
- Simplify `Result` instances
- Add documentation to `run`
- Replace the `Result (Either a b)` instance with something more useful

## 1.15.0.0
- Redesigned and improved most of the API. This release is not backwards compatible.
- Added all missing ReQL operations for RethinkDB 1.15 such as binary data, http, changes and geospatial operations
- Switched to the JSON protocol

## 1.8.0.5
- Added many examples to the haddock documentation
- Fixed cursor bug: replaced readMVar with takeMVar
- Explicitly call hClose
- Don't call cursor finalizer twice
- Added the missing non_atomic flag
- Fixed a bug in groupBy that caused the reduction not to be finalised
- Fixed a bug that caused some queries not to be finalised on the server
- Adjusted the Expr instance for (->) to avoid ambiguity
- Generalized the Bound type used by during and used it to fix the broken between function
- Fix the broken order of arguments in eqJoin
- Renamed mergeRightLeft to mergeLeftRight
- Renamed distinct to nub
- Changed run' to return JSON instead of Value because it has a better Show instance
- Made Javascript instances for (->) more liberal
- Made update and replace accept functions that don't return ReQL explicitly
- Added support for multi-indexes
- Renamed member to elem
- Made js monomorphic
- Set fixity of (:=) to 0
- Added Expr instance for tuples
- Made hasFields, withFields and json more monomorphic
- Added the missing (-) method to Num ReQL

## 1.8.0.4
- Added a `WriteResponse` type with a `FromJSON` instance to easily parse the return value of write operations
- `returnVals` was broken. It now works and the returned `WriteResponse` contains the new and old values

## 1.8.0.3
- Fixed bug in time deserialisation

## 1.8.0.2
- Added the Database.RethinkDB.NoClash module that can be imported unqualified.
- Add some missing exports to Database.RethinkDB: RunOptions, UTCTime and ZondeTime
- Fix bugs in tableList, take and (=~)
- Added README.md

## 1.8.0.1
This new release of the Haskell driver for RethinkDB is compatible with rethinkdb 1.8 and above.

It is a complete rewrite from version 0.1. See the Haddock documentation for details.
