{-# LANGUAGE OverloadedStrings #-}

module Database.RethinkDB.Time where

import Data.Text (Text)

import Database.RethinkDB.ReQL

import Database.RethinkDB.Protobuf.Ql2.Term.TermType

import Data.Time as Time

-- | The time and date when the query is executed
now :: ReQL
now = op NOW () ()

-- | Build a time object from the year, month, day, hour, minute, second and timezone fields
time :: ReQL -> ReQL -> ReQL -> ReQL -> ReQL -> ReQL -> ReQL -> ReQL
time y m d hh mm ss tz = op TIME [y, m, d, hh, mm, ss, tz] ()

-- | Build a time object given the number of seconds since the unix epoch
epochTime :: ReQL -> ReQL
epochTime t = op EPOCH_TIME [t] ()

-- | Build a time object given an iso8601 string
iso8601 :: ReQL -> ReQL
iso8601 t = op ISO8601 [t] ()

-- | The same time in a different timezone
inTimezone :: Expr time => ReQL -> time -> ReQL
inTimezone tz t = op IN_TIMEZONE (t, tz) ()

data Bound a =
  Open { boundValue ::  a } |
  Closed { boundValue :: a }

boundString :: Bound a -> Text
boundString Open{} = "open"
boundString Closed{} = "closed"

-- | Test if a time is between two other times
during :: (Expr left, Expr right, Expr time) => Bound left -> Bound right -> time -> ReQL
during l r t = op DURING (t, boundValue l, boundValue r) [
  "left_bound" := boundString l, "right_bound" := boundString r]

-- | Extract part of a time value
timezone, date, timeOfDay, year, month, day, dayOfWeek, dayOfYear, hours, minutes, seconds ::
  Expr time => time -> ReQL
timezone t = op TIMEZONE [t] ()
date  t = op DATE [t] ()
timeOfDay t = op TIME_OF_DAY [t] ()
year t = op YEAR [t] ()
month t = op MONTH [t] ()
day t = op DAY [t] ()
dayOfWeek t = op DAY_OF_WEEK [t] ()
dayOfYear t = op DAY_OF_YEAR [t] ()
hours t = op HOURS [t] ()
minutes t = op MINUTES [t] ()
seconds t = op SECONDS [t] ()

-- | Convert a time to another representation
toIso8601, toEpochTime :: Expr t => t -> ReQL
toIso8601 t = op TO_ISO8601 [t] ()
toEpochTime t = op TO_EPOCH_TIME [t] ()