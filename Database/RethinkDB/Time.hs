{-# LANGUAGE OverloadedStrings #-}

module Database.RethinkDB.Time where

import qualified  Data.Time as Time
import qualified  Data.Time.Clock.POSIX as Time
import Data.Aeson as JSON
import Data.Aeson.Types (Parser)
import Control.Monad
import Control.Applicative

import Database.RethinkDB.ReQL

import Database.RethinkDB.Protobuf.Ql2.Term.TermType

-- | The time and date when the query is executed
--
-- > >>> run h $ now :: IO (Maybe R.ZonedTime)
-- > Just 2013-10-28 00:01:43.930000066757 +0000
now :: ReQL
now = op NOW ()

-- | Build a time object from the year, month, day, hour, minute, second and timezone fields
--
-- > >>> run h $ time 2011 12 24 23 59 59 "Z" :: IO (Maybe R.ZonedTime)
-- > Just 2011-12-24 23:59:59 +0000
time :: ReQL -> ReQL -> ReQL -> ReQL -> ReQL -> ReQL -> ReQL -> ReQL
time y m d hh mm ss tz = op TIME [y, m, d, hh, mm, ss, tz]

-- | Build a time object given the number of seconds since the unix epoch
--
-- > >>> run h $ epochTime 1147162826 :: IO (Maybe R.ZonedTime)
-- > Just 2006-05-09 08:20:26 +0000
epochTime :: ReQL -> ReQL
epochTime t = op EPOCH_TIME [t]

-- | Build a time object given an iso8601 string
--
-- > >>> run h $ iso8601 "2012-01-07T08:34:00-0700" :: IO (Maybe R.UTCTime)
-- > Just 2012-01-07 15:34:00 UTC
iso8601 :: ReQL -> ReQL
iso8601 t = op ISO8601 [t]

-- | The same time in a different timezone
--
-- > >>> run h $ inTimezone "+0800" now :: IO (Maybe R.ZonedTime)
-- > Just 2013-10-28 08:16:39.22000002861 +0800
inTimezone :: Expr time => ReQL -> time -> ReQL
inTimezone tz t = op IN_TIMEZONE (t, tz)

-- | Test if a time is between two other times
--
-- > >>> run h $ during (Open $ now - (60*60)) (Closed now) $ epochTime 1382919271 :: IO (Maybe Bool)
-- > Just True
during :: (Expr left, Expr right, Expr time) => Bound left -> Bound right -> time -> ReQL
during l r t = op' DURING (t, getBound l, getBound r) [
  "left_bound" :== closedOrOpen l, "right_bound" :== closedOrOpen r]

-- | Extract part of a time value
timezone, date, timeOfDay, year, month, day, dayOfWeek, dayOfYear, hours, minutes, seconds ::
  Expr time => time -> ReQL
timezone t = op TIMEZONE [t]
date  t = op DATE [t]
timeOfDay t = op TIME_OF_DAY [t]
year t = op YEAR [t]
month t = op MONTH [t]
day t = op DAY [t]
dayOfWeek t = op DAY_OF_WEEK [t]
dayOfYear t = op DAY_OF_YEAR [t]
hours t = op HOURS [t]
minutes t = op MINUTES [t]
seconds t = op SECONDS [t]

-- | Convert a time to another representation
toIso8601, toEpochTime :: Expr t => t -> ReQL
toIso8601 t = op TO_ISO8601 [t]
toEpochTime t = op TO_EPOCH_TIME [t]

-- | Time with no time zone
--
-- The default FromJSON instance for Data.Time.UTCTime is incompatible with ReQL's time type
newtype UTCTime = UTCTime Time.UTCTime

instance Show UTCTime where
  show (UTCTime t) = show t

-- | Time with a time zone
--
-- The default FromJSON instance for Data.Time.ZonedTime is incompatible with ReQL's time type
newtype ZonedTime = ZonedTime Time.ZonedTime

instance Show ZonedTime where
  show (ZonedTime t) = show t

instance FromJSON UTCTime where
  parseJSON (JSON.Object v) = UTCTime . Time.posixSecondsToUTCTime . fromRational <$> v .: "epoch_time"
  parseJSON _ = mzero

instance FromJSON ZonedTime where
  parseJSON (JSON.Object v) = do
                         tz <- v .: "timezone"
                         t <- v.: "epoch_time"
                         tz' <- parseTimeZone tz
                         return . ZonedTime $ Time.utcToZonedTime tz'
                           (Time.posixSecondsToUTCTime (fromRational t))
  parseJSON _ = mzero

parseTimeZone :: String -> Parser Time.TimeZone
parseTimeZone "Z" = return Time.utc
parseTimeZone tz = Time.minutesToTimeZone <$> case tz of 
  ('-':tz') -> negate <$> go tz'
  ('+':tz') -> go tz'
  _ -> go tz
  where
    go tz' = do
        (h, _:m) <- return $ break (==':') tz'
        ([(hh, "")], [(mm, "")]) <- return $ (reads h, reads m)
        return $ hh * 60 + mm

instance Expr UTCTime where
  expr (UTCTime t) = expr t

instance Expr ZonedTime where
  expr (ZonedTime t) = expr t