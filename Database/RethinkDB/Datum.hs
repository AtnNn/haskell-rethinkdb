{-# LANGUAGE OverloadedStrings, PatternGuards, DefaultSignatures, 
    FlexibleInstances, OverlappingInstances #-}

-- TODO: geo types should correctly parse as tuples and lists

module Database.RethinkDB.Datum (
  parse, Parser, Result(..),
  Datum(..), ToDatum(..), FromDatum(..), fromDatum,
  LonLat(..), Array, Object, Line, Polygon,
  (.=), (.:), (.:?),
  encode, decode, eitherDecode,
  resultToMaybe, resultToEither,
  object
  ) where

import qualified Data.Aeson as J
import Data.Aeson.Types (Parser, Result(..), FromJSON(..), parse, ToJSON(..), Value)
import Data.Aeson (fromJSON)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.Text as ST
import Data.Text.Encoding (encodeUtf8)
import qualified Data.HashMap.Strict as HM
import Data.Monoid
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.ByteString.Base64 as Base64
import Control.Applicative
import Data.Scientific
import Data.Int
import qualified Data.ByteString.Char8 as Char8
import Control.Monad
import qualified Data.Map as Map

-- | A ReQL value
data Datum =
  Null |
  Bool Bool |
  String ST.Text |
  Number Double |
  Array Array |
  Object Object |
  Time ZonedTime |
  Point LonLat |
  Line Line |
  Polygon Polygon |
  Binary SB.ByteString

class FromDatum a where
  parseDatum :: Datum -> Parser a
  default parseDatum :: FromJSON a => Datum -> Parser a
  parseDatum = parseJSON . toJSON

instance FromDatum a => FromDatum [a] where
  parseDatum (Array v) = mapM parseDatum $ V.toList v
  parseDatum _ = mempty

instance FromDatum Datum where
  parseDatum = return

instance FromDatum a => FromDatum (Map.Map String a) where
  parseDatum (Object o) = fmap Map.fromList . mapM (\(k,v) -> (,) (ST.unpack k) <$> parseDatum v) $ HM.toList o
  parseDatum _ = mempty

-- TODO: populate this list
instance FromDatum String
instance FromDatum Int
instance FromDatum Double
instance FromDatum Bool
instance FromDatum ST.Text
instance FromDatum J.Value

type Array = Vector Datum
type Object = HM.HashMap ST.Text Datum
type Line = Vector LonLat
type Polygon = Vector (Vector LonLat)

data LonLat = LonLat { longitude, latitude :: Double }
            deriving (Eq, Ord)

instance ToJSON LonLat where
  toJSON (LonLat a b) = toJSON [a, b]

instance Eq Datum where
  Null == Null = True
  Bool a == Bool b = a == b
  String a == String b = a == b
  Number a == Number b = a == b
  Array a == Array b = a == b
  Object a == Object b = a == b
  Time a == Time b = zonedTimeToUTC a == zonedTimeToUTC b
  Point a == Point b = a == b
  Line a == Line b = a == b
  Polygon a == Polygon b = a == b
  Binary a == Binary b = a == b
  _ == _ = False

instance Show LonLat where
  show (LonLat lon lat) = "[" ++ showDouble lon ++ "," ++ showDouble lat ++ "]"

instance J.FromJSON LonLat where
  parseJSON v | Success [lon, lat] <- fromJSON v = return $ LonLat lon lat
  parseJSON _ = mempty

instance Show Datum where
  show Null = "null"
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (Number d) = showDouble d
  show (String t) = show t
  show (Array v) = "[" ++ intercalate "," (map show $ V.toList v) ++ "]"
  show (Object o) = "{" ++ intercalate "," (map (\(k,v) -> show k ++ ":" ++ show v) $ HM.toList o) ++ "}"
  show (Time t) = "Time<" ++ show t ++ ">"
  show (Point p) = "Point<" ++ show p ++ ">"
  show (Line l) = "Line<" ++ intercalate "," (map show $ V.toList l) ++ ">"
  show (Polygon p) = "Polygon<" ++ intercalate "," (map (\x -> "[" ++ intercalate "," (map show $ V.toList x) ++ "]") (V.toList p)) ++ ">"
  show (Binary b) = "Binary<" ++ show b ++ ">"

showDouble :: Double -> String
showDouble d = let s = show d in if ".0" `isSuffixOf` s then init (init s) else s

fromDatum :: FromDatum a => Datum -> Result a
fromDatum = parse parseDatum

class ToDatum a where
  toDatum :: a -> Datum
  default toDatum :: ToJSON a => a -> Datum
  toDatum = toJSONDatum

instance ToDatum a => ToDatum [a] where
  toDatum = Array . V.fromList . map toDatum

instance ToDatum a => ToDatum (V.Vector a) where
  toDatum = Array . V.map toDatum

instance ToDatum Datum where
  toDatum = id

instance ToDatum () where
  toDatum _ = Array $ V.empty

instance (ToDatum a, ToDatum b) => ToDatum (a, b) where
  toDatum (a, b) = Array $ V.fromList [toDatum a, toDatum b]

instance (ToDatum a, ToDatum b, ToDatum c) => ToDatum (a, b, c) where
  toDatum (a, b, c) = Array $ V.fromList [toDatum a, toDatum b, toDatum c]

instance (ToDatum a, ToDatum b, ToDatum c, ToDatum d) => ToDatum (a, b, c, d) where
  toDatum (a, b, c, d) = Array $ V.fromList [toDatum a, toDatum b, toDatum c, toDatum d]

instance ToDatum a => ToDatum (HM.HashMap ST.Text a) where
  toDatum = Object . HM.map toDatum

instance ToDatum ZonedTime where
  toDatum = Time

instance ToDatum UTCTime where
  toDatum = Time . utcToZonedTime (TimeZone 0 False "Z")

instance ToDatum Value
instance ToDatum Int
instance ToDatum Char
instance ToDatum String
instance ToDatum Int64
instance ToDatum Integer
instance ToDatum ST.Text
instance ToDatum Bool
instance ToDatum Double
instance ToDatum Rational

toJSONDatum :: ToJSON a => a -> Datum
toJSONDatum a = case toJSON a of
  J.Object o ->
    let asObject = Object $ HM.map toJSONDatum o
        ptype = HM.lookup "$reql_type$" o
    in case ptype of
      Just "GEOMETRY" |
        Just t <- HM.lookup "type" o,
        Just c <- HM.lookup "coordinates" o ->
          case t of
            "Point" | Success p <- fromJSON c -> Point p
            "LineString" | Success l <- fromJSON c -> Line l
            "Polygon" | Success p <- fromJSON c -> Polygon p
            _ -> asObject
      Just "TIME" |
        Just (J.Number ts) <- HM.lookup "epoch_time" o,
        Just (J.String tz) <- HM.lookup "timezone" o,
        Just tz' <- parseTimeZone (ST.unpack tz) ->
          Time $ utcToZonedTime tz' (posixSecondsToUTCTime . fromRational . toRational $ ts)
      Just "BINARY" |
        Just (J.String b64) <- HM.lookup "data" o,
        Right dat <- Base64.decode (encodeUtf8 b64) ->
         Binary dat 
      _ -> asObject
  J.Null -> Null
  J.Bool b -> Bool b
  J.Number s -> Number (toRealFloat s)
  J.String t -> String t
  J.Array v -> Array (fmap toJSONDatum v)

instance J.FromJSON Datum where
  parseJSON = return . toJSONDatum

instance ToJSON Datum where
  toJSON Null = J.Null
  toJSON (Bool b) = J.Bool b
  toJSON (Number d) = J.Number $ realToFrac d
  toJSON (String t) = J.String t
  toJSON (Array v) = J.Array $ V.map toJSON v
  toJSON (Object o) = J.Object $ HM.map toJSON o
  toJSON (Time ts@(ZonedTime _ tz)) = J.object [
    "$reql_type$" J..= ("TIME" :: ST.Text),
    "epoch_time" J..= (realToFrac (utcTimeToPOSIXSeconds (zonedTimeToUTC ts)) :: Double),
    "timezone" J..= timeZoneOffsetString tz]
  toJSON (Point p) = J.object [
    "$reql_type$" J..= ("GEOMETRY" :: ST.Text),
    "type" J..= ("Point" :: ST.Text),
    "coordinates" J..= toJSON p]
  toJSON (Line l) = J.object [
    "$reql_type$" J..= ("GEOMETRY" :: ST.Text),
    "type" J..= ("LineString" :: ST.Text),
    "coordinates" J..= toJSON l]
  toJSON (Polygon p) = J.object [
    "$reql_type$" J..= ("GEOMETRY" :: ST.Text),
    "type" J..= ("Polygon" :: ST.Text),
    "coordinates" J..= toJSON p]
  toJSON (Binary b) = J.object [
    "$reql_type$" J..= ("BINARY" :: ST.Text),
    "data" J..= Char8.unpack (Base64.encode b)]
  

parseTimeZone :: String -> Maybe TimeZone
parseTimeZone "Z" = Just utc
parseTimeZone tz = minutesToTimeZone <$> case tz of 
  ('-':tz') -> negate <$> go tz'
  ('+':tz') -> go tz'
  _ -> go tz
  where
    go tz' =
        let (h, _:m) = break (==':') tz' in
        case (reads h, reads m) of
            ([(hh, "")], [(mm, "")]) -> Just $ hh * 60 + mm
            _ -> Nothing

-- ReQL datums are compared alphabetically by type name. Objects are
-- compared field by field in alphabetical order.
instance Ord Datum where
  compare (Object a) (Object b) =
        compare (sort $ HM.keys a) (sort $ HM.keys b) <>
        mconcat (map (\k -> (a HM.! k) `compare` (b HM.! k) ) (sort $ HM.keys a))
  compare (Array a) (Array b) = compare a b
  compare (String a) (String b) = compare a b
  compare (Number a) (Number b) = compare a b
  compare (Bool a) (Bool b) = compare a b
  compare Null Null = EQ
  compare (Time a) (Time b) = zonedTimeToUTC a `compare` zonedTimeToUTC b
  compare (Point a) (Point b) = compare a b
  compare (Line a) (Line b) = compare a b
  compare (Polygon a) (Polygon b) = compare a b
  compare (Binary a) (Binary b) = compare a b
  compare Array{} _ = LT
  compare _ Array{} = GT
  compare Bool{} _ = LT
  compare _ Bool{} = GT
  compare Null _ = LT
  compare _ Null = GT
  compare Number{} _ = LT
  compare _ Number{} = GT
  compare Object{} _ = LT
  compare _ Object{} = GT
  compare Polygon{} _ = LT
  compare _ Polygon{} = GT
  compare Line{} _ = LT
  compare _ Line{} = GT
  compare Point{} _ = LT
  compare _ Point{} = GT
  compare Time{} _ = LT
  compare _ Time{} = GT
  compare Binary{} _ = LT -- TODO: is this the right ordering for binary
  compare _ Binary{} = GT

(.=) :: ToDatum a => ST.Text -> a -> (ST.Text, Datum)
k .= v = (k, toDatum v)

(.:) :: FromDatum a => HM.HashMap ST.Text Datum -> ST.Text -> Parser a
o .: k = maybe mempty parseDatum $ HM.lookup k o

(.:?) :: FromDatum a => HM.HashMap ST.Text Datum -> ST.Text -> Parser (Maybe a)
o .:? k = maybe (return Nothing) (fmap Just . parseDatum) $ HM.lookup k o

encode :: ToDatum a => a -> LB.ByteString
encode = J.encode . toDatum

decode :: FromDatum a => LB.ByteString -> Maybe a
decode = resultToMaybe . fromDatum <=< J.decode

eitherDecode :: FromDatum a => LB.ByteString -> Either String a
eitherDecode b = resultToEither . fromDatum =<< J.eitherDecode b

resultToMaybe :: Result a -> Maybe a
resultToMaybe (Success a) = Just a
resultToMaybe (Error _) = Nothing

resultToEither :: Result a -> Either String a
resultToEither (Success a) = Right a
resultToEither (Error s) = Left s

object :: [(ST.Text, Datum)] -> Datum
object = Object . HM.fromList