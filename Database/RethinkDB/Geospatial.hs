{-# LANGUAGE OverloadedStrings #-}
module Database.RethinkDB.Geospatial where

import Database.RethinkDB.ReQL
import Database.RethinkDB.Types
import Database.RethinkDB.Wire.Term

-- $setup
--
-- Get the doctests ready
--
-- >>> :set -XOverloadedStrings
-- >>> :load Database.RethinkDB.NoClash
-- >>> import qualified Database.RethinkDB as R
-- >>> import Control.Exception
-- >>> let try' x = (try x `asTypeOf` return (Left (undefined :: SomeException))) >> return ()
-- >>> h <- fmap (use "doctests") $ connect "localhost" 28015 def
-- >>> try' $ run' h $ dbCreate "doctests"
-- >>> try' $ run' h $ tableCreate "places"
-- >>> try' $ run' h $ table "places" # insert ["location" := point (-120) 60]
-- >>> try' $ run' h $ table "places" # insert ["location" := point (-122) 43]
-- >>> try' $ run' h $ table "places" # insert ["location" := point (-91) 44, "area" := polygon [[-124,30],[-113,54],[-80,44]]]
-- >>> try' $ run' h $ table "places" # ex indexCreate ["geo":=True] "location" (!"location")
-- >>> try' $ run' h $ table "places" # ex indexCreate ["geo":=True] "geo" (!"area")

-- | Convert a line object into a polygon
--
-- >>> run' h $ fill $ line [[-122,37], [-120,39], [-121,38]]
-- Polygon<[[-122,37],[-120,39],[-121,38],[-122,37]]>

fill :: Expr line => line -> ReQL
fill l = op FILL [l]

-- | Convert a GeoJSON object into a RethinkDB geometry object
--
-- >>> run' h $ geoJSON ["type" := "Point", "coordinates" := [-45,80]]
-- Point<[-45,80]>
geoJSON :: Expr geojson => geojson -> ReQL
geoJSON g = op GEOJSON [g]

-- | Convert a RethinkDB geometry object into a GeoJSON object
--
-- >>> run' h $ toGeoJSON $ point (-122.423246) 37.779388
-- {"coordinates":[-122.423246,37.779388],"type":"Point"}
toGeoJSON :: Expr geo => geo -> ReQL
toGeoJSON g = op TO_GEOJSON [g]

-- | Search a geospatial index for intersecting objects
--
-- >>> run' h $ table "places" # getIntersecting (point (-122) 37) (Index "geo")
-- []

getIntersecting :: (Expr geo, Expr table) => geo -> Index -> table -> ReQL
getIntersecting g i t = op' GET_INTERSECTING (t, g) $ idx
  where idx = case i of
          PrimaryKey -> []
          Index n -> ["index" := n]

-- | Query a geospatial index for the nearest matches
--
-- >>> run' h $ table "places" # getNearest (point (-122) 37) (Index "location")
-- []
-- >>> run' h $ table "places" # ex getNearest [maxResults 5, maxDist 10, unit Kilometer] (point (-122) 37) (Index "location")
-- []
getNearest :: (Expr point, Expr table) => point -> Index -> table -> ReQL
getNearest p i t = op' GET_NEAREST (t, p) idx
  where idx = case i of
          PrimaryKey -> []
          Index n -> ["index" := n]

-- | Test whether a geometry object includes another
--
-- >>> run' h $ circle (point (-122) 37) 5000 # includes (point (-120) 48)
-- false
includes :: (Expr area, Expr geo) => geo -> area -> ReQL
includes g a = op INCLUDES (a, g)

-- | Test if two geometry objects intersects
--
-- >>> run' h $ intersects (line [[-122,37],[-120,48]]) (line [[-120,49],[-122,48]])
-- false
intersects :: (Expr a, Expr b) => a -> b -> ReQL
intersects a b = op INTERSECTS (b, a)

-- | Create a line object
--
-- >>> run' h $ line [[-73,45],[-122,37]]
-- Line<[-73,45],[-122,37]>
line :: Expr points => points -> ReQL
line p = op LINE [op ARGS [p]]

-- | Create a point objects
--
-- >>> run' h $ point (-73) 40
-- Point<[-73,40]>
point :: (Expr longitude, Expr latitude) => longitude -> latitude -> ReQL
point lon lat = op POINT (lon, lat)

-- | Create a polygon object
--
-- >>> run' h $ polygon [[-73,45],[-122,37],[-73,40]]
-- Polygon<[[-73,45],[-122,37],[-73,40],[-73,45]]>
polygon :: Expr points => points -> ReQL
polygon p = op POLYGON [op ARGS [p]]

-- | Punch a hole in a polygon
--
-- >>> run' h $ (polygon [[-73,45],[-122,37],[-73,40]]) # polygonSub (polygon [[-73.2,40.1],[-73.2,40.2],[-73.3,40.1]])
-- Polygon<[[-73,45],[-122,37],[-73,40],[-73,45]],[[-73.2,40.1],[-73.2,40.2],[-73.3,40.1],[-73.2,40.1]]>
polygonSub :: (Expr polygon, Expr hole) => hole -> polygon -> ReQL
polygonSub h p = op POLYGON_SUB (p, h)

-- | Create a polygon approximating a circle
--
-- >>> run' h $ ex circle [numVertices 6, unit Kilometer] (point (-73) 40) 100
-- Polygon<[[-73,39.099310036015424],[-74.00751390838496,39.54527799206398],[-74.02083610406069,40.445812561599965],[-73,40.900549591978255],[-71.97916389593931,40.445812561599965],[-71.99248609161504,39.54527799206398],[-73,39.099310036015424]]>
circle :: (Expr point, Expr radius) => point -> radius -> ReQL
circle p r = op CIRCLE (p, r)

-- | Distance between a point and another geometry object
--
-- >>> run' h $ distance (point (-73) 40) (point (-122) 37)
-- 4233453.467303547
-- >>> run' h $ ex distance [unit Mile] (point (-73) 40) (point (-122) 37)
-- 2630.54602825968
distance :: (Expr a, Expr b) => a -> b -> ReQL
distance a b = op DISTANCE (a,b)

-- | Optional argument for getNearest
maxResults :: ReQL -> Attribute a
maxResults n = "max_results" := n
  
-- | Optional argument for getNearest
maxDist :: ReQL -> Attribute a
maxDist d = "max_dist" := d

-- | Optional argument for getNearest, circle and distance
unit :: Unit -> Attribute a
unit u = "unit" := u

-- | Optional argument for circle
numVertices :: ReQL -> Attribute a
numVertices n = "num_vertices" := n

data Unit = Meter | Kilometer | Mile | NauticalMile | Foot

instance Expr Unit where
  expr Meter = "m"
  expr Kilometer = "km"
  expr Mile = "mi"
  expr NauticalMile = "nm"
  expr Foot = "ft"