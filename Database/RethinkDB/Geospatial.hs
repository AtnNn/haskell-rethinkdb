{-# LANGUAGE OverloadedStrings #-}

module Database.RethinkDB.Geospatial where

import Database.RethinkDB.ReQL
import Database.RethinkDB.Types
import Database.RethinkDB.Wire.Term

-- $setup
--
-- Get the doctests ready
--
-- >>> import qualified Database.RethinkDB as R
-- >>> import Database.RethinkDB.NoClash
-- >>> h' <- connect "localhost" 28015 def
-- >>> let h = use "doctests" h'
-- >>> let sampleLine = run' h $ line [[-122.423246,37.779388], [-122.423246,37.329898], [-121.886420,37.329898], [-121.886420,37.779388]]
-- >>> let samplePoint = run' h $ point (-122.423246) 37.779388

-- | Convert a line object into a polygon
--
-- >>> run' h $ fill sampleLine

fill :: Expr line => line -> ReQL
fill l = op FILL [l]

-- | Convert a GeoJSON object into a RethinkDB geometry object
--
-- >>> run' h $ geoJSON 

geoJSON :: Expr geojson => geojson -> ReQL
geoJSON g = op GEOJSON [g]

-- | Convert a RethinkDB geometry object into a GeoJSON object
--
-- >>> run' h $ toGeoJSON sampleLine

toGeoJSON :: Expr geo => geo -> ReQL
toGeoJSON g = op TO_GEOJSON [g]

-- | Search a geospatial index for intersecting objects
--
-- >>> run' h $ table "places" # getIntersecting samplePoint (Index "geo")

getIntersecting :: (Expr geo, Expr table) => geo -> Index -> table -> ReQL
getIntersecting g i t = op' GET_INTERSECTING (t, g) $ idx
  where idx = case i of
          PrimaryKey -> []
          Index n -> ["index" := n]

-- | Query a geospatial index for the nearest matches
--
-- >>> run' h $ table "places" # getNearest samplePoint (Index "location")

-- TODO: optional arguments: max_results, max_dist, unit, geo_system
getNearest :: (Expr point, Expr table) => point -> Index -> table -> ReQL
getNearest p i t = op' GET_NEAREST (t, p) idx
  where idx = case i of
          PrimaryKey -> []
          Index n -> ["index" := n]

-- | Test whether a geometry object includes another
--
-- >>> run' h $ sampleArea # includes samplePoint
includes :: (Expr area, Expr geo) => geo -> area -> ReQL
includes g a = op INCLUDES (a, g)

-- | Test if two geometry objects intersects
--
-- >>> run' h $ intersects lineA lineB
intersects :: (Expr a, Expr b) => a -> b -> ReQL
intersects a b = op INTERSECTS (b, a)

-- | Create a line object
--
-- >>> run' h $ line [[-73,45],[-122,37]]
line :: Expr points => points -> ReQL
line p = op LINE [op ARGS [p]]

-- | Create a point objects
--
-- >>> run' h $ point (-73) 40
point :: (Expr longitude, Expr latitude) => longitude -> latitude -> ReQL
point lon lat = op POINT (lon, lat)

-- | Create a polygon object [[-73,45],[-122,37],[-73,40]]
--
-- >>> run' h $ polygon 
polygon :: Expr points => points -> ReQL
polygon p = op POLYGON [op ARGS [p]]

-- | Punch a hole in a polygon
--
-- >>> run' h $ samplePolygon # polygonSub hole
polygonSub :: (Expr polygon, Expr hole) => hole -> polygon -> ReQL
polygonSub h p = op POLYGON_SUB (p, h)

-- | Create a polygon approximating a circle
--
-- >>> run' h $ circle samplePoint 100

-- TODO: num_vertices, geo_system, unit, fill
circle :: (Expr point, Expr radius) => point -> radius -> ReQL
circle p r = op CIRCLE (p, r)

-- | Distance between a point and another geometry object
--
-- >>> run' h $ distance samplePoint sampleLine

-- TODO: geo_system, unit
distance :: (Expr a, Expr b) => a -> b -> ReQL
distance a b = op DISTANCE (a,b)