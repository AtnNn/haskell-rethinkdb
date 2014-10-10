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
-- >>> import Control.Exception
-- >>> :set -XOverloadedStrings
-- >>> let try' x = (try x `asTypeOf` return (Left (undefined :: SomeException))) >> return ()
-- >>> h' <- connect "localhost" 28015 def
-- >>> let h = use "doctests" h'
-- >>> sampleRect1 <- run' h $ line [[-122.423246,37.779388], [-122.423246,37.329898], [-121.886420,37.329898], [-121.886420,37.779388]]
-- >>> sampleLine1 <- run' h $ line [[-122.423246,37.779388], [-88.886420,37.329898]]
-- >>> sampleLine2 <- run' h $ line [[-100.423246, 25.779338], [-100.423246, 49.779338]]
-- >>> samplePoint1 <- run' h $ point (-117.220406) 32.719464
-- >>> samplePoint2 <- run' h $ point (-117.206201) 32.725186
-- >>> samplePolygon1 <- run' h $ polygon [[-122.4,37.7], [-122.4,37.3], [-121.8,37.3], [-121.8,37.7]]
-- >>> samplePolygon2 <- run' h $ polygon [[-122.3,37.4], [-122.3,37.6], [-122.0,37.6], [-122.0,37.4]]
-- >>> sampleArea <- run' h $ circle samplePoint1 2000
--
-- $init_doctests
-- >>> try' $ run' h $ delete $ table "places"
-- >>> try' $ run' h $ tableCreate (table "places") def
-- >>> try' $ run' h $ table "places" # indexCreate "geo" (!"geo") def{indexGeo = Just True}
-- >>> try' $ run' h $ table "places" # indexCreate "location" (!"location") def{indexGeo = Just True}

-- | Convert a line object into a polygon
--
-- >>> run' h $ fill sampleRect1
-- Polygon<[[-122.423246,37.779388],[-122.423246,37.329898],[-121.88642,37.329898],[-121.88642,37.779388],[-122.423246,37.779388]]>
fill :: Expr line => line -> ReQL
fill l = op FILL [l]

-- | Convert a GeoJSON object into a RethinkDB geometry object
--
-- >>> run' h $ geoJSON ["type" := "Point", "coordinates" := [-122.423246,37.779388]]
-- Point<[-122.423246,37.779388]>
geoJSON :: Expr geojson => geojson -> ReQL
geoJSON g = op GEOJSON [g]

-- | Convert a RethinkDB geometry object into a GeoJSON object
--
-- >>> run' h $ toGeoJSON sampleLine1
-- {"coordinates":[[-122.423246,37.779388],[-88.88642,37.329898]],"type":"LineString"}
toGeoJSON :: Expr geo => geo -> ReQL
toGeoJSON g = op TO_GEOJSON [g]

-- | Search a geospatial index for intersecting objects
-- TODO: Insert document to run getIntersecting query against
--
-- >>> run' h $ table "places" # getIntersecting samplePoint1 (Index "geo")
-- []
getIntersecting :: (Expr geo, Expr table) => geo -> Index -> table -> ReQL
getIntersecting g i t = op' GET_INTERSECTING (t, g) $ idx
  where idx = case i of
          PrimaryKey -> []
          Index n -> ["index" := n]

-- | Query a geospatial index for the nearest matches
-- TODO: optional arguments: max_results, max_dist, unit, geo_system
-- TODO: Insert document to run getNearest query against
--
-- >>> run' h $ table "places" # getNearest samplePoint1 (Index "location")
-- []
getNearest :: (Expr point, Expr table) => point -> Index -> table -> ReQL
getNearest p i t = op' GET_NEAREST (t, p) idx
  where idx = case i of
          PrimaryKey -> []
          Index n -> ["index" := n]

-- | Test whether a geometry object includes another
--
-- >>> run' h $ sampleArea # includes samplePoint1
-- true
includes :: (Expr area, Expr geo) => geo -> area -> ReQL
includes g a = op INCLUDES (a, g)

-- | Test if two geometry objects intersects
--
-- >>> run' h $ intersects sampleLine1 sampleLine2
-- true
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

-- | Create a polygon object [[-73,45],[-122,37],[-73,40]]
--
-- >>> run' h $ polygon [[-122.4,37.7], [-122.4,37.3], [-121.8,37.3], [-121.8,37.7]]
-- Polygon<[[-122.4,37.7],[-122.4,37.3],[-121.8,37.3],[-121.8,37.7],[-122.4,37.7]]>
polygon :: Expr points => points -> ReQL
polygon p = op POLYGON [op ARGS [p]]

-- | Punch a hole in a polygon
--
-- >>> run' h $ samplePolygon1 # polygonSub samplePolygon2
-- Polygon<[[-122.4,37.7],[-122.4,37.3],[-121.8,37.3],[-121.8,37.7],[-122.4,37.7]],[[-122.3,37.4],[-122.3,37.6],[-122,37.6],[-122,37.4],[-122.3,37.4]]>
polygonSub :: (Expr polygon, Expr hole) => hole -> polygon -> ReQL
polygonSub h p = op POLYGON_SUB (p, h)

-- | Create a polygon approximating a circle
-- TODO: num_vertices, geo_system, unit, fill
--
-- >>> run' h $ circle samplePoint1 100
-- Polygon<[[-117.220406,32.718562282427186],[-117.22061409908127,32.718579608506055],[-117.22081420126531,32.71863092093052],[-117.22099861693556,32.71871424784805],[-117.22116025923364,32.718826387134506],[-117.22129291638319,32.71896302943737],[-117.22139149039789,32.71911892376696],[-117.22145219300117,32.71928807927419],[-117.2214726912279,32.71946399546295],[-117.22145219710899,32.71963991199213],[-117.22139149798812,32.71980906846879],[-117.22129292630035,32.71996496424926],[-117.2211602699679,32.720101608263526],[-117.22099862685272,32.720213749261376],[-117.22081420885557,32.72029707762976],[-117.22061410318909,32.72034839102367],[-117.220406,32.72036571744295],[-117.2201978968109,32.72034839102367],[-117.21999779114442,32.72029707762976],[-117.21981337314728,32.720213749261376],[-117.2196517300321,32.720101608263526],[-117.21951907369964,32.71996496424926],[-117.21942050201187,32.71980906846879],[-117.219359802891,32.71963991199213],[-117.21933930877209,32.71946399546295],[-117.21935980699882,32.71928807927419],[-117.2194205096021,32.71911892376696],[-117.2195190836168,32.71896302943737],[-117.21965174076635,32.718826387134506],[-117.21981338306443,32.71871424784805],[-117.21999779873468,32.71863092093052],[-117.22019790091872,32.718579608506055],[-117.220406,32.718562282427186]]>
circle :: (Expr point, Expr radius) => point -> radius -> ReQL
circle p r = op CIRCLE (p, r)

-- | Distance between a point and another geometry object
-- TODO: geo_system, unit
--
-- >>> run' h $ distance samplePoint1 samplePoint2
-- 1475.1119643660977
distance :: (Expr a, Expr b) => a -> b -> ReQL
distance a b = op DISTANCE (a,b)
