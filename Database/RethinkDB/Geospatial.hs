
module Database.RethinkDB.Geospatial where

import Database.RethinkDB.ReQL
import Database.RethinkDB.Wire.Term

-- | Convert a line object into a polygon
--
-- > line [
--        [-122.423246,37.779388],
--        [-122.423246,37.329898],
--        [-121.886420,37.329898],
--        [-121.886420,37.779388]
--    )
fill :: Expr line => line -> ReQL
fill l = op FILL [l]

geojson :: ()
geojson = undefined

toGeojson :: ()
toGeojson = undefined

getIntersecting :: ()
getIntersecting = undefined

getNearest :: ()
getNearest = undefined

includes :: ()
includes = undefined

intersects :: ()
intersects = undefined

line :: ()
line = undefined

point :: ()
point = undefined

polygon :: ()
polygon = undefined

polygonSub :: ()
polygonSub = undefined

circle :: ()
circle = undefined

distance :: ()
distance = undefined