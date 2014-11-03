{-# LANGUAGE OverloadedStrings #-}

import Geodetics.Grid
import Geodetics.Geodetic
import Geodetics.TransverseMercator
import Database.RethinkDB.NoClash hiding (circle, (#))
import qualified Database.RethinkDB as R
import Numeric.Units.Dimensional.SIUnits
import Numeric.Units.Dimensional ((*~), (/~), _1)
import Diagrams.Prelude hiding (project)
import Diagrams.Backend.SVG.CmdLine
import Data.Colour.SRGB

project :: LonLat -> (Double, Double)
project (LonLat lon lat) = let
  pos = Geodetic (lat *~ degree) (lon *~ degree) (0 *~ meter) WGS84
  sf_sw = Geodetic (37.614775 *~ degree) ((-122.522278) *~ degree) (0 *~ meter) WGS84
  offset = GridOffset (0 *~ meter) (0 *~ meter) (0 *~ meter)
  scale = _1
  pt = toGrid (mkGridTM sf_sw offset scale) pos
  y = fromRational $ toRational (northings pt /~ meter / 100)
  x = fromRational $ toRational (eastings pt /~ meter / 100)
  in (x,y)

toLonLat (Point l) = l

main = do
  h <- fmap (use "muni") $ R.connect "localhost" 28015 def
  runs <- run h $ flip R.map (table "runs") $ \run -> [
    (\r -> expr [r!"bg_color", r!"fg_color"]) `R.apply` [get (run!"route_id") (table "routes")],
    run!"stops" # R.map (\stop -> table "stops" # get stop # (!"location"))]
  let lines = flip map runs $ \((bg, fg), pts) ->
        let line = fromVertices (map (p2 . project . toLonLat) pts)
        in (line # lineColor (sRGB24read fg) # lw medium, line # lineColor (sRGB24read bg) # lw veryThick)
  mainWith (mconcat (map fst lines) `atop` mconcat (map snd lines) :: Diagram B R2)