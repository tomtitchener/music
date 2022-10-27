
-- See:
--   https://github.com/timbod7/haskell-chart/wiki,
--   https://hackage.haskell.org/package/Chart-1.9.4

-- NB:
--  -Diagrams is the older, less efficent back end vs. Cairo.
--   But "For the cairo backend, it is recommended to install and test gtk2hs first."
--   And the instructions for that look complicated.
--  -Examples on the wiki all show output to PNG, which
--   default readers saw as corrupt.
--   Files are just text though, with web headers showing SVG
--   is actually the format, which does render though only inside a browser.
--  -When using 1000000 points the memory footprint blows up before
--   it finishes writing the output file.
--   The code below uses 100000 instead, which leaves holes in the lower
--   leaves of the output fern but which does render, whereas 500000
--   successfully produces an output file but the rendering crashes,
--   at least on Chrome.
--  -Takes around 3/4 of a minute to run:
--    % time stack exec fern
--    time stack exec fern
--    stack exec fern  41.58s user 1.51s system 98% cpu 43.548 total
--  -Seems to spend almost all the time writing out a large file:
--    -rw-r--r--@  1 tomtitchener  staff  107188001 Oct 27 15:57 fern.svg
--  -Code from https://en.wikipedia.org/wiki/Barnsley_fern for affine
--   transform is easy to understand and copy, and works fine.

module Main where

import Control.Lens
import Control.Monad.Random.Class (MonadRandom(getRandoms))
import Data.List (find)
import Data.Traversable (mapAccumR)
import Data.Tuple.Extra (dupe, (&&&))

import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart.Backend.Diagrams

import Linear.Matrix
import Linear.V2

main :: IO (PickFn ())
main = do
  xyPrs <- getRandoms <&> snd . mapAccumR (nextXY params) (V2 0.0 0.0) . take 100000
  renderableToFile def "fern.svg" (chart (v22Pr <$> xyPrs))
  where
    v22Pr = (^. _x) &&& (^. _y)

params :: [(Float,M22 Float,V2 Float)]
params = [(0.01, V2 (V2   0.0    0.0)   (V2   0.0   0.16), V2 0.0 0.0)
         ,(0.86, V2 (V2   0.85 (-0.04)) (V2   0.04  0.85), V2 0.0 1.60)
         ,(0.93, V2 (V2   0.20   0.23)  (V2 (-0.26) 0.22), V2 0.0 1.60)
         ,(1.0,  V2 (V2 (-0.15)  0.26)  (V2   0.28  0.24), V2 0.0 0.44)]

nextXY :: [(Float,M22 Float,V2 Float)] -> V2 Float -> Float -> (V2 Float,V2 Float)
nextXY ps v r =
  case find (\(x,_,_) -> r < x) ps of
    Just (_,m,v') -> dupe $ v *! m + v'
    Nothing -> error $ "nextXYP no such " <> show r <> " in " <> show ps

chart :: (PlotValue x, PlotValue y) => [(x, y)] -> Renderable ()
chart coords = toRenderable layout
  where
    fern = plot_points_style .~ filledCircles 1 (opaque green)
           $ plot_points_values .~ coords
           $ def
    layout = layout_title .~ "fern"
           $ layout_plots .~ [toPlot fern]
           $ def

-- TBD: Add config file with data for list of 
-- 1) random percent
-- 2) V2 (V2 Float) matrix
-- 3) V2 Float vector
-- Create parsers for
-- 1) V2 (V2 Float)
-- 2) V2 Float
-- 3) Float
-- 4) Non-empty list of (Float, V2 (V2 Float), V2 Float)
-- Add config param for name of config YML file with default fern.yml
-- Call parser to get [(Float, V2 (V2 Float), V2 Float)]
-- Intepret [(Float, V2 (V2 Float), V2 Float)] in nextXY
-- Push with default config file for Barnsley fern.
