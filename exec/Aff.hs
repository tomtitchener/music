{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

-- Customize aff.yml for different affine transforms.
-- To generate Barnsley fern:
-- $ stack exec aff -- -t fern -c ./exec/aff.yml
-- Code from https://en.wikipedia.org/wiki/Barnsley_fern for affine
-- transform is easy to understand and copy, and works fine.

-- See:
--   https://github.com/timbod7/haskell-chart/wiki,
--   https://hackage.haskell.org/package/Chart-1.9.4

-- Notes:
--  -Diagrams is the older, less efficent back end vs. Cairo.
--   But "For the cairo backend, it is recommended to install and test gtk2hs first."
--   And the instructions for that look complicated.
--  -Examples on the wiki all show output to PNG, which
--   default readers saw as corrupt.
--   Files are just text though, with web headers showing SVG
--   is actually the format, which does render though only inside a browser.
--  -When using 1000000 points the memory footprint blows up before
--   it finishes writing the output file.
--   The configuration file uses 100000 instead, which leaves holes in the lower
--   leaves of the output fern but which does render, whereas 500000
--   successfully produces an output file but the rendering crashes,
--   at least on Chrome.
--  -Takes around 40+ seconds to run:
--    % time stack exec aff
--    time stack exec aff
--    stack exec aff  41.58s user 1.51s system 98% cpu 43.548 total
--  -Seems to spend almost all the time writing out a large output file:
--    -rw-r--r--@  1 tomtitchener  staff  107188001 Oct 27 15:57 fern.svg

module Main where

import Control.Lens
import Control.Monad (void)
import Control.Monad.Random.Class (MonadRandom(getRandoms))
import Data.List (find)
import Data.List.Split (splitOn)
import Data.Traversable (mapAccumR)
import Data.Tuple.Extra (dupe, (&&&))
import Data.Text (unpack)
-- graphics
import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart.Backend.Diagrams
-- math 
import Linear.Matrix
import Linear.V2
-- inputs
import Options.Applicative as AP
import System.Directory (doesFileExist)
import qualified Data.Yaml as Y
-- parsing
import Data.Aeson (Value)
import Data.Aeson.Key (fromString)
import Data.Aeson.Lens (key,_String)
import Text.Parsec (parse,char,spaces,string,between,sepBy1)
import Text.Parsec.Number (int,floating)
import Text.Parsec.String as PS (Parser)

data Options = Options
  { _optConfigYaml :: FilePath
  , _optTarget     :: String
  } deriving Show

options :: AP.Parser Options
options = Options
  <$> strOption (short 'c' <> metavar "CONFIG_FILE"
                 <> value "config.yml"
                 <> help "Default: config.yml, if available")
  <*> strOption (short 't' <> metavar "TARGET"
                 <>  value ""
                 <> help "Config file target")

opts :: AP.ParserInfo Options
opts = info (helper <*> options)
            (header "aff")

main :: IO (PickFn ())
main = do
  Options{..} <- AP.execParser opts
  config <- do
    e <- doesFileExist _optConfigYaml
    if e
    then either (error . Y.prettyPrintParseException) id <$> Y.decodeFileEither _optConfigYaml
    else error $ "config file " <> _optConfigYaml <> " does not exist"
  let (cntPoints,params) = readParams _optTarget config
  xyPrs <- getRandoms <&> snd . mapAccumR (nextXY params) (V2 0.0 0.0) . take cntPoints
  renderableToFile def (_optTarget <> ".svg") (chart _optTarget (v22Pr <$> xyPrs))
  where
    v22Pr = (^. _x) &&& (^. _y)

-- target is top-level key in YAML file, Value is YAML file contents as Aeson
-- answer tuple as (cnt iterations,[(random range max,float matrix for (*!), float vector for +)])
readParams :: String -> Value -> (Int,[(Float,M22 Float,V2 Float)])
readParams target config = (cnt,params)
  where
    cnt = lookupConfig (target <> ".cntPoints") config
    rawParams = lookupConfig (target <> ".params") config
    params = rawParams2Param <$> rawParams
    rawParams2Param :: AFTup -> (Float,M22 Float,V2 Float)
    rawParams2Param (r,((r1,r2),(r3,r4)),(r5,r6)) = (r,V2 (V2 r1 r2) (V2 r3 r4),V2 r5 r6)

nextXY :: [(Float,M22 Float,V2 Float)] -> V2 Float -> Float -> (V2 Float,V2 Float)
nextXY ps v r =
  case find (\(x,_,_) -> r < x) ps of   -- lookup row in ps by r (0..1) < first column
    Just (_,m,v') -> dupe $ v *! m + v' -- affine xform: V2 Float (*!) M22 Float + V2 Float -> V2 Float
    Nothing -> error $ "nextXY no such " <> show r <> " in " <> show ps -- random ranges in first column are bad?

-- see https://github.com/timbod7/haskell-chart/wiki/example-2
chart :: (PlotValue x, PlotValue y) => String -> [(x, y)] -> Renderable ()
chart title coords = toRenderable layout
  where
    plot = plot_points_style .~ filledCircles 1 (opaque green)
           $ plot_points_values .~ coords
           $ def
    layout = layout_title .~ title
           $ layout_plots .~ [toPlot plot]
           $ def

-- Parsing: data types and aggregate combinators
lexeme :: PS.Parser a -> PS.Parser a
lexeme p = spaces *> p

pPr :: PS.Parser a -> PS.Parser b -> PS.Parser (a,b)
pPr pFst pSnd = between (char '(') (char ')') ((,) <$> pFst <*> (char ',' *> pSnd))

pTup :: PS.Parser a -> PS.Parser b -> PS.Parser c -> PS.Parser (a,b,c)
pTup pFst pSnd pThrd = between (char '(') (char ')') ((,,) <$> pFst <*> (char ',' *> pSnd) <*> (char ',' *> pThrd))

mkPs :: PS.Parser a -> PS.Parser [a]
mkPs p = between (char '(') (char ')') (lexeme p `sepBy1` char ',')

-- WTF, Text.Parsec.Number 'floating' doesn't do negative floats?
-- See https://mail.haskell.org/pipermail/haskell-cafe/2002-August/003280.html
data Sign      = Positive | Negative

applySign          :: Num a => Sign -> a -> a
applySign Positive =  id
applySign Negative =  negate
               
sign  :: PS.Parser Sign
sign  =  do { void (char '-')
            ; return Negative
            }
     <|> do { void (char '+')
            ; return Positive
            }
     <|> return Positive

myFloating :: PS.Parser Float
myFloating =  do { s   <- sign
                 ; fl  <- floating
                 ; pure (applySign s fl)
                 }

pFloatPr :: PS.Parser (Float,Float)
pFloatPr = pPr myFloating myFloating

pFloatPrPr :: PS.Parser ((Float,Float),(Float,Float))
pFloatPrPr = pPr pFloatPr pFloatPr

type AFTup = (Float,((Float,Float),(Float,Float)),(Float,Float))

pAFTup :: PS.Parser AFTup
pAFTup = pTup myFloating pFloatPrPr pFloatPr

pAFTups :: PS.Parser [(Float,((Float,Float),(Float,Float)),(Float,Float))]
pAFTups = mkPs pAFTup

class FromConfig a where
  -- | Convert a config string to a Haskell value
  parseConfig :: String -> a

mkParseConfig :: PS.Parser a -> String -> a
mkParseConfig parser input  = either (error . show) id . parse parser input $ input

instance FromConfig Int where
  parseConfig = mkParseConfig (string "int" *> spaces *> int)
  
instance FromConfig [(Float,((Float,Float),(Float,Float)),(Float,Float))] where
  parseConfig = mkParseConfig (mkPs pAFTup)

lookupConfig :: FromConfig a => String -> Value -> a
lookupConfig path config =
  let segments = splitOn "." path
  in case preview (foldl1 (.) (map (key . fromString) segments) . _String) config of
    Nothing -> error $
               "Could not find value for path: " <>
               path <> "\nin values:\n" <>
               show config
    Just txt -> parseConfig (unpack txt)
