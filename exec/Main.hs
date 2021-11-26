{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RecordWildCards     #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Data.Yaml as Y
import Data.List
import Options.Applicative
import Prelude (String, error, show)
import Protolude hiding (option, print, show, to)
import System.Directory (doesFileExist)
import System.Random
import System.Random.Internal
import System.Random.SplitMix

import Compose
import Driver

import Compositions.Swirls.Compose (cfg2SwirlsScore)

-- _optRandomSeed via command-line argument  -s "<string>"
-- to recreate pseudo-random number generator by copying
-- from LilyPond comment, e.g.:
--
-- % "no comment StdGen {unStdGen = SMGen 11888972784562141867 7849352481482538343}"
--
-- e.g.:
-- $ stack exec music -- -s "SMGen 11888972784562141867 7849352481482538343"

data Options = Options
  { _optConfigYaml :: FilePath
  , _optRandomSeed :: String
  , _optTarget     :: String
  } deriving Show

options :: Parser Options
options = Options
  <$> strOption (short 'c' <> metavar "CONFIG_FILE"
                 <> value "config.yml"
                 <> help "Default: config.yml, if available")
  <*> strOption (short 's' <> metavar "RANDOM_SEED"
                 <>  value ""
                 <> help "Seed string for random generator")
  <*> strOption (short 't' <> metavar "TARGET"
                 <>  value ""
                 <> help "Config file target")

opts :: ParserInfo Options
opts = info (helper <*> options)
            (header "music")

main :: IO ()
main =  do
  Options{..} <- execParser opts
  config <- do
    e <- doesFileExist _optConfigYaml
    if e
    then either (error . show) identity <$> Y.decodeFileEither _optConfigYaml
    else error $ "config file " <> _optConfigYaml <> " does not exist"
  unless (null _optRandomSeed) $ do
    case readMaybe _optRandomSeed::Maybe SMGen of
      Nothing -> error $ "failed to parse random seed " <> _optRandomSeed
      Just smGen -> do
        let stdGen = StdGen { unStdGen = smGen }
        setStdGen stdGen
  gen <- getStdGen
  void . liftIO $ runReaderT (runDriver (cfg2Score _optTarget)) (initEnv config (show gen))

cfg2Score :: String -> Driver ()
cfg2Score title = do
  scoreType <- searchConfigParam (title <> ".common.score")
  maybe (error $ "cfg2Score: no fun for title " <> title) ($ title) $ lookup scoreType driverFuns
  
driverFuns :: [(String,String -> Driver ())]
driverFuns =
  [("example_maxrand",   cfg2MaxRandScore)   -- to be deprecated
  ,("example_homophon",  cfg2HomoPhonScore)  -- to be deprecated
  ,("example_canon",     cfg2CanonScore)     -- to be deprecated
  ,("example_randmot",   cfg2RandMotScore)   -- to be deprecated
  ,("example_arpeggios", cfg2ArpeggiosScore) -- to be deprecated
  ,("swirls",            cfg2SwirlsScore)]

