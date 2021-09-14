{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Data.Yaml as Y
import Options.Applicative
import Prelude (String, error, show)
import Protolude hiding (option, print, show, to)
import System.Directory (doesFileExist)
import System.Random
import System.Random.Internal
import System.Random.SplitMix

import Compose
import Driver
import Types

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
  void . liftIO $ runReaderT (runDriver (cfg2Driver _optTarget)) (initEnv config (show gen))

---------
-- Test -
---------

exRandList :: Driver ()
exRandList = randomizeList [C,D,E,F,G,A,B] >>= mapM_ print

exRandElem :: Driver ()
exRandElem = randomElement [C,D,E,F,G,A,B] >>= print

exRandElems :: Int -> Driver ()
exRandElems n = randomElements [C,D,E,F,G,A,B] >>= print . take n
