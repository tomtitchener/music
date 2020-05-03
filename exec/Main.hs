{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Aeson hiding (Options)
import qualified Data.Yaml as Y
import Options.Applicative
import Protolude hiding (print, to)
import System.Directory (doesFileExist)
import System.Random

import Driver
import Types

data Options = Options
  { _optConfigYaml :: FilePath
  , _optRandomSeed :: [Char]
  } deriving Show

options :: Parser Options
options = Options
  <$> strOption (short 'c' <> metavar "CONFIG_FILE"
                 <> value "config.yml"
                 <> help "Default: config.yml, if available")
  <*> strOption (short 's' <> metavar "RANDOM_SEED"
                 <> help "Seed string for random generator")

opts :: ParserInfo Options
opts = info (helper <*> options)
            (header "music")

main :: IO ()
main =  do
  Options{..} <- execParser opts
  stdGen <- getStdGen
  config <- do
    e <- doesFileExist _optConfigYaml
    if e
    then either (panic.show) identity <$> Y.decodeFileEither _optConfigYaml
    else pure Null
  void . liftIO $ execStateT (runReaderT (runDriver exEnv) (initEnv config)) (initState _optRandomSeed stdGen)

exAction :: Driver ()
exAction = writeLily "example.ly" (Note C COct QDur Accent Forte False)

exRandList :: Driver ()
exRandList = randomizeList [C,D,E,F,G,A,B] >>= mapM_ print

exRandElem :: Driver ()
exRandElem = randomElement [C,D,E,F,G,A,B] >>= print

exRandElems :: Int -> Driver ()
exRandElems n = randomElements [C,D,E,F,G,A,B] >>= print . take n

printConfigParam :: [Char] -> Driver ()
printConfigParam sel = getConfigParam ("example_param." <> sel) >>= print

exEnv :: Driver ()
exEnv = mapM_ printConfigParam ["pits","accs","accss","dyns","dynss","durs","durss","ints","intss","pitoct","pitocts","instrument"]
