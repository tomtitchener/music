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

import Driver
import Types

newtype Options = Options
  { _optConfigYaml   :: FilePath
  } deriving Show

options :: Parser Options
options = Options
  <$> strOption (short 'c' <> metavar "CONFIG_FILE"
                 <> value "config.yml"
                 <> help "Default: config.yml, if available")

opts :: ParserInfo Options
opts = info (helper <*> options)
            (header "music")

main :: IO ()
main =  do
  Options{..} <- execParser opts
  config <- do
    e <- doesFileExist _optConfigYaml
    if e
    then either (panic.show) identity <$> Y.decodeFileEither _optConfigYaml
    else pure Null
  void . liftIO $ execStateT (runReaderT (runDriver $ exRandElems 20) (initEnv config)) initState

exAction :: Driver ()
exAction = writeLily "example.ly" (Note C COct QDur Accent Forte False)

exRandList :: Driver ()
exRandList = randomizeList [C,D,E,F,G,A,B] >>= mapM_ print

exRandElem :: Driver ()
exRandElem = randomElement [C,D,E,F,G,A,B] >>= print

exRandElems :: Int -> Driver ()
exRandElems n = randomElements [C,D,E,F,G,A,B] >>= print . take n

exEnv :: Driver ()
exEnv = getConfigParam "example_param.durs" >>= print

{--
exEnv :: Driver ()
exEnv = do
  pitches::[Pitch] <- getConfigData "example_param.pits"
  mapM_ print pitches
--}
