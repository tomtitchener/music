{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Aeson hiding (Options)
import qualified Data.Yaml as Y
import Options.Applicative
import Protolude hiding (print)
import System.Directory (doesFileExist)

import Driver
import Types

data Options = Options
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
  void . liftIO $ execStateT (runReaderT (runDriver exEnv) (initEnv config)) initState

exAction :: Driver ()
exAction = printLily (Note C COct QDur Accent Forte False)

exComb :: Driver ()
exComb = randomizeList [C,D,E,F,G,A,B] >>= mapM_ printLily

exEnv :: Driver ()
exEnv = getConfigParam "example_param.pitch_list" >>= print
