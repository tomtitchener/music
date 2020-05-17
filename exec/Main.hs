{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Aeson hiding (Options)
import qualified Data.Yaml as Y
import Options.Applicative
import Prelude (String, error, read, show)
import Protolude hiding (option, print, show, to)
import System.Directory (doesFileExist)
import System.Random

import Driver
import Types
import Utils

data Options = Options
  { _optConfigYaml :: FilePath
  , _optRandomSeed :: String
  } deriving Show

options :: Parser Options
options = Options
  <$> strOption (short 'c' <> metavar "CONFIG_FILE"
                 <> value "config.yml"
                 <> help "Default: config.yml, if available")
  <*> strOption (short 's' <> metavar "RANDOM_SEED"
                 <>  value ""
                 <> help "Seed string for random generator")

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
    else pure Null
  unless (null _optRandomSeed) $
    setStdGen (read _optRandomSeed::StdGen)
  gen <- getStdGen
  void . liftIO $ runReaderT (runDriver (cfg2Score "example_texture")) (initEnv config (show gen))

type VoiceTup = (Instrument, KeySignature, Clef, [Pitch], (Pitch,Octave))

cfg2VocTup :: String -> Driver VoiceTup
cfg2VocTup pre = do
  instr <- getConfigParam (pre <> ".instr")
  key <- getConfigParam (pre <> ".key")
  clef <- getConfigParam (pre <> ".clef")
  pitOctPr <- getConfigParam (pre <> ".start")
  scale <- getConfigParam (pre <> ".scale")
  pure (instr, key, clef, scale, pitOctPr)

cfg2VocTups :: String -> [String] -> Driver [VoiceTup]
cfg2VocTups root = mapM (\v -> cfg2VocTup (root <> "." <> v))

genVoc :: Int -> [[Maybe Int]] -> [[Duration]] -> [[Accent]] -> [[Dynamic]] -> VoiceTup -> Driver Voice
genVoc reps mottos durss accss dynss (instr, key, clef, scale, (p,o))= do
  mots <- concatMap (mtranspose scale (p,o)) . take reps <$> randomElements mottos
  durs <- concat . take reps <$> randomElements durss
  accs <- concat . take reps <$> randomElements accss
  dyns <- concat . take reps <$> randomElements dynss
  pure $ SingleVoice instr (VeKeySignature key:VeClef clef:genNotes mots durs accs dyns)

genVocs :: Int -> [[Maybe Int]] -> [[Duration]] -> [[Accent]] -> [[Dynamic]] -> [VoiceTup] -> Driver [Voice]
genVocs reps mottos durss accss dynss = mapM (genVoc reps mottos durss accss dynss)

cfg2Score :: String -> Driver ()
cfg2Score title = do
  voctups <- cfg2VocTups title ["voice1","voice2","voice3","voice4"]
  mottos <- getConfigParam (title <> ".intss")
  durss <- getConfigParam (title <> ".durss")
  accss <- getConfigParam (title <> ".accss")
  dynss <- getConfigParam (title <> ".dynss")
  reps  <- getConfigParam (title <> ".reps")
  voices <- genVocs reps mottos durss accss dynss voctups
  writeScore title $ Score title voices

---------
-- Test -
---------

exRandList :: Driver ()
exRandList = randomizeList [C,D,E,F,G,A,B] >>= mapM_ print

exRandElem :: Driver ()
exRandElem = randomElement [C,D,E,F,G,A,B] >>= print

exRandElems :: Int -> Driver ()
exRandElems n = randomElements [C,D,E,F,G,A,B] >>= print . take n

