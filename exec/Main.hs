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

cfg2Int :: String -> Driver Int
cfg2Int k = do
  (SelInt i) <- getConfigParam k
  pure i

type VoiceTup = (Instrument, KeySignature, Clef, [Pitch], (Pitch,Octave))

cfg2VocTup :: String -> Driver VoiceTup
cfg2VocTup pre = do
  (SelInstrument i) <- getConfigParam (pre <> ".instr")
  (SelKey k) <- getConfigParam (pre <> ".key")
  (SelClef c) <- getConfigParam (pre <> ".clef")
  (SelPitOctPr po) <- getConfigParam (pre <> ".start")
  (SelPitches s) <- getConfigParam (pre <> ".scale")
  pure (i, k, c, s, po)

cfg2VocTups :: String -> [String] -> Driver [VoiceTup]
cfg2VocTups root = mapM (\v -> cfg2VocTup (root <> "." <> v))

cfg2IntMottos :: String -> Driver [[Maybe Int]]
cfg2IntMottos pre = do
  (SelMIntervalss mIMs) <- getConfigParam (pre <> ".intss")
  pure mIMs

cfg2Durss :: String -> Driver [[Duration]]
cfg2Durss pre = do
  (SelDurationss durss) <- getConfigParam (pre <> ".durss")
  pure durss

cfg2Accss :: String -> Driver [[Accent]]
cfg2Accss pre = do
  (SelAccentss accss) <- getConfigParam (pre <> ".accss")
  pure accss

cfg2Dynss :: String -> Driver [[Dynamic]]
cfg2Dynss pre = do
  (SelDynamicss dynss) <- getConfigParam (pre <> ".dynss")
  pure dynss

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
  mottos <- cfg2IntMottos title
  durss <- cfg2Durss title
  accss <- cfg2Accss title
  dynss <- cfg2Dynss title
  reps  <- cfg2Int (title <> ".reps")
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

printConfigParam :: String -> Driver ()
printConfigParam sel = getConfigParam ("example_param." <> sel) >>= print

printConfigParams :: Driver ()
printConfigParams = mapM_ printConfigParam ["pits","accs","accss","dyns","dynss","durs","durss","ints","intss","pitoct","pitocts","instrument"]

