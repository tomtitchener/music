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
import qualified Prelude as P (read, show)
import Protolude hiding (print, to)
import System.Directory (doesFileExist)
import System.Random

import Driver
import Types
import Utils

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
    then either (panic.show) identity <$> Y.decodeFileEither _optConfigYaml
    else pure Null
  unless (null _optRandomSeed) $ do
    setStdGen (P.read _optRandomSeed::StdGen)
  gen <- getStdGen
  void . liftIO $ runReaderT (runDriver (cfg2Score "example_texture")) (initEnv config (P.show gen))

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

cfg2VocTup :: [Char] -> Driver (Instrument, Clef, [Pitch], (Pitch,Octave))
cfg2VocTup pre = do
  (SelInstrument i) <- getConfigParam (pre <> ".instr")
  (SelClef c) <- getConfigParam (pre <> ".clef")
  (SelPitOctPr po) <- getConfigParam (pre <> ".start")
  (SelPitches s) <- getConfigParam (pre <> ".scale")
  pure (i, c, s, po)

cfg2VocTups :: [Char] -> [[Char]] -> Driver [(Instrument, Clef, [Pitch], (Pitch,Octave))]
cfg2VocTups root vocs = mapM (\v -> cfg2VocTup (root <> "." <> v)) vocs

cfg2IntMottos :: [Char] -> Driver [[Maybe Int]]
cfg2IntMottos pre = do
  (SelMIntervalss mIMs) <- getConfigParam (pre <> ".intss")
  pure mIMs

cfg2Durss :: [Char] -> Driver [[Duration]]
cfg2Durss pre = do
  (SelDurationss durss) <- getConfigParam (pre <> ".durss")
  pure durss

cfg2Accss :: [Char] -> Driver [[Accent]]
cfg2Accss pre = do
  (SelAccentss accss) <- getConfigParam (pre <> ".accss")
  pure accss

cfg2Dynss :: [Char] -> Driver [[Dynamic]]
cfg2Dynss pre = do
  (SelDynamicss dynss) <- getConfigParam (pre <> ".dynss")
  pure dynss

genVoc :: (Instrument, Clef, [Pitch], (Pitch,Octave)) -> [Maybe Int] -> [Duration] -> [Accent] -> [Dynamic] -> Voice
genVoc (instr, clef, scale, (p,o)) motto durs accs dns = SingleVoice instr (VeClef clef:genNotes (mtranspose scale motto (p,o)) durs accs dns)

genVocs :: [(Instrument, Clef, [Pitch], (Pitch,Octave))] -> [[Maybe Int]] -> [[Duration]] -> [[Accent]] -> [[Dynamic]] -> [Voice]
genVocs tups mottos durss accss dynss = genVoc <$> tups <*> mottos <*> durss <*> accss <*> dynss

genScore :: [Char] -> [(Instrument, Clef, [Pitch], (Pitch,Octave))] -> [[Maybe Int]] -> [[Duration]] -> [[Accent]] -> [[Dynamic]] -> Score
genScore title tups mottos durss accss dyns = Score title (genVocs tups mottos durss accss dyns)

cfg2Score :: [Char] -> Driver ()
cfg2Score title = do
  voctups <- cfg2VocTups title ["voice1","voice2","voice3","voice4"]
  mottos <- cfg2IntMottos title
  durss <- cfg2Durss title
  accss <- cfg2Accss title
  dynss <- cfg2Dynss title
  writeScore title $ genScore title voctups mottos durss accss dynss


