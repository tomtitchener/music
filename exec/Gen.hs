{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Gen(erate) lilypond output given the name of a config YAML
-- file and a top-level target or chain of up to four components
-- e.g. title.group.section.voice assuming that the YAML structure
-- is a list of GroupConfig from src/ConfigData.hs.  Optionally,
-- specify a seed for the random number generator.
-- Note: all voices must be mono voices only, e.g. each Voice
-- contains a single NE.NonEmpty VoiceEvent, e.g. PitchedVoice,
-- PercussionVoice (TBD), or SplitStaffVoice.

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Data.List.NonEmpty as NE
import Data.List.Split (splitOn)
import qualified Data.Yaml as Y
import Options.Applicative
import Prelude (String, error, show, head)
import Protolude hiding (print, show, to, second, head)
import System.Directory (doesFileExist)
import System.Random
import System.Random.Internal
import System.Random.SplitMix

import Driver
import Compose
import Types

-- _optRandomSeed via command-line argument  -s "<string>"
-- to recreate pseudo-random number generator by copying
-- from LilyPond comment, e.g.:
--
-- % "StdGen {unStdGen = SMGen 11888972784562141867 7849352481482538343}"
--
-- e.g.:
-- $ stack exec gen -- -s "SMGen 11888972784562141867 7849352481482538343"

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
            (header "gen")

main :: IO ()
main =  do
  Options{..} <- execParser opts
  config <- do
    e <- doesFileExist _optConfigYaml
    if e
    then either (error . Y.prettyPrintParseException) identity <$> Y.decodeFileEither _optConfigYaml
    else error $ "config file " <> _optConfigYaml <> " does not exist"
  unless (null _optRandomSeed) $ do
    case readMaybe _optRandomSeed::Maybe SMGen of
      Nothing -> error $ "failed to parse random seed " <> _optRandomSeed
      Just smGen -> do
        let stdGen = StdGen { unStdGen = smGen }
        setStdGen stdGen
  gen <- getStdGen
  void . liftIO $ runReaderT (runDriver (cfg2MonoVoiceScore _optTarget (show gen))) (initEnv config (show gen))

-- Expects top-level keys for tempo, time signature and key signature, assuming all voices are the same.
cfg2MonoVoiceScore :: String -> String -> Driver ()
cfg2MonoVoiceScore path gen = do
  tempo   <- searchConfigParam (title <> ".common.tempo")
  timeSig <- searchConfigParam (title <> ".common.time")
  keySig  <- searchConfigParam (title <> ".common.key")
  voices  <- config2MonoVoices path timeSig keySig <&> groupAndTrimDursAndAddTempo tempo timeSig 
  writeScore ("./" <> path <> ".ly") $ Score path gen (NE.fromList voices)
  where
    title = head (splitOn "." path)
