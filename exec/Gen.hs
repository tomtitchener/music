{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

cfg2MonoVoiceScore :: String -> String -> Driver ()
cfg2MonoVoiceScore path gen = do
  tempo   <- searchConfigParam (title <> ".common.tempo")
  timeSig <- searchConfigParam (title <> ".common.time")
  keySig  <- searchConfigParam  (title <> ".common.key")
  instr   <- searchConfigParam (title <> ".common.instr")
  voices  <- config2Voices path timeSig keySig instr <&> pipeline tempo timeSig 
  writeScore ("./" <> path <> ".ly") $ Score path gen (NE.fromList voices)
  where
    title :: String = head (splitOn "." path)
    pipeline :: Tempo -> TimeSignature -> [Voice] -> [Voice]
    pipeline tempo timeSig voices =
       fmap (alignVoiceDurations timeSig) voices
       & zipWith (mkVoiceTotDur timeSig (maximum voiceLens)) voiceLens
       & tagTempo tempo
      where
        voiceLens = voice2DurVal <$> voices
    
    -- -- 2) the signature changes here from -> [[VoiceEvent]] -> [Voice] to [Voice] -> [Voice]
    -- pipeline :: Tempo -> TimeSignature -> KeySignature -> Instrument -> [[VoiceEvent]] -> [Voice]
    -- pipeline tempo timeSig keySig instr vess =
    --   -- 2a) alignVoiceEventsDurations takes and answers a Voice instead of a [VoiceEvent]
    --   fmap (alignVoiceEventsDurations timeSig) vess            -- -> [[VoiceEvent]]
    --   -- 2b) mkVesToTDur ditto
    --   & zipWith (mkVesTotDur timeSig (maximum veLens)) veLens  -- -> [[VoiceEvent]]
    --   -- 2c) eliminate this because we already have a [Voice]
    --   & fmap (genSplitStaffVoc instr keySig timeSig)           -- -> [Voice]
    --   & tagTempo tempo                                         -- -> [Voice]
    --   where
    --     -- 3) voice2DurVal replaces ves2DurVal and voices replaces vess
    --     veLens = ves2DurVal <$> vess
