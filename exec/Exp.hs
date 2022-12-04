{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RecordWildCards     #-}

-- Gen(erate) lilypond output given the name of a config YAML
-- file and top-level target.
-- Optionally, specify a seed for the random number generator.
-- 
-- Exp.hs for experimental, a trial to create an interactive
-- program to speed trial and error.
--
-- To start:  focus on the simplest example of rapid prototyping.
-- The goal is a small amount of configuration data that generates 
-- a multi-voice texture.
--
--
-- Make config data from YAML file minimal, like a list of
-- PitOctOrPitOcts to be mapped e.g. via transposition, a
-- list of PitOct to drive the mapping plus a PitOct to
-- start and maybe a count of voices.
--
-- TBD:
-- 
-- Explore features like multiple voices e.g. with sostenuto 
-- pedal followed by shorter voices and keyboard voice with
-- explicit treble and bass staff.
--
-- Generate progressions algorithmically, starting with
-- successive steps through more deeply nested recursive
-- paths over the same progression.
--
-- Consider multiple voices on one staff.  
-- 
--

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Biapplicative 
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
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
import Utils

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
  void . liftIO $ runReaderT (runDriver (cfg2ExpVoiceScore _optTarget (show gen))) (initEnv config (show gen))

-- First pass:  transpose (treble,bass) pairs in _cfgNDurOrNDurTupsPr via (treble,bass) pairs in _cfgStartPitOcts
data ConfigData =
  ConfigData {
  _cfgNDurOrNDurTupsPr :: ([NoteDurOrNoteDurTup],[NoteDurOrNoteDurTup]) -- same durations?
  ,_cfgStartPitOcts    :: [(PitOct,PitOct)]
  }
  deriving Show

cfgValue2ConfigData :: String -> Driver ConfigData
cfgValue2ConfigData pre =
  ConfigData
  <$> (searchConfigParam (pre <> ".nDurOrNDurTupsPr") <&> bimap ndOrNDNETups2ndOrNDTups ndOrNDNETups2ndOrNDTups
  <*> (searchConfigParam (pre <> ".startPitOcts") <&> NE.toList)
  where
    ndOrNDNETups2ndOrNDTups = map nDOrNDTup2Arrs . NE.toList

cfgInfo2Voice :: String -> ConfigData -> PitOct -> Driver Voice
cfgInfo2Voice pre ConfigData{..} pitOct = do
  keySig <- searchConfigParam  (pre <> ".common.key")
  scale  <- searchMConfigParam (pre <> ".common.scale") <&> fromMaybe (keySig2Scale M.! keySig)
  instr  <- searchConfigParam  (pre <> ".common.instr")
  dyn    <- searchConfigParam  (pre <> ".common.dyn")
  pure KeyboardVoice instr (bimap (y dyn) (y dyn) (mkPr scale))
  where
    y d = NE.fromList . tagFirstSoundDynamic d
    mkPr s  = map (\pitOctsPr -> bimap (f s) (f s) pitOctsPr <<*>> _cfgNDOrNDurTupsPr) _cfgStartPitOcts
    f s nDOrNDTups pitOcts = xposeFromNoteDurOrNoteDurTups s pitOcts nDOrNDTups
  -- KeyboardVoice has (NonEmpty VoiceEvent,NonEmpty VoiceEvent) for treble, bass
  -- _cfgNDurOrNDurTupsPr has ([NoteDurOrNoteDurTup],[NoteDurOrNoteDurTup]) to map via [(PitOct,PitOct)]
  -- to ([VoiceEvent],[VoiceEvent]) via xposeFromNoteDurOrNoteDurTupss.
  -- Individual step is bimap of Scale -> PitOct [[NoteDurOrNoteDurTup]] -> [[NoteDurOrNoteDurTup]]
  -- that's stripped down to [NoteDurOrNoteDurTup] (or else write xposeFromNoteDurOrNoteDurTups derived
  -- from xposeFromNoteDurOrNoteDurTupss), is concatenated to single [NoteDurOrNoteDurTup] and mapped
  -- to VoiceEvent visa nDOrNDTup2VE and than passed to NE.fromList
  --
  -- fmap across _cfgStartPitOcts so individual input is (PitOct,PitOct), context is ([NoteDurOrNoteDurTup],[NoteDurOrNoteDurTup])
  -- what to do with multiple (,) inputs?  Outer bimap should be over (PitOct,PitOct), meaning inner function starts with PitOct.
  -- Ugh, then I want association of head of ([NoteDurOrNoteDurTup],[NoteDurOrNoteDurTup]), which can't be an inner bimap.
  -- What I have as inputs is two pairs, (PitOct,PitOct) -> ([NoteDurOrNoteDurTup],[NoteDurOrNoteDurTup]) -> ([VoiceEvent],[VoiceEvent])
  -- that I can accumulate to ([[VoiceEvent]],[[VoiceEvent]]) and then concat back to ([VoiceEvent],[VoiceEvent]), including tagging with dyn.
  -- So I have to pairs as inputs that maybe I just want to dissect as (po1,po2) and etc.?  Then I can have a common routine that gives
  -- [VoiceEvent] output
  --
  -- Aha, but what I really want is <<*>> from Data.Biapplicative, which is going to look like
  -- bimap f f (PitOct,PitOct) <<*>> ([NoteDurOrNoteDurTup],[NoteDurOrNoteDurTup]) where
  -- where
  --   f = fmap NDOrNDTup2VE . xposeFromNoteDurOrNoteDurTups s
  -- f :: Scale -> PitOct -> [NoteDurOrNoteDurTup] -> [VoiceEvent]
  -- f = map nDOrNDTup2VE . xposeFromNoteDurOrNoteDurTups
  -- and that's inside a map over [(PitOct,PitOct)] yielding a [([VoiceEvent],[VoiceEvent])] that gets
  -- concatenated to be ([VoiceEvent],[VoiceEvent]) where each gets tagged with dyn.
  -- 
  -- pure $ KeyboardVoice instr (NE.fromList . tagFirstSoundDynamic dyn $ nDOrNDTup2VE <$> ndurOrNDurTups scale)
  -- where
  --   ndurOrNDurTups scale = head $ xposeFromNoteDurOrNoteDurTupss scale pitOct [_cfgNDurOrNDurTupsPr]
  
cfgData2Voices :: String -> Tempo -> TimeSignature -> ConfigData -> Driver [Voice]
cfgData2Voices pre tempo timeSig cfgData@ConfigData{..} = 
  traverse (cfgInfo2Voice pre cfgData) _cfgStartPitOcts <&> groupAndTrimDursAndAddTempo tempo timeSig 

-- Expects top-level keys for tempo, time signature and key signature, assuming all voices are the same.
cfg2ExpVoiceScore :: String -> String -> Driver ()
cfg2ExpVoiceScore pre gen = do
  tempo   <- searchConfigParam (pre <> ".common.tempo")
  timeSig <- searchConfigParam (pre <> ".common.time")
  voices <- cfgValue2ConfigData pre >>= cfgData2Voices pre tempo timeSig <&> groupAndTrimDursAndAddTempo tempo timeSig 
  writeScore ("./" <> pre <> ".ly") $ Score pre gen (NE.fromList voices)
