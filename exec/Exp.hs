{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Data.List (foldl1)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Yaml as Y
import Options.Applicative
import Prelude (String, error, show)
import Protolude hiding (print, show, to, second, head, (<<*>>))
import System.Directory (doesFileExist)
import System.Random
import System.Random.Internal
import System.Random.SplitMix

import Driver
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

prefix2ConfigData :: String -> Driver ConfigData
prefix2ConfigData pre =
  ConfigData
  <$> (searchConfigParam (pre <> ".nDurOrNDurTupsPr") <&> bimap ndOrNDNETups2ndOrNDTups ndOrNDNETups2ndOrNDTups)
  <*> (searchConfigParam (pre <> ".startPitOcts") <&> NE.toList)
  where
    ndOrNDNETups2ndOrNDTups = map nDOrNDTup2Arrs . NE.toList

cfgInfo2Voice :: String -> ConfigData -> Driver Voice
cfgInfo2Voice pre ConfigData{..} = do
  keySig  <- searchConfigParam (pre <> ".common.key")
  scale  <- searchMConfigParam (pre <> ".common.scale") <&> fromMaybe (keySig2Scale M.! keySig)
  instr  <- searchConfigParam  (pre <> ".common.instr")
  dyn    <- searchConfigParam  (pre <> ".common.dyn")
  pure $ KeyboardVoice instr (neVEsPr scale dyn)
  where
    -- What I have:
    --  * Scale
    --  * Dynamic
    --  * ([NoteDurOrNoteDurTup],[NoteDurOrNoteDurTup])
    --  * [(PitOct,PitOct)]
    -- Outer function:
    --  of :: Dynamic -> Scale -> (PitOct,PitOct) -> ([NoteDurOrNoteDurTup],[NoteDurOrNoteDurTup]) -> (NonEmpty VoiceEvent,NonEmpty VoiceEvent)
    -- first step 
    f0 :: Scale -> ([NoteDurOrNoteDurTup],[NoteDurOrNoteDurTup]) -> [(PitOct,PitOct)] -> [([NoteDurOrNoteDurTup],[NoteDurOrNoteDurTup])]
    f0 scale ndONDTsPr poPrs = f1 scale ndONDTsPr <$> poPrs
    -- first step inner routine:
    f1 :: Scale -> ([NoteDurOrNoteDurTup],[NoteDurOrNoteDurTup]) -> (PitOct,PitOct) -> ([NoteDurOrNoteDurTup],[NoteDurOrNoteDurTup])
    f1 scale ndONDTsPr poPr = bimap (xposeFromNoteDurOrNoteDurTups scale) (xposeFromNoteDurOrNoteDurTups scale) poPr <<*>> ndONDTsPr
    -- First step: [([NoteDurOrNoteDurTup],[NoteDurOrNoteDurTup])] -> ([NoteDurOrNoteDurTup],[NoteDurOrNoteDurTup])
    --   which is  [([a],[a])] -> ([a],[a]) as foldl1 (\(a,b) (c,d) -> (a<>c,b<>d))
    f2 :: [([a],[a])] -> ([a],[a])
    -- f2 = foldl1 (\(a,b) (c,d) -> (a<>c,b<>d))
    f2 = foldl1 (\pr1 pr2 -> bimap (<>) (<>) pr1 <<*>> pr2)
    -- Second step:  ([NoteDurOrNoteDurTup],[NoteDurOrNoteDurTup]) -> ([VoiceEvent],[VoiceEvent])
    --   which is bimap (map nDOrNDTup2VE) (map nDOrNDTup2VE)
    f3 :: ([NoteDurOrNoteDurTup],[NoteDurOrNoteDurTup]) -> ([VoiceEvent],[VoiceEvent])
    f3 = bimap (map nDOrNDTup2VE) (map nDOrNDTup2VE)
    -- Fourth step:  Dynamic -> ([VoiceEvent],[VoiceEvent]) -> (NonEmpty VoiceEvent,NonEmpty VoiceEvent)
    --   which is bimap (NEfromList . tagFirstSoundDynamic d) (NEfromList . tagFirstSoundDynamic d)
    f4 :: Dynamic -> ([VoiceEvent],[VoiceEvent]) -> (NonEmpty VoiceEvent,NonEmpty VoiceEvent)
    f4 dyn = bimap (NE.fromList . tagFirstSoundDynamic dyn) (NE.fromList . tagFirstSoundDynamic dyn)
    --
    neVEsPr :: Scale -> Dynamic -> (NE.NonEmpty VoiceEvent,NE.NonEmpty VoiceEvent)
    neVEsPr scale dyn = neVesPr
      where
        ndOrNDTupPrs :: [([NoteDurOrNoteDurTup],[NoteDurOrNoteDurTup])] = f0 scale _cfgNDurOrNDurTupsPr _cfgStartPitOcts
        ndOrNDTupPr  ::  ([NoteDurOrNoteDurTup],[NoteDurOrNoteDurTup])  = f2 ndOrNDTupPrs
        vesPr        :: ([VoiceEvent],[VoiceEvent])                     = f3 ndOrNDTupPr
        neVesPr      :: (NonEmpty VoiceEvent,NonEmpty VoiceEvent)       = f4 dyn vesPr
        
    -- -- Procedure:
    -- --   * make separate functions and verify the types all work when they're pipelined
    -- --   * find places to combine via Biapplicative <<*>> and/or inner function composition
    -- y :: Dynamic -> ([VoiceEvent],[VoiceEvent]) -> (NE.NonEmpty VoiceEvent,NE.NonEmpty VoiceEvent)
    -- y d = NE.fromList . tagFirstSoundDynamic d . bimap nDOrNDTup2VE nDOrNDTup2VE 
    -- -- This yields [([NoteDurOrNoteDurTup],[NoteDurOrNoteDurTup])],
    -- -- Needs to be turned into ([NoteDurOrNoteDurTup],[NoteDurOrNoteDurTup]),
    -- -- maybe through foldl1 (\(a,b) (c,d) -> (a<>c,b<>d))
    -- mkPr :: Scale -> ([NoteDurOrNoteDurTup],[NoteDurOrNoteDurTup])
    -- mkPr s  = map (\pitOctsPr -> bimap (f s) (f s) pitOctsPr <<*>> _cfgNDurOrNDurTupsPr) _cfgStartPitOcts
    -- squashPrs :: [([a],[a])] -> ([a],[a])
    -- squashPrs = foldl1 (\(a,b) (c,d) -> (a<>c,b<>d))
    -- f :: Scale -> PitOct -> [NoteDurOrNoteDurTup] -> [NoteDurOrNoteDurTup]
    -- f s pitOcts nDOrNDTups = xposeFromNoteDurOrNoteDurTups s pitOct nDOrNDTups
    
  
-- Expects top-level keys for tempo, time signature and key signature, assuming all voices are the same.
cfg2ExpVoiceScore :: String -> String -> Driver ()
cfg2ExpVoiceScore pre gen = do
  keySig  <- searchConfigParam (pre <> ".common.key")
  timeSig <- searchConfigParam (pre <> ".common.time")
  voice <- (prefix2ConfigData pre >>= cfgInfo2Voice pre) <&> tagVoiceEvent (VeTimeSignature timeSig) . tagVoiceEvent (VeKeySignature keySig)
  writeScore ("./" <> pre <> ".ly") $ Score pre gen (NE.fromList [voice])
