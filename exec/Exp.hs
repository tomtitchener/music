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
import Data.List.Utils
import qualified Data.Map.Strict as M
import Data.Tuple.Extra (both)
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

name2Cfg2ScoreFuns ::[(String,String -> String -> Driver ())]
name2Cfg2ScoreFuns = [("txt",cfg2TxtExpVoiceScore)
                     ,("pulse",cfg2PulseExpVoiceScore)]

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
  let cfg2ScoreFun = snd $ fromMaybe (error $ "no match for " <> _optTarget) $ find ((flip startswith) _optTarget . fst) name2Cfg2ScoreFuns
  void . liftIO $ runReaderT (runDriver (cfg2ScoreFun _optTarget (show gen))) (initEnv config (show gen))


-- pairs for keyboard voices: (left-hand,right-hand)
data ConfigData =
  ConfigData {
  _cfgSustainDynamic        :: Dynamic
  ,_cfgSustainNotes         :: ([NoteDurOrNoteDurTup],[NoteDurOrNoteDurTup]) -- same durations!
  ,_cfgSustainXPosePitches  :: [(PitOct,PitOct)]
  ,_cfgStaccatoDynamic      :: Dynamic
  ,_cfgStaccatoPitches      :: ([PitOctOrPitOcts],[PitOctOrPitOcts])
  ,_cfgStaccatoRhythms      :: ([RestDurValOrNoteDurVal],[RestDurValOrNoteDurVal])
  ,_cfgStaccatoXPosePitches :: [(PitOct,PitOct)]
  }
  deriving Show

prefix2ConfigData :: String -> Driver ConfigData
prefix2ConfigData pre =
  ConfigData
  <$> (searchConfigParam (pre <> ".sustain.dyn"))
  <*> (searchConfigParam (pre <> ".sustain.notesPr")       <&> both (map nDOrNDTup2Arrs . NE.toList))
  <*> (searchConfigParam (pre <> ".sustain.startPitOcts")  <&> NE.toList)
  <*> (searchConfigParam (pre <> ".staccato.dyn"))
  <*> (searchConfigParam (pre <> ".staccato.notesPr")      <&> both (map pOOrNEPOs2pOOrPOs . NE.toList))
  <*> (searchConfigParam (pre <> ".staccato.rhythmsPr")    <&> both NE.toList)
  <*> (searchConfigParam (pre <> ".staccato.startPitOcts") <&> NE.toList)

cfgInfo2Voice :: String -> Dynamic -> ([NoteDurOrNoteDurTup],[NoteDurOrNoteDurTup]) -> [(PitOct,PitOct)] -> Driver Voice
cfgInfo2Voice pre dyn notes pitOctPrs = do
  keySig <- searchConfigParam (pre <> ".common.key")
  scale  <- searchMConfigParam (pre <> ".common.scale") <&> fromMaybe (keySig2Scale M.! keySig)
  instr  <- searchConfigParam  (pre <> ".common.instr")
  pure $ KeyboardVoice instr (neVEsPr scale)
  where
    f1 :: Scale -> (PitOct,PitOct) -> ([NoteDurOrNoteDurTup],[NoteDurOrNoteDurTup])
    f1 scale poPr = both (xposeFromNoteDurOrNoteDurTups scale) poPr <<*>> notes
    f2 :: [([NoteDurOrNoteDurTup],[NoteDurOrNoteDurTup])] -> (NE.NonEmpty VoiceEvent,NE.NonEmpty VoiceEvent)
    f2  = both (NE.fromList . tagFirstSoundDynamic dyn . map nDOrNDTup2VE . concat) . unzip
    neVEsPr scale = f2 (f1 scale <$> pitOctPrs)

-- Expects top-level keys for tempo, time signature and key signature, assuming all voices are the same.
cfg2TxtExpVoiceScore :: String -> String -> Driver ()
cfg2TxtExpVoiceScore pre gen = do
  keySig  <- searchConfigParam (pre <> ".common.key") -- TBD: per-voice?
  tempo   <- searchConfigParam (pre <> ".common.tempo")
  timeSig <- searchConfigParam (pre <> ".common.time")
  ConfigData{..} <- prefix2ConfigData pre
  let
    tagVoice = tagAVoiceEvent (VeTempo tempo) . tagVoiceEvent (VeTimeSignature timeSig) . tagVoiceEvent (VeKeySignature keySig)
    -- looking for pair with max lengths for p air of resulting (treble,bass) of ([NoteDurOrNoteDurTup],[NoteDurOrNoteDurTup])
    -- numNotesPr = both (\ps rs = max (length ps) (length rs))  _cfgStaccatoPitches <<*>> _cfgStaccatoRhythms
    -- though really should be taking something like sum of durations to equal other voice?
    numNotesPr = both (curry (uncurry max . bimap length length)) _cfgStaccatoPitches <<*>> _cfgStaccatoRhythms
    -- tbd: take while length is less than length of sust voice
    -- problem here is _cfgStaccatoPitches is actually pair ([PitOctOrPitOcts],[PitOctOrPitOcts]) and so is
    -- _cfgStaccatoRhythms [(RestDurValOrNoteDurVal,RestDurValOrNoteDurVal)], and what I need is a both of 
    -- this function across via biapplicative operator: <<*>>
    staccatoNotes = both (\numNotes pitches rhythms -> take numNotes . snd $ mapAccumL mapAccumF pitches rhythms) numNotesPr <<*>> (both cycle _cfgStaccatoPitches) <<*>> (both cycle _cfgStaccatoRhythms)
  sustVoice <- cfgInfo2Voice pre _cfgSustainDynamic _cfgSustainNotes _cfgSustainXPosePitches <&> tagVoice
  stacVoice <- cfgInfo2Voice pre _cfgStaccatoDynamic staccatoNotes _cfgStaccatoXPosePitches <&> tagVoice
  writeScore ("./" <> pre <> ".ly") $ Score pre gen (NE.fromList [sustVoice,stacVoice])
  where
    mapAccumF :: [PitOctOrPitOcts] -> RestDurValOrNoteDurVal -> ([PitOctOrPitOcts],NoteDurOrNoteDurTup)
    mapAccumF (poOrPO:poOrPOs) (Left durVal)  = ((poOrPO:poOrPOs),Left (Nothing,durVal,Staccatissimo))
    mapAccumF (poOrPO:poOrPOs) (Right durVal) = (poOrPOs,Left (Just poOrPO,durVal,Staccatissimo))
    mapAccumF x _ = error $ "cfg2ExpVoiceScore gen unexpected [PitOctOrPitOcts]: " <> show (take 10 x)


cfg2PulseExpVoiceScore :: String -> String -> Driver ()
cfg2PulseExpVoiceScore _ _ = pure ()
