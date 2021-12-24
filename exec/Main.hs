{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RecordWildCards     #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Tuple.Extra (dupe, second, secondM)
import Data.List (zipWith3, zipWith4)
import qualified Data.List.NonEmpty as NE
import qualified Data.Yaml as Y
import Options.Applicative
import Prelude (String, error, show)
import Protolude hiding (option, print, show, to, second)
import System.Directory (doesFileExist)
import System.Random
import System.Random.Internal
import System.Random.SplitMix

import Driver
import Types
import Compose
import ComposeData

-- _optRandomSeed via command-line argument  -s "<string>"
-- to recreate pseudo-random number generator by copying
-- from LilyPond comment, e.g.:
--
-- % "StdGen {unStdGen = SMGen 11888972784562141867 7849352481482538343}"
--
-- e.g.:
-- $ stack exec music -- -s "SMGen 11888972784562141867 7849352481482538343"

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
            (header "music")

main :: IO ()
main =  do
  Options{..} <- execParser opts
  config <- do
    e <- doesFileExist _optConfigYaml
    if e
    then either (error . show) identity <$> Y.decodeFileEither _optConfigYaml
    else error $ "config file " <> _optConfigYaml <> " does not exist"
  unless (null _optRandomSeed) $ do
    case readMaybe _optRandomSeed::Maybe SMGen of
      Nothing -> error $ "failed to parse random seed " <> _optRandomSeed
      Just smGen -> do
        let stdGen = StdGen { unStdGen = smGen }
        setStdGen stdGen
  gen <- getStdGen
  void . liftIO $ runReaderT (runDriver (cfg2Score _optTarget (show gen))) (initEnv config (show gen))

changeClefs :: Int
changeClefs = 5

cfg2Score :: String -> String -> Driver ()
cfg2Score title gen = do
  chgClfInt <- searchMConfigParam (title <> ".common.changeclefs") <&> fromMaybe changeClefs
  tempoInt  <- searchConfigParam  (title <> ".common.tempo")
  timeSig   <- searchConfigParam  (title <> ".common.time")
  keySig    <- searchConfigParam  (title <> ".common.key")
  instr     <- searchConfigParam  (title <> ".common.instr")
  secNames  <- cfgPath2Keys ("section" `isPrefixOf`) title <&> fmap ((title <> ".") <>)
  secVcsPrs <- traverse (secondM (cfgPath2Keys ("voice" `isPrefixOf`)) . dupe) secNames
  secCfgs   <- traverse (uncurry cfg2SectionConfig) (second NE.fromList <$> secVcsPrs)
  nOrRsss   <- traverse sectionConfig2NoteOrRests secCfgs
  let nOrRss    = concat <$> transpose nOrRsss
      voices    = pipeline chgClfInt tempoInt timeSig keySig instr nOrRss
  writeScore ("./" <> title <> ".ly") $ Score title gen voices
  where
    pipeline :: Int -> Int -> TimeSignature -> KeySignature -> Instrument -> [[NoteOrRest]] -> NE.NonEmpty Voice
    pipeline chgClfs tempoInt timeSig keySig instr nOrRss = --
      zipWith alignNoteOrRestsDurations timeSigs nOrRss            -- -> [[NoteOrRest]]
      & zipWith splitNoteOrRests winLens                           -- -> [([VoiceEvent],[VoiceEvent])]
      & zipWith3 (mkVesPrTotDur (maximum veLens)) veLens timeSigs  -- -> [([VoiceEvent],[VoiceEvent])]
      & fmap (bimap NE.fromList NE.fromList)                       -- -> [(NonEmpty VoiceEvent,NonEmpty VoiceEvent)]
      & NE.fromList . zipWith4 genPolyVocs instrs keySigs timeSigs -- -> NonEmpty Voice
      & tagTempo tempo                                             -- -> NonEmpty Voice
      where
        tempo      = TempoDur QDur (fromIntegral tempoInt)
        cntVoices  = length nOrRss
        veLens     = nOrRs2DurVal <$> nOrRss
        timeSigs   = replicate cntVoices timeSig
        keySigs    = replicate cntVoices keySig
        instrs     = replicate cntVoices instr
        winLens    = replicate cntVoices chgClfs
  
