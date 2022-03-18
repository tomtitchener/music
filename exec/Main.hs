{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Compose
import ComposeData
import Types

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
    then either (error . Y.prettyPrintParseException) identity <$> Y.decodeFileEither _optConfigYaml
    else error $ "config file " <> _optConfigYaml <> " does not exist"
  unless (null _optRandomSeed) $ do
    case readMaybe _optRandomSeed::Maybe SMGen of
      Nothing -> error $ "failed to parse random seed " <> _optRandomSeed
      Just smGen -> do
        let stdGen = StdGen { unStdGen = smGen }
        setStdGen stdGen
  gen <- getStdGen
  void . liftIO $ runReaderT (runDriver (cfg2Score _optTarget (show gen))) (initEnv config (show gen))

cfg2Score :: String -> String -> Driver ()
cfg2Score title gen = do
  tempo     <- searchConfigParam  (title <> ".common.tempo")
  timeSig   <- searchConfigParam  (title <> ".common.time")
  keySig    <- searchConfigParam  (title <> ".common.key")
  instr     <- searchConfigParam  (title <> ".common.instr") -- TBD: this forces an ensemble of identical instruments only
  grpNames   <- cfgPath2Keys ("group" `isPrefixOf`) title <&> fmap ((title <> ".") <>)
  grpScnsPrs <- traverse (secondM (cfgPath2Keys ("section" `isPrefixOf`)) . dupe) grpNames
  grpCfgs    <- traverse (uncurry cfg2GroupConfig) (second NE.fromList <$> grpScnsPrs)
  vesss      <- traverse groupConfig2VoiceEvents grpCfgs <&> concat
  let vess = concat <$> transpose vesss
      voices = pipeline tempo timeSig keySig instr vess
  writeScore ("./" <> title <> ".ly") $ Score title gen (NE.fromList voices)
  where
    pipeline :: Tempo -> TimeSignature -> KeySignature -> Instrument -> [[VoiceEvent]] -> [Voice]
    pipeline tempo timeSig keySig instr vess = --
      zipWith alignVoiceEventsDurations timeSigs vess            -- -> [[VoiceEvent]]
      & zipWith3 (mkVesTotDur (maximum veLens)) veLens timeSigs  -- -> [[VoiceEvent]]
      & zipWith4 genSplitStaffVoc instrs keySigs timeSigs        -- -> [Voice]
      & tagTempo tempo                                           -- -> [Voice]
      where
        cntVoices  = length vess
        veLens     = ves2DurVal <$> vess
        timeSigs   = replicate cntVoices timeSig
        keySigs    = replicate cntVoices keySig
        instrs     = replicate cntVoices instr

