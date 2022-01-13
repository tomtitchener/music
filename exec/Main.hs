{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RecordWildCards     #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Tuple.Extra (dupe, second, secondM)
import Data.List (zipWith3, zipWith4, last)
import qualified Data.List.NonEmpty as NE
import qualified Data.Yaml as Y
import Options.Applicative
import Prelude (String, error, show, head)
import Protolude hiding (option, print, show, to, second, head)
import System.Directory (doesFileExist)
import System.Random
import System.Random.Internal
import System.Random.SplitMix

import Driver
import Compose
import ComposeData
import Types
import Utils hiding (transpose)

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

cfg2Score :: String -> String -> Driver ()
cfg2Score title gen = do
  tempo     <- searchConfigParam  (title <> ".common.tempo")
  timeSig   <- searchConfigParam  (title <> ".common.time")
  keySig    <- searchConfigParam  (title <> ".common.key")
  instr     <- searchConfigParam  (title <> ".common.instr")
  secNames  <- cfgPath2Keys ("section" `isPrefixOf`) title <&> fmap ((title <> ".") <>)
  secVcsPrs <- traverse (secondM (cfgPath2Keys ("voice" `isPrefixOf`)) . dupe) secNames
  secCfgs   <- traverse (uncurry cfg2SectionConfig) (second NE.fromList <$> secVcsPrs)
  vesss     <- traverse sectionConfig2VoiceEvents secCfgs >>= extendVoicesEvents (last secCfgs)
  -- TBD: this is point where list of section configs and list of list of list of Voice Events
  -- are all present together do something like extend the length of all voices to match the
  -- length of the longest voice by repeating sectionConfig2VoiceEvents for the last of secCfgs
  -- until all voices are at least as long as the longest one and clipping the last of vesss
  -- so they're all of equal length.
  -- Overall shape is going to be [[[VoiceEvent]]] -> SectionConfig -> Driver [[[VoiceEvent]]]
  -- so it could be inlined via >== 
  let vess   = concat <$> transpose vesss
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

extendVoicesEvents :: SectionConfig -> [[[VoiceEvent]]] -> Driver [[[VoiceEvent]]]
extendVoicesEvents sectionConfig vesssIn = 
  let go vesss = do
        let vess = concat <$> transpose vesss
            veLens = ves2DurVal <$> vess
        if sum veLens == length veLens * head veLens
        then do pure vesss
        else do
          let maxLen = maximum veLens
              veLenDiffs = (-) maxLen <$> veLens
          vessNew <- sectionConfig2VoiceEvents sectionConfig <&> zipWith trimVes veLenDiffs
          go $ vesss <> [vessNew]
  in 
    go vesssIn

trimVes :: Int -> [VoiceEvent] -> [VoiceEvent]
trimVes lenDiff vesIn =
  case vesLen `compare` lenDiff of
    GT -> trim lenDiff vesIn
    LT -> vesIn
    EQ -> []
  where
    vesLen = ves2DurVal vesIn
    trim :: Int -> [VoiceEvent] -> [VoiceEvent]
    trim lenTot = snd . foldl' tr (lenTot,[])
    tr :: (Int,[VoiceEvent]) -> VoiceEvent -> (Int,[VoiceEvent])
    tr (0,ves) _  = (0,ves)
    tr (n,ves) ve  = (n',ves <> [ve'])
      where
        veLen  = ve2DurVal ve
        n'     = if n >= veLen then n - veLen else n
        veLen' = if n >= veLen then veLen else n
        -- swapVeLens squashes Tuplet to Rest:  only swap when needed
        ve'    = if veLen == veLen' then ve else swapVeLens (durVal2Dur veLen') ve
