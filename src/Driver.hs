{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Driver (initEnv
              ,runDriver
              ,writeScore
              ,printLily
              ,randomElement
              ,randomWeightedElement
              ,randomElements
              ,randomIndex
              ,randomIndices
              ,randomizeList
              ,getConfigParam
              ,getMConfigParam
              ,searchConfigParam
              ,searchMConfigParam
              ,searchAltConfigParam              
              ,printIt
              ,cfgPath2Keys
              ,Driver
              ) where

import Control.Lens (preview)
import Control.Monad.Free (Free(..), liftF)
import Control.Monad.Random.Class (MonadRandom(getRandomR, getRandomRs))
import Control.Monad.Reader (MonadIO(..), MonadReader, asks)
import Data.Aeson (Value)
import Data.Aeson.Lens (key, AsValue(_String), AsValue(_Object))
import Data.Aeson.Key (fromString, toString)
import Data.Aeson.KeyMap (keys)
import Data.List (intercalate, sort)
import Data.List.Split (splitOn)
import Data.Text (unpack)
import System.IO (hFlush,stdout)
import System.Random.Shuffle (shuffleM)

import Config (FromConfig(..))
import Lily (ToLily(..))
import Types (Score(Score))

data DriverEnv = DriverEnv {
     _config :: Value
    ,_seed   :: String
  } deriving Show

initEnv :: Value -> String -> DriverEnv
initEnv = DriverEnv

-- requires GADTS
data ActionNoValue where
  WriteScore ::  FilePath -> Score -> ActionNoValue
  PrintLily :: (ToLily a) => a -> ActionNoValue
  Print     :: String -> ActionNoValue

data ActionWithValue a where
  RandomElement    :: [b] -> ActionWithValue b
  RandomElements   :: [b] -> ActionWithValue [b]
  RandomIndex      :: Int -> ActionWithValue Int
  RandomIndices    :: Int -> ActionWithValue [Int]
  RandomizeList    :: [b] -> ActionWithValue [b]
  GetConfigParam   :: FromConfig a => String -> ActionWithValue a
  GetMConfigParam  :: FromConfig a => String -> ActionWithValue (Maybe a)
  GetConfigSubKeys :: String -> ActionWithValue [String]

data DriverF next where
  DoAction       :: ActionNoValue -> next -> DriverF next
  DoActionThen   :: ActionWithValue a -> (a -> next) -> DriverF next

deriving instance Functor DriverF -- requires DeriveFunctor, StandaloneDeriving

type Driver = Free DriverF

-- requires FlexibleContexts and RankeNTypes
runDriver :: forall a m.(MonadIO m, MonadRandom m, MonadReader DriverEnv m) => Driver a -> m a
runDriver (Free (DoActionThen act k)) =
  case act of
    RandomElement  l      -> getRandomR  (0, length l - 1) >>= runDriver . k . (l !!)
    RandomElements l      -> getRandomRs (0, length l - 1) >>= runDriver . k . map (l !!)
    RandomIndex    n      -> getRandomR  (0, n - 1) >>= runDriver . k
    RandomIndices  n      -> getRandomRs (0, n - 1) >>= runDriver . k
    RandomizeList  l      -> shuffleM l >>= runDriver . k
    GetConfigParam path   -> asks (lookupConfig path . _config) >>= runDriver . k
    GetMConfigParam path  -> asks (lookupMConfig path . _config) >>= runDriver . k
    GetConfigSubKeys path -> asks (lookupConfigKeys path . _config) >>= runDriver . k
runDriver (Free (DoAction act k)) =
  case act of
    WriteScore fileName (Score t _ vs) -> asks _seed >>= (\s -> liftIO (writeFile fileName (toLily (Score t (showSeed s) vs))) *> runDriver k)
    PrintLily l -> liftIO (putStrLn (toLily l) >> hFlush stdout) *> runDriver k
    Print t -> liftIO (putStrLn t) *> runDriver k
runDriver (Pure k) = pure k

-- from: "StdGen {unStdGen = SMGen 16160098205052642697 16833526763116284519}" to: "SMGen 16160098205052642697 16833526763116284519"
showSeed :: String -> String
showSeed = init . drop (length "StdGen {unStdGen = SMGen ")

lookupConfig :: FromConfig a => String -> Value -> a
lookupConfig path config =
  let segments = splitOn "." path
  -- config is JSON Value from read of YAML config file
  -- path is '.' separated String of key values
  -- used to be you converted String to Text to call key
  -- the segments consist of a list of keys encoded as Strings ["a","b",..]
  -- we parse step-by-step from the top-most key e.g. "a" to the next Value,
  -- then parse with the next key e.g. "b" into the next Value,
  -- and etc. until we're done with segments, at which point we convert
  -- the Value to a String, JSON parse that and answer the result.
  -- The problem is that the key method used to take a Text for input,
  -- but now it takes a new Key type, which is the result of the key method.
  -- Or no, that's not quite right, because the key method takes a Key and
  -- answers a Value
  -- 
  in case preview (foldl1 (.) (map (key . fromString) segments) . _String) config of
    Nothing -> error $
               "Could not find value for path: " <>
               path <> "\nin values:\n" <>
               show config
    Just txt -> parseConfig (unpack txt)

lookupMConfig :: FromConfig a => String -> Value -> Maybe a
lookupMConfig path config =
  let segments = splitOn "." path
  in parseConfig . unpack <$> preview (foldl1 (.) (map (key . fromString) segments) . _String) config

lookupConfigKeys :: String -> Value -> [String]
lookupConfigKeys path config =
  let segments = splitOn "." path
  in case preview (foldl1 (.) (map (key . fromString) segments) . _Object) config of
    Nothing -> error $
               "Could not find value for path: " <>
               path <> "\nin values:\n" <>
               show config
    Just m -> toString <$> keys m

writeScore :: FilePath -> Score -> Driver ()
writeScore fName s = liftF $ DoAction (WriteScore fName s) ()

printLily :: ToLily a => a -> Driver ()
printLily l = liftF $ DoAction (PrintLily l) ()

randomElement :: [a] -> Driver a
randomElement ls = liftF $ DoActionThen (RandomElement ls) id

-- [(Int,a)] Int is proportions by element of [a], e.g.:
-- [(1,a),(1,a)] => [50%,50%],
-- [(1,a),(1,a),(2,a)] => [25%,25%,50%]
genByWeight :: [(Int,a)] -> [a]
genByWeight = concatMap (uncurry replicate)

randomWeightedElement :: [(Int,a)] -> Driver a
randomWeightedElement ws = liftF $ DoActionThen (RandomElement (genByWeight ws)) id

randomElements :: [a] -> Driver [a]
randomElements ls = liftF $ DoActionThen (RandomElements ls) id

randomIndex :: Int -> Driver Int
randomIndex n = liftF $ DoActionThen (RandomIndex n) id

randomIndices :: Int -> Driver [Int]
randomIndices n = liftF $ DoActionThen (RandomIndices n) id

randomizeList :: [a] -> Driver [a]
randomizeList ls = liftF $ DoActionThen (RandomizeList ls) id

getConfigParam :: (FromConfig a, Show a) => String -> Driver a
getConfigParam path = liftF $ DoActionThen (GetConfigParam path) id

getMConfigParam :: (FromConfig a, Show a) => String -> Driver (Maybe a)
getMConfigParam path = liftF $ DoActionThen (GetMConfigParam path) id

printIt :: Show a => a -> Driver ()
printIt s = liftF $ DoAction (Print (show s)) ()

-- String in path must end with key for Value that is Object (HashMap Text Value),
-- answers list of keys in Object matching regexp in target
cfgPath2Keys :: (String ->  Bool) -> String -> Driver [String]
cfgPath2Keys filtf path = liftF $ DoActionThen (GetConfigSubKeys path) (sort . filter filtf)

-- If initial path doesn't contain key, repeatedly swap next higher level
-- with "common" looking for same key, e.g.:
--   ["title","section1","voice1","tempo"] -> ["title","section1","common","tempo"]
--   ["title","section1","common","tempo"] -> ["title","common","tempo"]
--   ["title","common","tempo"] -> []
searchConfigParam :: (FromConfig a, Show a) => String -> Driver a
searchConfigParam path = do
  let go segs =
        if null segs
        then do
          getConfigParam path
        else do
          mVal <- getMConfigParam (intercalate "." segs)
          case mVal of
            Just val -> pure val
            Nothing  -> go (retrySegs segs)
  go $ splitOn "." path
  where
    retrySegs :: [String] -> [String]
    retrySegs segs
      | "common" `notElem` segs = take (length segs - 2) segs <> ["common"] <> [last segs]
      | length segs > 3 = take (length segs - 3) segs  <> drop (length segs - 2) segs
      | otherwise = []

searchMConfigParam :: (FromConfig a, Show a) => String -> Driver (Maybe a)
searchMConfigParam path = do
  let go segs =
        if null segs
        then do
          pure Nothing
        else do
          mVal <- getMConfigParam (intercalate "." segs)
          case mVal of
            Just val -> pure $ Just val
            Nothing  -> go (retrySegs segs)
  go $ splitOn "." path
  where
    retrySegs :: [String] -> [String]
    retrySegs segs
      | "common" `notElem` segs = take (length segs - 2) segs <> ["common"] <> [last segs]
      | length segs > 3 = take (length segs - 3) segs  <> drop (length segs - 2) segs
      | otherwise = []

searchAltConfigParam :: (FromConfig a, Show a) => [String] -> Driver a
searchAltConfigParam paths = inner paths []
  where
    inner (p:ps) tried = do
      mv <- searchMConfigParam p
      case mv of
        Just v  -> pure v
        Nothing -> inner ps (p:tried)
    inner [] tried = error $ "searchAltConfigParam failed paths: " <> show tried

-- https://www.parsonsmatt.org/2017/09/22/what_does_free_buy_us.html

------------------------
-- Thanks to Dan Choi --
------------------------
