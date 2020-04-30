{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Driver where

import Control.Lens
import Control.Monad.Free
import Control.Monad.Random.Class
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Lens
import Data.List.Split hiding (sepBy)
import qualified Data.Text as T
import System.Random
import System.Random.Shuffle
import Text.Parsec
import Text.Parsec.String

import Lily
import Types

newtype DriverState = DriverState {
    _randGen :: StdGen
  } deriving (Show)

initState :: DriverState
initState = DriverState (mkStdGen 5)

data DriverEnv = DriverEnv {
    _config :: Value
  } deriving Show

initEnv :: Value -> DriverEnv
initEnv = DriverEnv

data ActionNoValue where
  WriteLily :: (ToLily a) => FilePath -> a -> ActionNoValue
  PrintLily :: (ToLily a) => a -> ActionNoValue
  Print     :: String -> ActionNoValue

data ActionWithValue a where
  RandomElement  :: [b] -> ActionWithValue b
  RandomElements :: [b] -> ActionWithValue [b]
  RandomizeList  :: [b] -> ActionWithValue [b]
  GetConfigParam :: String -> ActionWithValue ConfigSelector

data DriverF next where
  DoAction       :: ActionNoValue -> next -> DriverF next
  DoActionThen   :: ActionWithValue a -> (a -> next) -> DriverF next

deriving instance Functor DriverF

type Driver = Free DriverF

runDriver :: forall a m.(MonadIO m, MonadRandom m, MonadState DriverState m, MonadReader DriverEnv m) => Driver a -> m a
runDriver (Free (DoActionThen act k)) =
  case act of
    RandomElement  l    -> getRandomR (0, (length l) - 1) >>= runDriver . k . (l !!)
    RandomElements l    -> getRandomRs (0, (length l) - 1) >>= runDriver . k . map (l !!)
    RandomizeList  l    -> shuffleM l >>= runDriver . k
    GetConfigParam path -> asks _config >>= pure . lookupConfig path >>= runDriver . k
runDriver (Free (DoAction act k)) =
  case act of
    WriteLily fn l -> liftIO (writeFile fn (toLily l)) *> runDriver k
    PrintLily l    -> liftIO (putStrLn (toLily l)) *> runDriver k
    Print t        -> liftIO (putStrLn t) *> runDriver k
runDriver (Pure k) = pure k

lookupConfig :: String -> Value -> ConfigSelector
lookupConfig path config =
  let segments = splitOn "." path
  in case preview (foldl1 (.) (map (key . T.pack) segments) . _String) config of
    Nothing -> error $
               "Could not find value for path: " <>
               path <> "\nin values:\n" <>
               show config
    Just txt -> parseConfigSelector (T.unpack txt)

writeLily :: ToLily a => FilePath -> a -> Driver ()
writeLily fName l = liftF $ DoAction (WriteLily fName l) ()

printLily :: ToLily a => a -> Driver ()
printLily l = liftF $ DoAction (PrintLily l) ()

randomElement :: ToLily a => [a] -> Driver a
randomElement ls = liftF $ DoActionThen (RandomElement ls) id

randomElements :: ToLily a => [a] -> Driver [a]
randomElements ls = liftF $ DoActionThen (RandomElements ls) id

randomizeList :: ToLily a => [a] -> Driver [a]
randomizeList ls = liftF $ DoActionThen (RandomizeList ls) id

getConfigParam :: String -> Driver ConfigSelector
getConfigParam path = liftF $ DoActionThen (GetConfigParam path) id

print :: Show a => a -> Driver ()
print s = liftF $ DoAction (Print (show $ s)) ()

data ConfigSelector =
  SelPitches [Pitch]
  | SelDurations [Duration]
  deriving (Eq, Show)

parseConfigSelector :: String -> ConfigSelector
parseConfigSelector = either (error . show) id . parse pConfigSelector ""

pConfigSelector :: Parser ConfigSelector
pConfigSelector =
  choice [
    SelPitches <$> (string "pitches" *> spaces *> parsePitch `sepBy` space)  -- "c d e"
  , SelDurations <$> (string "durations" *> spaces *> parseDuration `sepBy` space) -- "4 2. 2 16 32"
  ]

-- https://www.programming-idioms.org/idiom/10/shuffle-a-list/826/haskell
-- shuffle :: [a] -> IO [a]
-- shuffle x = if length x < 2 then return x else do
-- 	  i <- System.Random.randomRIO (0, length(x)-1)
-- 	  r <- shuffle (take i x ++ drop (i+1) x)
-- 	  return (x!!i : r)

-- https://www.parsonsmatt.org/2017/09/22/what_does_free_buy_us.html

--randomSt :: (RandomGen g, Random a) => State g a
--randomSt = State random

-----------------------------
-- Many thanks to Dan Choi --
-----------------------------
