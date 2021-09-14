{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Driver where

import Control.Lens ( preview )
import Control.Monad.Free ( Free(..), liftF )
import Control.Monad.Random.Class
    ( MonadRandom(getRandomRs, getRandomR) )
import Control.Monad.Reader ( MonadIO(..), MonadReader, asks )
import Data.Aeson ( Value )
import Data.Aeson.Lens ( key, AsPrimitive(_String) )
import Data.List.Split ( splitOn )
import qualified Data.Text as T
import System.Random.Shuffle ( shuffleM )

import Config ( FromConfig(..) )
import Lily ( ToLily(..) )
import Types ( Score(Score) )
import Utils ( genByWeight )

data DriverEnv = DriverEnv {
     _config :: Value
    ,_seed   :: String
  } deriving Show

initEnv :: Value -> String -> DriverEnv
initEnv = DriverEnv

data ActionNoValue where
  WriteScore ::  FilePath -> Score -> ActionNoValue
  PrintLily :: (ToLily a) => a -> ActionNoValue
  Print     :: String -> ActionNoValue

data ActionWithValue a where
  RandomElement  :: [b] -> ActionWithValue b
  RandomElements :: [b] -> ActionWithValue [b]
  RandomizeList  :: [b] -> ActionWithValue [b]
  GetConfigParam :: FromConfig a => String -> ActionWithValue a

data DriverF next where
  DoAction       :: ActionNoValue -> next -> DriverF next
  DoActionThen   :: ActionWithValue a -> (a -> next) -> DriverF next

deriving instance Functor DriverF

type Driver = Free DriverF

runDriver :: forall a m.(MonadIO m, MonadRandom m, MonadReader DriverEnv m) => Driver a -> m a
runDriver (Free (DoActionThen act k)) =
  case act of
    RandomElement  l    -> getRandomR (0, length l - 1) >>= runDriver . k . (l !!)
    RandomElements l    -> getRandomRs (0, length l - 1) >>= runDriver . k . map (l !!)
    RandomizeList  l    -> shuffleM l >>= runDriver . k
    GetConfigParam path -> asks (lookupConfig path . _config) >>= runDriver . k
runDriver (Free (DoAction act k)) =
  case act of
    WriteScore fileName (Score c vs) -> asks _seed >>= (\s -> liftIO (writeFile fileName (toLily (Score (c <> " " <> s) vs))) *> runDriver k)
    PrintLily l -> liftIO (putStrLn (toLily l)) *> runDriver k
    Print t -> liftIO (putStrLn t) *> runDriver k
runDriver (Pure k) = pure k

lookupConfig :: FromConfig a => String -> Value -> a
lookupConfig path config =
  let segments = splitOn "." path
  in case preview (foldl1 (.) (map (key . T.pack) segments) . _String) config of
    Nothing -> error $
               "Could not find value for path: " <>
               path <> "\nin values:\n" <>
               show config
    Just txt -> parseConfig (T.unpack txt)

writeScore :: FilePath -> Score -> Driver ()
writeScore fName s = liftF $ DoAction (WriteScore fName s) ()

printLily :: ToLily a => a -> Driver ()
printLily l = liftF $ DoAction (PrintLily l) ()

randomElement :: [a] -> Driver a
randomElement ls = liftF $ DoActionThen (RandomElement ls) id

randomWeightedElement :: [(Int,a)] -> Driver a
randomWeightedElement ws = liftF $ DoActionThen (RandomElement (genByWeight ws)) id

randomElements :: [a] -> Driver [a]
randomElements ls = liftF $ DoActionThen (RandomElements ls) id

randomizeList :: [a] -> Driver [a]
randomizeList ls = liftF $ DoActionThen (RandomizeList ls) id

getConfigParam :: (FromConfig a, Show a) => String -> Driver a
getConfigParam path = liftF $ DoActionThen (GetConfigParam path) id

print :: Show a => a -> Driver ()
print s = liftF $ DoAction (Print (show s)) ()

-- https://www.parsonsmatt.org/2017/09/22/what_does_free_buy_us.html

------------------------
-- Thanks to Dan Choi --
------------------------
