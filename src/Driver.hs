{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Driver where

import Control.Lens
import Control.Monad.Free
import Control.Monad.Random.Class
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Lens
import Data.List.Split hiding (sepBy)
import qualified Data.Text as T
import System.Random.Shuffle

import Config
import Lily
import Types

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

--instance MonadFail Driver where
--  fail = error

runDriver :: forall a m.(MonadIO m, MonadRandom m, MonadReader DriverEnv m {--, MonadFail m0--}) => Driver a -> m a
runDriver (Free (DoActionThen act k)) =
  case act of
    RandomElement  l    -> getRandomR (0, length l - 1) >>= runDriver . k . (l !!)
    RandomElements l    -> getRandomRs (0, length l - 1) >>= runDriver . k . map (l !!)
    RandomizeList  l    -> shuffleM l >>= runDriver . k
    GetConfigParam path -> (lookupConfig path <$> asks _config) >>= runDriver . k
runDriver (Free (DoAction act k)) =
  case act of
    WriteScore fn (Score c vs) -> asks _seed >>= (\s -> liftIO (writeFile fn (toLily (Score (c <> " " <> s) vs))) *> runDriver k)
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
