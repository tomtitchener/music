{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Prelude (foldl1)
import Protolude
import System.Random
import System.Random.Shuffle

import Lily

-- https://www.parsonsmatt.org/2017/09/22/what_does_free_buy_us.html

--randomSt :: (RandomGen g, Random a) => State g a
--randomSt = State random

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
  PrintLily :: (ToLily a) => a -> ActionNoValue
  Print     :: Text -> ActionNoValue

data ActionWithValue a where
  RandomizeList :: [b] -> ActionWithValue [b]
  GetConfigParam :: Text -> ActionWithValue Value

data DriverF next where
  DoAction       :: ActionNoValue -> next -> DriverF next
  DoActionThen   :: ActionWithValue a -> (a -> next) -> DriverF next

deriving instance Functor DriverF

type Driver = Free DriverF

runDriver :: forall a m.(MonadIO m, MonadRandom m, MonadState DriverState m, MonadReader DriverEnv m) => Driver a -> m a
runDriver (Free (DoActionThen act k)) =
  case act of
    RandomizeList l -> do
      l' <- shuffleM l
      runDriver $ k l'
    GetConfigParam path -> do
      conf <- asks _config
      let r = lookupConfig path conf
      case r of
        Right val -> runDriver $ k val
        Left err  -> panic err
runDriver (Free (DoAction act k)) =
  case act of
    PrintLily l -> liftIO (putStrLn (toLily l)) *> runDriver k
    Print t -> liftIO (T.putStrLn t) *> runDriver k
runDriver (Pure k) = pure k

lookupConfig :: Text -> Value -> Either Text Value
lookupConfig path config =
  let segments = T.splitOn "." path
      mval = preview (foldl1 (.) (map key segments)) config
    in case mval of
        Nothing -> Left $
            "Could not find value for path: " <>
            path <> "\nin values:\n" <>
            (toS . encode $ config)
        Just val -> Right val

printLily :: ToLily a => a -> Driver ()
printLily l = liftF $ DoAction (PrintLily l) ()

randomizeList :: ToLily a => [a] -> Driver [a]
randomizeList ls = liftF $ DoActionThen (RandomizeList ls) identity

getConfigParam :: Text -> Driver Value
getConfigParam path = liftF $ DoActionThen (GetConfigParam path) identity

print :: Show a => a -> Driver ()
print s = liftF $ DoAction (Print (T.pack . show $ s)) ()

-- https://www.programming-idioms.org/idiom/10/shuffle-a-list/826/haskell
-- shuffle :: [a] -> IO [a]
-- shuffle x = if length x < 2 then return x else do
-- 	  i <- System.Random.randomRIO (0, length(x)-1)
-- 	  r <- shuffle (take i x ++ drop (i+1) x)
-- 	  return (x!!i : r)

-----------------------------
-- Many thanks to Dan Choi --
-----------------------------

