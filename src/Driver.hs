{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Driver where

import Control.Monad.Free
import Control.Monad.Random.Class
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson
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

data ActionWithValue a where
  RandomizeList :: [b] -> ActionWithValue [b]

data DriverF next where
  DoAction     :: ActionNoValue -> next -> DriverF next
  DoActionThen :: ActionWithValue a -> (a -> next) -> DriverF next

deriving instance Functor DriverF

type Driver = Free DriverF

runDriver :: forall a m.(MonadIO m, MonadRandom m, MonadState DriverState m, MonadReader DriverEnv m) => Driver a -> m a
runDriver (Free (DoActionThen act k)) =
  case act of
    RandomizeList l -> do
      l' <- shuffleM l
      runDriver $ k l'
runDriver (Free (DoAction act k)) =
  case act of
    PrintLily l -> liftIO (putStrLn (toLily l)) *> runDriver k
runDriver (Pure k) = pure k

printLily :: ToLily a => a -> Driver ()
printLily l = liftF $ DoAction (PrintLily l) ()

randomizeList :: ToLily a => [a] -> Driver [a]
randomizeList ls = liftF $ DoActionThen (RandomizeList ls) identity

-- https://www.programming-idioms.org/idiom/10/shuffle-a-list/826/haskell
-- shuffle :: [a] -> IO [a]
-- shuffle x = if length x < 2 then return x else do
-- 	  i <- System.Random.randomRIO (0, length(x)-1)
-- 	  r <- shuffle (take i x ++ drop (i+1) x)
-- 	  return (x!!i : r)

-----------------------------
-- Many thanks to Dan Choi --
-----------------------------
