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
import Text.Parsec.Number
import Text.Parsec.String

import Lily
import Types

newtype DriverState = DriverState {
    _randGen :: StdGen
  } deriving (Show)

initState :: String -> StdGen -> DriverState
initState "" gen = DriverState gen
initState seed _ = DriverState ((read seed)::StdGen)

newtype DriverEnv = DriverEnv {
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
    RandomElement  l    -> getRandomR (0, length l - 1) >>= runDriver . k . (l !!)
    RandomElements l    -> getRandomRs (0, length l - 1) >>= runDriver . k . map (l !!)
    RandomizeList  l    -> shuffleM l >>= runDriver . k
    GetConfigParam path -> (lookupConfig path <$> asks _config) >>= runDriver . k
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
print s = liftF $ DoAction (Print (show s)) ()

data ConfigSelector =
  SelPitches [Pitch]
  | SelAccents [Accent]
  | SelAccentss [[Accent]]
  | SelDynamics [Dynamic]
  | SelDynamicss [[Dynamic]]
  | SelDurations [Duration]
  | SelDurationss [[Duration]]
  | SelMIntervals [Maybe Int]
  | SelMIntervalss [[Maybe Int]]
  | SelPitOctPr (Pitch,Octave)
  | SelPitOctPrs [(Pitch,Octave)]
  | SelInstrument Instrument
  deriving (Eq, Show)

parseConfigSelector :: String -> ConfigSelector
parseConfigSelector = either (error . show) id . parse pConfigSelector ""

pConfigSelector :: Parser ConfigSelector
pConfigSelector =
  choice [
    try $ SelPitches <$> (string "pitches" *> spaces *> parsePitch `sepBy` space)          -- c d e
  , try $ SelAccentss <$> (string "accentss" *> spaces *> pAccents `sepBy` char ',')       -- _ > .,espressivo ^ -
  , try $ SelAccents <$> (string "accents" *> spaces *> pAccents)                          -- ^ - !
  , try $ SelDynamicss <$> (string "dynamicss" *> spaces *> pDynamics `sepBy` char ',')    -- p f pp,sf ff rfz
  , try $ SelDynamics <$> (string "dynamics" *> spaces *> pDynamics)                       -- p f pp
  , try $ SelDurationss <$> (string "durationss" *> spaces *> pDurations `sepBy` char ',') -- 4 2. 2,16 32 64,4 2 1
  , try $ SelDurations <$> (string "durations" *> spaces *> pDurations)                    -- 4 2. 2 16 32
  , try $ SelMIntervalss <$> (string "intmottos" *> spaces *> pMInts `sepBy` char ',')     -- 1 r 2 -3 4 r
  , try $ SelMIntervals <$> (string "intmotto" *> spaces *> pMInts)                        -- 1 r 2 -3 4 r
  , try $ SelPitOctPrs <$> (string "pitoctprs" *> spaces *> pPitOctPr `sepBy` char ',')    -- (c,"'"),(g,""),(d,",")
  , try $ SelPitOctPr <$> (string "pitoctpr" *> spaces *> pPitOctPr)                       -- (c,"'")
  , try $ SelInstrument <$> (string "instrument" *> spaces *> parseInstrument)             -- acoustic grand
  ]

pAccents :: Parser [Accent]
pAccents = pAccentStr `sepBy` space

pDynamics :: Parser [Dynamic]
pDynamics = pDynamicStr `sepBy` space

pDurations :: Parser [Duration]
pDurations = parseDuration `sepBy` space

pMInt :: Parser (Maybe Int)
pMInt = (Just <$> int) <|> (char 'r' >> pure Nothing)

pMInts :: Parser [Maybe Int]
pMInts = pMInt `sepBy` space

pPitOctPr :: Parser (Pitch,Octave)
pPitOctPr = between (char '(') (char ')') ((,) <$> parsePitch <*> (char ',' *> pQuotedOct))

pQuotedOct :: Parser Octave
pQuotedOct = between (symbol '"') (symbol '"') parseOctave

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
