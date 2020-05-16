{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Driver where

import Control.Lens hiding (only)
import Control.Monad.Free
import Control.Monad.Random.Class
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Lens
import Data.List.Split hiding (sepBy)
import qualified Data.Text as T
import System.Random.Shuffle
import Text.Parsec
import Text.Parsec.Number
import Text.Parsec.String

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
  GetConfigParam :: String -> ActionWithValue ConfigSelector

data DriverF next where
  DoAction       :: ActionNoValue -> next -> DriverF next
  DoActionThen   :: ActionWithValue a -> (a -> next) -> DriverF next

deriving instance Functor DriverF

type Driver = Free DriverF

instance MonadFail Driver where
  fail = error

runDriver :: forall a m.(MonadIO m, MonadRandom m, MonadReader DriverEnv m, MonadFail m) => Driver a -> m a
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

lookupConfig :: String -> Value -> ConfigSelector
lookupConfig path config =
  let segments = splitOn "." path
  in case preview (foldl1 (.) (map (key . T.pack) segments) . _String) config of
    Nothing -> error $
               "Could not find value for path: " <>
               path <> "\nin values:\n" <>
               show config
    Just txt -> parseConfigSelector (T.unpack txt)

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

getConfigParam :: String -> Driver ConfigSelector
getConfigParam path = liftF $ DoActionThen (GetConfigParam path) id

print :: Show a => a -> Driver ()
print s = liftF $ DoAction (Print (show s)) ()

data ConfigSelector =
  SelInt Int
  | SelPitches [Pitch]
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
  | SelKey KeySignature
  | SelClef Clef
  deriving (Eq, Show)

parseConfigSelector :: String -> ConfigSelector
parseConfigSelector = either (error . show) id . parse pConfigSelector ""

pConfigSelector :: Parser ConfigSelector
pConfigSelector =
  choice [
    try $ SelInt <$> (string "int" *> spaces *> int)   -- 5
  , try $ SelInstrument  <$> only parseInstrument      -- acoustic grand
  , try $ SelKey         <$> only pKeySig              -- g major
  , try $ SelClef        <$> only pClefStr             -- bass
  , try $ SelPitOctPr    <$> only pPitOctPr            -- (c,0)
  , try $ SelPitches     <$> only (mkPs parsePitch)    -- c,d,e
  , try $ SelAccents     <$> only (mkPs pAccentStr)    -- ^,-,!
  , try $ SelDynamics    <$> only (mkPs pDynamicStr)   -- p,f,pp
  , try $ SelDurations   <$> only (mkPs parseDuration) -- 4,2.,2,16,32
  , try $ SelMIntervals  <$> only (mkPs pMInt)         -- 1,r,2,-3,4,r
  , try $ SelPitOctPrs   <$> only (mkPs pPitOctPr)     -- (c,-1),(g,0),(d,1)
  , try $ SelAccentss    <$> mkPss pAccentStr          -- _,>,.;espressivo,^,-
  , try $ SelDynamicss   <$> mkPss pDynamicStr         -- p,f,pp;sf,ff,rfz
  , try $ SelDurationss  <$> mkPss parseDuration       -- 4,2.,2;16,32,64;4,2,1
  , try $ SelMIntervalss <$> mkPss pMInt               -- 1,r,2;-3,4,r
  ]

comma :: Parser Char
comma = char ','

semi :: Parser Char
semi = char ';'

only :: Parser a -> Parser a
only p = lexeme p <* eof

lexeme :: Parser a -> Parser a
lexeme p = spaces *> p

mkParser :: String -> a -> Parser a
mkParser s d = try (string s >> pure d)

mkPs :: Parser a -> Parser [a]
mkPs p = lexeme p `sepBy1` comma

mkPss :: Parser a -> Parser [[a]]
mkPss p = mkPs p `sepBy1` semi

accentStrs :: [String]
accentStrs = ["^", "-", "!", ".",  ">", "_", "espressivo", "~"]

dynamicStrs :: [String]
dynamicStrs = ["ppppp", "pppp", "ppp", "pp", "p", "mp", "mf", "fffff", "ffff", "fff", "ff", "fp", "f", "sff", "sfz", "sf", "spp", "sp", "rfz", "~"]

dynamicVals :: [Dynamic]
dynamicVals = [PPPPP, PPPP, PPP, PP, Piano, MP, MF, FFFFF, FFFF, FFF, FF, FP, Forte, SFF, SFZ, SF, SPP, SP, RFZ, NoDynamic]

pDynamicStr :: Parser Dynamic
pDynamicStr = choice (zipWith mkParser dynamicStrs dynamicVals)

pAccentStr :: Parser Accent
pAccentStr = choice (zipWith mkParser accentStrs [Marcato .. NoAccent])

pMInt :: Parser (Maybe Int)
pMInt = (Just <$> int) <|> (char 'r' >> pure Nothing)

modeStrs :: [String]
modeStrs = ["major", "minor"]

pModeStr :: Parser Mode
pModeStr = choice (zipWith mkParser modeStrs [Major .. Minor])

pKeySig :: Parser KeySignature
pKeySig = KeySignature <$> parsePitch <*> (spaces *> pModeStr)

octaveInts :: [String]
octaveInts = ["-4","-3","-2","-1","0","1","2","3"]

pOctaveStr :: Parser Octave
pOctaveStr = choice (zipWith mkParser octaveInts [TwentyNineVBOct .. TwentyTwoVAOct])

pPitOctPr :: Parser (Pitch,Octave)
pPitOctPr = between (char '(') (char ')') ((,) <$> parsePitch <*> (char ',' *> pOctaveStr))

clefStrs :: [String]
clefStrs = ["bass_8", "bass", "tenor", "alto", "treble", "treble^8"]

pClefStr :: Parser Clef
pClefStr = choice (zipWith mkParser clefStrs [Bass8VB ..Treble8VA])

-- https://www.programming-idioms.org/idiom/10/shuffle-a-list/826/haskell
-- https://www.parsonsmatt.org/2017/09/22/what_does_free_buy_us.html

-----------------------------
-- Many thanks to Dan Choi --
-----------------------------
