
module Clef (Clef (..), parseClef) where

import Text.Parsec
import Text.Parsec.String

import Lily
import Utils

-- Ord order
data Clef = Bass8VB | Bass | Tenor | Alto | Treble | Treble8VA
  deriving (Eq, Ord, Show, Enum, Bounded)

-- parse order
lilySyms :: [String]
lilySyms = ["\\clef treble^8", "\\clef treble", "\\clef alto", "\\clef tenor", "\\clef bass", "\\clef bass_8"]

-- parse order
lilyVals :: [Clef]
lilyVals = [Bass8VB .. Treble8VA]

instance ToLily Clef where
  toLily = mkToLily "clef" lilyVals lilySyms

parseClef :: Parser Clef
parseClef = choice (zipWith mkParser lilySyms lilyVals)

instance FromLily Clef  where
  parseLily = mkParseLily parseClef
