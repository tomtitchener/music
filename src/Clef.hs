{-# LANGUAGE DeriveGeneric #-}

module Clef (Clef (..)
              ,lilySyms
              ,toLily
              ,parseLily
              ,parseClef
              ) where

import GHC.Generics
import Text.Parsec
import Text.Parsec.String
import Utils

import Lily

-- Ord order
data Clef = Bass8VB | Bass | Tenor | Alto | Treble | Treble8VA
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

-- parse order
lilySyms :: [String]
lilySyms = ["treble^8", "treble", "alto", "tenor", "bass", "bass_8"]

-- parse order
lilyVals :: [Clef]
lilyVals = [Bass8VB .. Treble8VA]

instance ToLily Clef where
  toLily = mkToLily "dynamic" lilyVals lilySyms

parseClef :: Parser Clef
parseClef = choice (zipWith mkParser lilySyms lilyVals)

instance FromLily Clef  where
  parseLily = mkParseLily parseClef
