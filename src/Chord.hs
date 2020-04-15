
module Chord (Chord (..), parseChord) where

import Text.Parsec
import Text.Parsec.String

import Accent
import Duration
import Dynamic
import Lily
import Octave
import Pitch
import Utils

-- PercussionInstrumentChord e.g. "cl" == ClavesChord
data Chord = Chord { _chordPitchOctavePairs :: [(Pitch, Octave)] , _chordDuration :: Duration, _chordDynamic :: Dynamic, _chordAccent :: Accent, _chordSlur :: Bool }
  deriving (Eq, Ord, Show)

pairToLily :: (Pitch,Octave) -> String
pairToLily (p,o) = toLily p <> toLily o

pairsToLily :: [(Pitch,Octave)] -> String
pairsToLily = unwords . map pairToLily

instance ToLily Chord where
  toLily (Chord prs dur dyn acc slr) = "<" <> pairsToLily prs <> ">" <> toLily dur <> toLily dyn <> toLily acc <> if slr then "~" else ""

parsePair :: Parser (Pitch,Octave)
parsePair = (,) <$> parsePitch <*> parseOct

parsePairs :: Parser [(Pitch,Octave)]
parsePairs = parsePair `sepBy` spaces

parseChord :: Parser Chord
parseChord = Chord <$> (string "<" *> parsePairs <* string ">") <*> parseDuration <*> parseDynamic <*> parseAccent <*> parseBool

instance FromLily Chord  where
  parseLily = mkParseLily parseChord
