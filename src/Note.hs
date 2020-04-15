
module Note (Note (..), parseNote) where

import Text.Parsec.String

import Accent
import Duration
import Dynamic
import Lily
import Octave
import Pitch
import Utils

-- PercussionInstrumentNote e.g. "cl" == ClavesNote
data Note = Note { _notePitch :: Pitch, _noteoOctave :: Octave, _noteDuration :: Duration, _notedDynamic :: Dynamic, _noteAccent :: Accent, _noteSlur :: Bool }
  deriving (Eq, Ord, Show)

instance ToLily Note where
  toLily (Note pit oct dur dyn acc slr) = toLily pit <> toLily oct <> toLily dur <> toLily dyn <> toLily acc <> if slr then "~" else ""

parseNote :: Parser Note
parseNote = Note <$> parsePitch <*> parseOct <*> parseDuration <*> parseDynamic <*> parseAccent <*> parseBool

instance FromLily Note  where
  parseLily = mkParseLily parseNote
