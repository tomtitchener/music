
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
data Note = Note { _notePitch :: Pitch, _noteoOctave :: Octave, _noteDuration :: Duration, _noteAccent :: Accent, _notedDynamic :: Dynamic, _noteSlur :: Bool }
  deriving (Eq, Ord, Show)

instance ToLily Note where
  toLily (Note pit oct dur acc dyn slr) = toLily pit <> toLily oct <> toLily dur <> toLily acc <> toLily dyn <> if slr then "~" else ""

parseNote :: Parser Note
parseNote = Note <$> parsePitch <*> parseOct <*> parseDuration <*> parseAccent <*> parseDynamic <*> parseBool

instance FromLily Note  where
  parseLily = mkParseLily parseNote
