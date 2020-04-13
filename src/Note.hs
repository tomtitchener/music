{-# LANGUAGE DeriveGeneric #-}

module Note (Note (..)
            ,toLily
            ,parseLily
            ,parseNote
            ) where

import Accent
import Duration
import Dynamic
import GHC.Generics
import Lily
import Octave
import Pitch
import Text.Parsec.String
import Utils

-- PercussionInstrumentNote e.g. "cl" == ClavesNote
data Note = Note { _notePitch :: Pitch, _noteoOctave :: Octave, _noteDuration :: Duration, _notedDynamic :: Dynamic, _noteAccent :: Accent, _noteSlur :: Bool }
  deriving (Eq, Ord, Show, Generic)

instance ToLily Note where
  toLily (Note pit oct dur dyn acc slr) = toLily pit <> toLily oct <> toLily dur <> toLily dyn <> toLily acc <> if slr then "~" else ""

parseNote :: Parser Note
parseNote = Note <$> parsePitch <*> parseOct <*> parseDuration <*> parseDynamic <*> parseAccent <*> parseBool

instance FromLily Note  where
  parseLily = mkParseLily parseNote
