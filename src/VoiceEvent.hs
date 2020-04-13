
module VoiceEvent (VoiceEvent (..)
                  ,toLily
                  ,parseLily
                  ,parseVoiceEvent
                  ) where

import Chord
import Clef
import KeySignature
import Lily
import Note
import Rest
import Tempo
import Text.Parsec
import Text.Parsec.String
import TimeSignature
import Utils

data VoiceEvent =
    VoiceEventClef { _voiceEventClef :: Clef }
  | VoiceEventNote { _voiceEventNote :: Note }
  | VoiceEventRest { _voiceEventRest :: Rest }
  | VoiceEventChord { _voiceEventChord :: Chord }
  | VoiceEventTempo { _voiceEventTempo :: Tempo.Tempo }
  | VoiceEventKeySignature { _voiceEventKeySignature :: KeySignature }
  | VoiceEventTimeSignature { _voiceEventTimeSignature :: TimeSignature }
  deriving (Eq, Ord, Show)

instance ToLily VoiceEvent where
  toLily (VoiceEventClef clef) = toLily clef
  toLily (VoiceEventNote note) = toLily note
  toLily (VoiceEventRest rest) = toLily rest
  toLily (VoiceEventChord chord) = toLily chord
  toLily (VoiceEventTempo tempo) = toLily tempo
  toLily (VoiceEventKeySignature keySignature) = toLily keySignature
  toLily (VoiceEventTimeSignature timeSignature) = toLily timeSignature

parseVoiceEvent :: Parser VoiceEvent
parseVoiceEvent = choice [try (VoiceEventClef <$> parseClef)
                         ,try (VoiceEventNote <$> parseNote)
                         ,try (VoiceEventRest <$> parseRest)
                         ,try (VoiceEventChord <$> parseChord)
                         ,try (VoiceEventTempo <$> parseTempo)
                         ,try (VoiceEventKeySignature <$> parseKeySignature)
                         ,try (VoiceEventTimeSignature <$> parseTimeSignature)]

instance FromLily VoiceEvent where
  parseLily = mkParseLily parseVoiceEvent
