{-# LANGUAGE QuasiQuotes #-}

module Voice (Voice (..), parseVoice) where

import Data.String.Interpolation
import Text.Parsec
import Text.Parsec.String

import Instrument
import Lily
import Utils
import VoiceEvent

data Voice =
  SingleVoice { _singleVoiceInstrument :: Instrument, _singleVoiceVoiceEvents :: [VoiceEvent] }
  | VoiceGroup { _voiceGroupVoices :: [Voice] }
  | PolyVoice { _polyVoiceInstrument :: Instrument, _polyVoiceVoiceEvents :: [[VoiceEvent]] }
  deriving (Eq, Ord, Show)

instance ToLily Voice where
  toLily (SingleVoice instr events) =  toSingleVoice instr events
  toLily (VoiceGroup voices) = toVoiceGroup voices
  toLily (PolyVoice instr eventss) = toPolyVoice instr eventss

toSingleVoice :: Instrument -> [VoiceEvent] -> String
toSingleVoice instr events =
  [str|\new Voice$endline$
      {\set Staff.instrumentName = ##"$shortName instr$" \set Voice.midiInstrument = ##"$midiName instr$"$endline$
      $unwords (map toLily events)$ \bar "|."$endline$
      }$endline$
      |]

toVoiceGroup :: [Voice] -> String
toVoiceGroup voices = [str|\new StaffGroup$endline$<<$endline$
                          $unwords (map toLily voices)$
                          >>$endline$
                          |]

eventsToPolyVoice :: [VoiceEvent] -> String
eventsToPolyVoice events  =
  [str|\new Staff {$endline$
      new Voice {$endline$
      $unwords (map toLily events)$"\bar "|."$endline$
      }$endline$
      }|]

toPolyVoice :: Instrument -> [[VoiceEvent]] -> String
toPolyVoice instr eventss =
  [str|\new PianoStaff {$endline$
      <<$endline$
      \set PianoStaff.instrumentName = ##"$shortName instr$"\set PianoStaff.midiInstrument = ##"$midiName instr$"$unwords (map eventsToPolyVoice eventss)$
      >>$endline$
      }$endline$
      |]

parsePolyVoiceEvents :: Parser [VoiceEvent]
parsePolyVoiceEvents = string "\\new Staff{\nnew Voice {\n" *> parseVoiceEvent `sepBy` space <* spaces <* string "\\bar \"|.\"\n}\n"

parseVoice :: Parser Voice
parseVoice = choice [
  try (SingleVoice <$> (string "\\new Voice\n{\\set Staff.instrumentName = #" *> parseQuotedIdentifier *> string "\\set Voice.midiInstrument = #" *> parseInstrument <* string "\n")
                   <*> (parseVoiceEvent `sepBy` space <* (spaces <* string "\\bar \"|.\"\n}\n")))
  ,try (VoiceGroup <$> (string "\\new StaffGroup\n<<\n" *> parseVoice `sepBy` space <* string ">>\n"))
  ,try (PolyVoice <$> (string "\\new PianoStaff {\n<<\n\\set PianoStaff.instrumentName = #" *> parseQuotedIdentifier *> string "\\set PianoStaff.midiInstrument = #" *> parseInstrument <* string "\n")
                  <*> (parsePolyVoiceEvents `sepBy` space) <* string ">>\n")
  ]

instance FromLily Voice where
  parseLily = mkParseLily parseVoice


