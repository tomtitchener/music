
module KeySignature (KeySignature (..), parseKeySignature) where

import Text.Parsec
import Text.Parsec.String

import Lily
import Mode
import Pitch
import Utils

data KeySignature = KeySignature { _kspit :: Pitch, _ksmode :: Mode }
  deriving (Eq, Ord, Show)

instance ToLily KeySignature where
  toLily (KeySignature pit mode) = "\\key " <> toLily pit <> " " <> toLily mode

parseKeySignature :: Parser KeySignature
parseKeySignature = KeySignature <$> (string "\\key " *> parsePitch) <*> (spaces *> parseMode)

instance FromLily KeySignature where
  parseLily = mkParseLily parseKeySignature
