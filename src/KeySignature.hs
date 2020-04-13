{-# LANGUAGE DeriveGeneric #-}

module KeySignature (KeySignature (..)
                    ,toLily
                    ,parseLily
                    ,parseKeySignature
                    ) where

import GHC.Generics
import Mode
import Pitch
import Text.Parsec
import Text.Parsec.String
import Utils

import Lily

data KeySignature = KeySignature { _kspit :: Pitch, _ksmode :: Mode }
  deriving (Eq, Ord, Show, Generic)

instance ToLily KeySignature where
  toLily (KeySignature pit mode) = "\\key " <> toLily pit <> " " <> toLily mode

parseKeySignature :: Parser KeySignature
parseKeySignature = KeySignature <$> (string "\\key " *> parsePitch) <*> (spaces *> parseMode)

instance FromLily KeySignature where
  parseLily = mkParseLily parseKeySignature
