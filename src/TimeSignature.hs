--  TODO: groupings and beats per measure \time #'(2 2 3) 7/8

module TimeSignature (TimeSignature (..)
                     ,toLily
                     ,parseLily
                     ,parseTimeSignature
                     ) where

import Duration
import Text.Parsec
import Text.Parsec.String
import Utils

import Lily

data TimeSignature = TimeSignature { _tsnum :: Int, _tsdenom :: Duration }
  deriving (Eq, Ord, Show)

instance ToLily TimeSignature where
  toLily (TimeSignature num denom) = "\\time " <> show num <> "/" <> toLily denom

parseTimeSignature :: Parser TimeSignature
parseTimeSignature = TimeSignature <$> (string "\\time " *> parseInt) <*> (string "/" *> parseDuration)

instance FromLily TimeSignature where
  parseLily = mkParseLily parseTimeSignature
