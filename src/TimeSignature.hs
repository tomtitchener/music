{-# LANGUAGE DeriveGeneric #-}

-- Time signature numerator/denominator
-- where denominator is integral duration WDur, HDur, QDur, EDur, SDur, TSDur, SFDur,
-- numerator is count of ingtegral durations in a measure
--  1) beats per measure: \time 2/4
--  2) groupings and beats per measure \time #'(2 2 3) 7/8

module TimeSignature (TimeSignature (..)
                     ,toLily
                     ,parseLily
                     ,parseTimeSignature
                     ) where

import Duration
import GHC.Generics
import Text.Parsec
import Text.Parsec.String
import Utils

import Lily

data TimeSignature = TimeSignature { _tsnum :: Int, _tsdenom :: Duration }
  deriving (Eq, Ord, Show, Generic)

instance ToLily TimeSignature where
  toLily (TimeSignature num denom) = "\\time " <> show num <> "/" <> toLily denom

parseTimeSignature :: Parser TimeSignature
parseTimeSignature = TimeSignature <$> (string "\\time " *> parseInt) <*> (string "/" *> parseDuration)

instance FromLily TimeSignature where
  parseLily = mkParseLily parseTimeSignature
