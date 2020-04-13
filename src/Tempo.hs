
-- Tempo numerator / denominator
-- where numerator is integral duration WDur, HDur, QDur, EDur, SDur, TSDur, SFDur,
-- denominator is
--  1) beats per second:  \tempo 4 = 128
--  2) low and high beats per second: \tempo 4 = 120 - 128
--  3) Text: \tempo "Allegretto" | "Faster" | "Slower" | "Meno Mosso" ... etc.
--  3) Text plus numerator denominator: \tempo "Allegro" (4 = 160) where text can be empty

module Tempo (Tempo (..)
             ,toLily
             ,parseLily
             ,parseTempo
             ) where

import Duration
import Text.Parsec
import Text.Parsec.String
import Utils

import Lily

data Tempo = Tempo { _tnum :: Int, _tdenom :: Duration }
  deriving (Eq, Ord, Show)

instance ToLily Tempo where
  toLily (Tempo num denom) = "\\tempo " <> show num <> " = " <> toLily denom

parseTempo :: Parser Tempo
parseTempo = Tempo <$> (string "\\tempo " *> parseInt) <*> (string " = " *> parseDuration)

instance FromLily Tempo where
  parseLily = mkParseLily parseTempo
