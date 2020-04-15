{-# LANGUAGE QuasiQuotes #-}

module Tempo (Tempo (..), parseTempo) where

import Data.Natural
import Data.String.Interpolation
import Text.Parsec
import Text.Parsec.String

import Duration
import Lily
import Utils

data Tempo = TempoText   { _ttText :: String }
           | Tempo       { _tDur :: Duration, _tPerMin :: Natural }
           | TempoLong   { _tlText :: String, _tlDur :: Duration, _tlPerMin :: Natural  }
           | TempoRange  { _trDur :: Duration, _trPerMinLo :: Natural, _trPerMinHi :: Natural  }
  deriving (Eq, Ord, Show)

instance ToLily Tempo where
  toLily (TempoText txt) = [str|\tempo $txt$|]
  toLily (Tempo dur perMin) =  [str|\tempo $toLily dur$ = $:perMin$|]
  toLily (TempoLong txt dur perMin) = [str|\tempo $txt$ ($toLily dur$ = $:perMin$)|]
  toLily (TempoRange dur loPerMin hiPerMin) = [str|\tempo $toLily dur$ = $:loPerMin$ - $:hiPerMin$|]

parseTempo :: Parser Tempo
parseTempo = choice [try (TempoRange <$> (string "\\tempo " *> parseDuration) <*> (string " = " *> parseNatural) <*> (string " - " *> parseNatural))
                    ,try (TempoLong <$> (string "\\tempo " *> manyTill anyChar (try (string " ("))) <*> parseDuration <*> (string " = " *> parseNatural <* char ')'))
                    ,try (Tempo <$> (string "\\tempo " *> parseDuration) <*> (string " = " *> parseNatural))
                    ,try (TempoText <$> (string "\\tempo " *> manyTill anyChar eof))]

instance FromLily Tempo where
  parseLily = mkParseLily parseTempo
