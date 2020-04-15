
module Rest (Rest (..),parseRest) where

import Text.Parsec
import Text.Parsec.String

import Duration
import Lily
import Utils

newtype Rest = Rest { _rdur :: Duration }
  deriving (Eq, Ord, Show)

instance ToLily Rest where
  toLily (Rest dur) = "r" <> toLily dur

parseRest :: Parser Rest
parseRest = Rest <$> (char 'r' *> parseDuration)

instance FromLily Rest  where
  parseLily = mkParseLily parseRest
