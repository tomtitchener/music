{-# LANGUAGE DeriveGeneric #-}

module Rest (Rest (..)
            ,toLily
            ,parseLily
            ,parseRest
            ) where

import Accent
import Duration
import GHC.Generics
import Lily
import Text.Parsec
import Text.Parsec.String
import Utils

newtype Rest = Rest { _rdur :: Duration }
  deriving (Eq, Ord, Show, Generic)

instance ToLily Rest where
  toLily (Rest dur) = "r" <> toLily dur

parseRest :: Parser Rest
parseRest = Rest <$> (char 'r' *> parseDuration)

instance FromLily Rest  where
  parseLily = mkParseLily parseRest
