
module Accent (Accent (..) ,parseAccent) where

import Text.Parsec
import Text.Parsec.String

import Lily
import Utils

-- Ord order
data Accent = Marcato | Tenuto | Staccatissimo | Staccato | Accent | Portato | Espressivo | NoAccent
  deriving (Eq, Ord, Show, Enum, Bounded)

-- parse order
lilySyms :: [String]
lilySyms = ["-^", "--", "-!", "-.",  "->", "-_", "\\espressivo", ""]

-- parse order
lilyVals :: [Accent]
lilyVals = [Marcato .. NoAccent]

instance ToLily Accent where
  toLily = mkToLily "accent" lilyVals lilySyms

parseAccent :: Parser Accent
parseAccent = choice (zipWith mkParser (init lilySyms) (init lilyVals)) <|> pure NoAccent

instance FromLily Accent  where
  parseLily = mkParseLily parseAccent
