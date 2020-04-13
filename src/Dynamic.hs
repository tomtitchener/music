
module Dynamic (Dynamic (..)
              ,lilySyms
              ,toLily
              ,parseLily
              ,parseDynamic
              ) where

import Text.Parsec
import Text.Parsec.String
import Utils

import Lily

-- Ord order
data Dynamic = PPPPP | PPPP | PPP | PP | Piano | MP | MF | Forte | FF | FFF | FFFF | FFFFF | FP | SF | SFF | SP | SPP | SFZ | RFZ | NoDynamic
  deriving (Eq, Ord, Show, Enum, Bounded)

-- parse order
lilySyms :: [String]
lilySyms = ["\\ppppp", "\\pppp", "\\ppp", "\\pp", "\\piano", "\\mp", "\\mf", "\\fffff", "\\ffff", "\\fff", "\\ff", "\\fp", "\\forte", "\\sff", "\\sfz", "\\sf", "\\spp", "\\sp", "\\rfz", ""]

-- parse order
lilyVals :: [Dynamic]
lilyVals = [PPPPP, PPPP, PPP, PP, Piano, MP, MF, FFFFF, FFFF, FFF, FF, FP, Forte, SFF, SFZ, SF, SPP, SP, RFZ, NoDynamic]

instance ToLily Dynamic where
  toLily = mkToLily "dynamic" lilyVals lilySyms

parseDynamic :: Parser Dynamic
parseDynamic = choice (zipWith mkParser (init lilySyms) (init lilyVals)) <|> pure NoDynamic

instance FromLily Dynamic  where
  parseLily = mkParseLily parseDynamic
