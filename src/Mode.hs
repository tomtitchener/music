
module Mode (Mode (..)
              ,lilySyms
              ,toLily
              ,parseLily
              ,parseMode
              ) where

import Text.Parsec
import Text.Parsec.String
import Utils

import Lily

-- Ord order
data Mode = Major | Minor
  deriving (Eq, Ord, Show, Enum, Bounded)

-- parse order
lilySyms :: [String]
lilySyms = ["\\major", "\\minor"]

-- parse order
lilyVals :: [Mode]
lilyVals = [Major .. Minor]

instance ToLily Mode where
  toLily = mkToLily "mode" lilyVals lilySyms

parseMode :: Parser Mode
parseMode = choice (zipWith mkParser lilySyms lilyVals)

instance FromLily Mode where
  parseLily = mkParseLily parseMode
