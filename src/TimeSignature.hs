
module TimeSignature (TimeSignature (..),parseTimeSignature) where

import Data.List
import Text.Parsec
import Text.Parsec.String

import Duration
import Lily
import Utils

data TimeSignature = TimeSignature { _tsNum :: Int, _tsDenom :: Duration }
                   | TimeSignatureGrouping { _tsgGroups :: [Int], tsgNum :: Int, tsgDenom :: Duration }
  deriving (Eq, Ord, Show)

instance ToLily TimeSignature where
  toLily (TimeSignature num denom) = "\\time " <> show num <> "/" <> toLily denom
  toLily (TimeSignatureGrouping nums num denom) = "\\time #'(" <>  intercalate "," (map show nums) <> ")"  <> " " <> show num <> "/" <> toLily denom

parseTimeSignature :: Parser TimeSignature
parseTimeSignature = choice [try (TimeSignatureGrouping <$> (string "\\time #'" *> parseIntList) <*> parseInt  <*> (string "/" *> parseDuration))
                            ,try (TimeSignature <$> (string "\\time " *> parseInt) <*> (string "/" *> parseDuration))]

parseIntList :: Parser [Int]
parseIntList = between (symbol '(') (symbol ')') (parseInt `sepBy` char ',')

instance FromLily TimeSignature where
  parseLily = mkParseLily parseTimeSignature
