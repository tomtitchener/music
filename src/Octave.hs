{-# LANGUAGE DeriveGeneric #-}

module Octave (Octave (..)
              ,lilySyms
              ,toLily
              ,parseLily
              ,parseOct
              ,incrOct
              ,decrOct
              ) where

import GHC.Generics
import Text.Parsec
import Text.Parsec.String
import Utils

import Lily

-- Ord order
data Octave = TwentyTwoVBOct | FifteenVBOct | EightVBOct | COct | EightVAOct | FifteenVAOct | TwentyTwoVAOct | TwentyNineVAOct
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

-- parse order
lilyVals :: [Octave]
lilyVals = [TwentyTwoVBOct,FifteenVBOct,EightVBOct,TwentyNineVAOct,TwentyTwoVAOct,FifteenVAOct,EightVAOct,COct]

-- parse order
lilySyms :: [String]
lilySyms = [",,,"
           ,",,"
           ,","
           ,"''''"
           ,"'''"
           ,"''"
           ,"'"
           ,""]

instance ToLily Octave where
  toLily = mkToLily "octave" lilyVals lilySyms

parseOct :: Parser Octave
parseOct = choice (zipWith mkParser (init lilySyms) (init lilyVals)) <|> pure COct

instance FromLily Octave  where
  parseLily = mkParseLily parseOct

incrOct :: Octave -> Octave
incrOct o = toEnum $ min (1 + fromEnum o) (fromEnum (maxBound::Octave))

decrOct :: Octave -> Octave
decrOct o = toEnum $ max (fromEnum o - 1) (fromEnum (minBound::Octave))
