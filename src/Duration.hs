{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Duration (Duration (..)
                ,DurationSum
                ,addDur
                ,zDurSum
                ,sumDurs
                ,durSum2Durs
                ,lilySyms
                ,toLily
                ,parseLily
                ,parseDuration
                ,integralDurations
                ,integralDurationSyms
                ) where

import Data.List
import qualified Data.Map as M
import Data.Maybe
import Text.Parsec
import Text.Parsec.String
import Utils

import Lily

-- Ord order
data Duration = HTEDur | SFDur | DSFDur | TSDur | DTSDur | SDur | DSDur | EDur | DEDur | QDur | DQDur | HDur | DHDur | WDur | DWDur
  deriving (Eq, Ord, Show, Enum, Bounded)

-- parse order
lilyVals :: [Duration]
lilyVals = [HTEDur,DSFDur,SFDur,DTSDur,TSDur,DSDur,SDur,DEDur,EDur,DQDur,QDur,DHDur,HDur,DWDur,WDur]

-- parse order
lilySyms :: [String]
lilySyms = ["128","64.","64","32.","32","16.","16","8.","8","4.","4","2.","2","1.","1"]

instance ToLily Duration where
  toLily = mkToLily "duration" lilyVals lilySyms

parseDuration :: Parser Duration
parseDuration = choice $ zipWith mkParser lilySyms lilyVals

instance FromLily Duration  where
  parseLily = mkParseLily parseDuration

integralDurations :: [Duration]
integralDurations = [WDur, HDur, QDur, EDur, SDur, SFDur, HTEDur]

integralDurationSyms :: [String]
integralDurationSyms = ["1","2","4","8","16","32","64","128"]

newtype DurationSum = DurationSum { getDurSum :: Int }
  deriving (Eq, Ord, Show, Num)

-- in terms of 128ths, same order as Duration
durVals :: [Int]
durVals = [1, 2, 2 + 1, 4, 4 + 2, 8, 8 + 4, 16, 16 + 8, 32, 32 + 16, 64, 64 + 32, 128, 128 + 64]

durVal2Duration :: M.Map Int Duration
durVal2Duration = M.fromList (zip durVals [HTEDur .. DWDur])

addDur :: Duration -> DurationSum -> DurationSum
addDur d ds = DurationSum $ (durVals !! fromEnum d) + getDurSum ds

zDurSum :: DurationSum
zDurSum = 0

sumDurs :: [Duration] -> DurationSum
sumDurs = foldr addDur zDurSum

-- Simple-minded disaggregation, will prefer dotted durations, e.g.:
-- > durSum2Durs (addDur WDur (addDur WDur (addDur WDur zDurSum)))
--  [DWDur,DWDur]
-- Shows way to disaggregation in the presence of:
--   * DurationSum that is total distance from downbeat
--   * Meter
-- With those, compute:
--   * Duration to next downbeat, small to large
--   * Duration from there to end of current measure
--   * Durations to cover remaining whole measures, if any
--   * Duration in final measure to start of partial beat
--   * Durations to remainder of total large to small
durSum2Durs :: DurationSum -> [Duration]
durSum2Durs = unfoldr f
  where
    f (DurationSum 0) = Nothing
    f (DurationSum i) = Just (d, ds)
      where
        v = fromJust $ find (i >=) (reverse durVals)
        d = durVal2Duration M.! v
        ds = DurationSum (i - v)
