module Utils (incrOct
             ,decrOct
             ,addDur
             ,zDurSum
             ,sumDurs
             ,durSum2Durs
             ,transpose
             ,mtranspose
             ,genNotes
             ) where

import Data.List hiding (transpose)
import qualified Data.Map as M
import Data.Maybe

import Types

incrOct :: Octave -> Octave
incrOct o = toEnum $ min (1 + fromEnum o) (fromEnum (maxBound::Octave))

decrOct :: Octave -> Octave
decrOct o = toEnum $ max (fromEnum o - 1) (fromEnum (minBound::Octave))

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
sumDurs = Data.List.foldr addDur zDurSum

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

transpose :: [Pitch] -> (Pitch,Octave) -> [Int] -> [(Pitch,Octave)]
transpose scale pr = map (xp scale pr) . scanl (+) 0

mtranspose :: [Pitch] -> (Pitch,Octave) -> [Maybe Int] -> [Maybe (Pitch,Octave)]
mtranspose scale pr = map (xp scale pr <$>) . reverse . snd . foldl' f (0,[Just 0])
  where
    f (s,l) Nothing  = (s, Nothing:l)
    f (s,l) (Just i) = (s+i, Just (s+i):l)

-- partial if Pitch from (Pitch,Octave) is not element of [Pitch]
xp :: [Pitch] -> (Pitch,Octave) -> Int -> (Pitch,Octave)
xp scale (p,o) i = (p',o')
  where
    cntSteps = length scale
    normScale = sort scale -- To [C..B] or closest enharmonic to compute new octave
    pitInt = fromMaybe (error $ "pitch " <> show p <> " not in scale " <> show scale) $ elemIndex p normScale
    cntOcts = (pitInt + i) `div` cntSteps
    o' = if i < 0 then fpow (abs cntOcts) decrOct o; else fpow cntOcts incrOct o
    pitIdx = (pitInt + i) `rem` cntSteps
    p' = if pitIdx < 0 then normScale !! (cntSteps + pitIdx); else normScale !! pitIdx

-- https://stackoverflow.com/questions/7423123/how-to-call-the-same-function-n-times
fpow :: Int -> (a -> a) -> a -> a
fpow n f x = iterate f x !! n

genNotes :: [Maybe (Pitch,Octave)] -> [Duration] -> [Accent] -> [Dynamic] -> [VoiceEvent]
genNotes = zipWith4 f
  where
    f Nothing du _ _ = VeRest $ Rest du
    f (Just (p,o)) du a dy = VeNote (Note p o du a dy False)

