module Utils where

import Data.List hiding (transpose)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe
import Data.Tuple

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

dur2DurVal :: Duration -> Int
dur2DurVal d = durVals !! fromEnum d

addDur :: Duration -> DurationSum -> DurationSum
addDur d ds = DurationSum $ dur2DurVal d + getDurSum ds

zDurSum :: DurationSum
zDurSum = 0

sumDurs :: [Duration] -> DurationSum
sumDurs = Data.List.foldr addDur zDurSum

halfDur :: Duration -> Duration
halfDur dur = durVal2Duration M.! (dur2DurVal dur `div` 2)

doubleDur :: Duration -> Duration
doubleDur dur = durVal2Duration M.! (dur2DurVal dur * 2)

composedDur :: Int -> Duration -> Duration
composedDur reps dur =
  durVal2Duration M.! durVal
  where
    denom :: Int
    denom  = truncate (fromIntegral (128::Int) / (fromIntegral $ dur2DurVal dur)::Double)
    durVal :: Int
    durVal = truncate (128 * (fromIntegral reps / fromIntegral denom)::Double)

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

-- in interval arithmetic 0 doesn't make any sense, 1/-1 is unison, 2/-2 is a second, etc.
-- to convert to offset from current pitch, 0 => exception, 1/-1 => 0, 2/-2 = 1/-1, etc.
intervalToOffset :: Int -> Int
intervalToOffset i
  | i < 0 = i + 1
  | i == 0 = error "intervalToOffset invalid interval 0"
  | otherwise = i - 1

transpose :: Scale -> (Pitch,Octave) -> [Int] -> [(Pitch,Octave)]
transpose scale pr = map (xp scale pr) . scanl1 (+) . map intervalToOffset

mtranspose :: Scale -> (Pitch,Octave) -> [Maybe Int] -> [Maybe (Pitch,Octave)]
mtranspose scale start = map (xp scale start <$>) . reverse . snd . foldl' f (0,[])
  where
    f (s,l) Nothing  = (s, Nothing:l)
    f (s,l) (Just i) = (s + intervalToOffset i, Just (s + intervalToOffset i):l)

-- sequentially transpose for Scale from start given mintlist until current (Pitch,Octave) equals or exceeds stop
seqMTranspose :: Scale -> (Pitch,Octave) -> (Pitch,Octave) -> [Maybe Int] -> [Maybe (Pitch,Octave)]
seqMTranspose scale start stop mintlist
  | intDir == GT && pitDir == GT = safeSeqMTranspose (<) start []
  | intDir == LT && pitDir == LT = safeSeqMTranspose (>) start []
  | intDir == EQ && pitDir == EQ = error $ "seqMTranpose intervals " <> show mintlist <> " and pitches " <> show start <> " " <> show stop <> " are both EQ "
  | otherwise = error $ "seqMTranspose int direction: " <> show intDir <> " " <> show mintlist <> " does not equal pitch direction " <> show pitDir <> " " <> show start <> " " <> show stop
  where
    safeSeqMTranspose cmp start' ret
      | (swap start') `cmp` (swap stop) = safeSeqMTranspose cmp (mkStart start' ret) ret'
      | otherwise = ret
      where
        ret' = ret ++ mtranspose scale start' mintlist
        mkStart s r = if null r then s else last . catMaybes $ r
    intDir
       | tot > 0   = GT
       | tot < 0   = LT
       | otherwise = EQ
       where
         tot = sum . map intervalToOffset . catMaybes $ mintlist -- (> 0, (< 0), or 0
    pitDir
      | start' < stop' = GT
      | start' > stop' = LT
      | otherwise      = EQ
      where
        start' = swap start
        stop'  = swap stop

-- partial if Pitch from (Pitch,Octave) is not element of Scale
xp :: Scale -> (Pitch,Octave) -> Int -> (Pitch,Octave)
xp (Scale scale) (p,o) off = (p',o')
  where
    cntSteps = length scale
    normScale = NE.sort scale -- To [C..B] or closest enharmonic to compute new octave
    pitInt = fromMaybe (error $ "pitch " <> show p <> " not in scale " <> show scale) $ elemIndex p (NE.toList normScale)
    cntOcts = (pitInt + off) `div` cntSteps
    o' = if off < 0 then fpow (abs cntOcts) decrOct o; else fpow cntOcts incrOct o
    pitIdx = (pitInt + off) `rem` cntSteps
    p' = if pitIdx < 0 then normScale NE.!! (cntSteps + pitIdx); else normScale NE.!! pitIdx

-- https://stackoverflow.com/questions/7423123/how-to-call-the-same-function-n-times
fpow :: Int -> (a -> a) -> a -> a
fpow n f x = iterate f x !! n

genNotes :: [Maybe (Pitch,Octave)] -> [Duration] -> [Accent] -> [Dynamic] -> [VoiceEvent]
genNotes = zipWith4 f
  where
    f Nothing du _ dy = VeRest $ Rest du dy
    f (Just (p,o)) du a dy = VeNote (Note p o du a dy NoSwell False)

genNotess :: [[Maybe (Pitch,Octave)]] -> [[Duration]] -> [[Accent]] -> [[Dynamic]] -> [[VoiceEvent]]
genNotess = zipWith4 genNotes

-- partial, panic on empty list
-- rotate a list forward by 1 step
rotFor :: [[a]] -> [[a]]
rotFor [] = error "rotFor empty list"
rotFor (x:xs) = xs<>[x]

rotNFor :: Int -> [[a]] -> [[a]]
rotNFor _ [] = error "rotForN empty list"
rotNFor i xs = iterate rotFor xs !! i

-- partial, panic on empty list
-- rotate a list backward by 1 step
rotRev :: [[a]] -> [[a]]
rotRev [] = error "rotRev empty list"
rotRev xs = last xs:take (length xs - 1) xs

rotNRev :: Int -> [[a]] -> [[a]]
rotNRev _ [] = error "rotRevN empty list"
rotNRev i xs = iterate rotRev xs !! i

-- [(Int,a)] Int is proportions by element of [a], e.g.:
-- [(1,a),(1,a)] => [50%,50%],
-- [(1,a),(1,a),(2,a)] => [25%,25%,50%]
genByWeight :: [(Int,a)] -> [a]
genByWeight = concatMap (uncurry replicate)
