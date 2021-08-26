{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Utils where

import Data.List hiding (transpose)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S (fromList)
import Data.Tuple
import Data.Tuple.Extra

import Types

newtype DurationSum = DurationSum { getDurSum :: Int }
  deriving (Eq, Ord, Show, Num)

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

composedDur :: Int -> Duration -> Duration
composedDur reps dur =
  durVal2Duration M.! durVal
  where
    denom :: Int
    denom  = truncate (fromIntegral (128::Int) / fromIntegral (dur2DurVal dur)::Double)
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
int2Off :: Int -> Int
int2Off i
  | i < 0 = i + 1
  | i == 0 = error "int2Off invalid interval 0"
  | otherwise = i - 1

transpose :: Scale -> (Pitch,Octave) -> [Int] -> [(Pitch,Octave)]
transpose scale pr = map (xp scale pr) . scanl1 (+) . map int2Off

mtranspose :: Scale -> (Pitch,Octave) -> [Maybe Int] -> [Maybe (Pitch,Octave)]
mtranspose scale start = map (xp scale start <$>) . reverse . snd . foldl' f (0,[])
  where
    f (s,l) Nothing  = (s, Nothing:l)
    f (s,l) (Just i) = (s', Just s':l)
      where
        s' = s + int2Off i

-- sequentially transpose for Scale from start given mintlist until current (Pitch,Octave) equals or exceeds stop
seqMTranspose :: Scale -> NE.NonEmpty (Maybe Int) -> ((Pitch,Octave),(Pitch,Octave)) -> NE.NonEmpty (Maybe (Pitch,Octave))
seqMTranspose scale mIntList (start,stop)
  | intDir == GT && pitDir == GT = NE.unfoldr (f (<)) (0,0)
  | intDir == LT && pitDir == LT = NE.unfoldr (f (>)) (0,0)
  | intDir == EQ && pitDir == EQ = error $ "seqMTranpose intervals " <> show mIntList <> " and pitches " <> show start <> " " <> show stop <> " are both EQ "
  | otherwise = error $ "seqMTranspose int direction: " <> show intDir <> " " <> show mIntList <> " does not equal pitch direction " <> show pitDir <> " " <> show start <> " " <> show stop
  where
    f _ (0,0) = case NE.head mIntList of
                  Nothing -> (Nothing, Just (1,0))
                  Just i  -> (Just (xp scale start (int2Off i)),Just (1,int2Off i))
    f cmp (ix,prv) = case mPitOct of
                       Nothing -> (Nothing,Just (ix',prv'))
                       Just next -> if swap next `cmp` swap stop
                                    then (mPitOct,Just (ix',prv'))
                                    else (mPitOct,Nothing)
      where
        ix' = if ix == NE.length mIntList - 1 then 0 else ix + 1
        mPitOct = xp scale start . (+) prv . int2Off <$> mInt
        prv' = maybe prv ((prv +) . int2Off) mInt
        mInt = mIntList NE.!! ix
    intDir
       | tot > 0   = GT
       | tot < 0   = LT
       | otherwise = EQ
       where
         tot = sum (maybe 0 int2Off <$> mIntList) -- (> 0), (< 0), or 0
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

genNotesWithTies :: NE.NonEmpty (Maybe (Pitch,Octave)) -> NE.NonEmpty Duration -> NE.NonEmpty Accent -> NE.NonEmpty Dynamic -> NE.NonEmpty Bool -> NE.NonEmpty VoiceEvent
genNotesWithTies = neZipWith5 f
  where
    f Nothing du _ dy _ = VeRest $ Rest du dy
    f (Just (p,o)) du a dy sl = VeNote $ Note p o du a dy NoSwell sl

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

-- Constraints:
--  * num > denom, -- no, this is wrong!
--  * length _tupNotes `mod` num == 0,
--  * 1 == length . nub . map _noteDur $ _tupNotes,
mkTuplet :: Int -> Int -> NE.NonEmpty Note -> Tuplet
mkTuplet num denom notes
--  | num <= denom                 = error $ "tuplet num: " <> show num <> " <= denom: " <> show denom
  | 0 /= numNotes `mod` num      = error $ "tuplet length notes: " <> show numNotes <> " not evenly divisible by num " <> show num
  | 1 /= NE.length (NE.nub durs) = error $ "tuplet non-uniform durations: " <> show durs
  | otherwise = Tuplet num denom (NE.head durs) notes
  where
    durs = NE.map _noteDur notes
    numNotes = NE.length notes

-- Cloned from https://hackage.haskell.org/package/base-4.14.1.0/docs/src/Data.List.NonEmpty.html#zipWith
neZipWith3 :: (a1 -> a2 -> a3 -> a4) -> NE.NonEmpty a1 -> NE.NonEmpty a2 -> NE.NonEmpty a3 -> NE.NonEmpty a4
neZipWith3 f ~(x1 NE.:| x1s) ~(x2 NE.:| x2s) ~(x3 NE.:| x3s) = f x1 x2 x3 NE.:| zipWith3 f x1s x2s x3s

neZipWith5 :: (a1 -> a2 -> a3 -> a4 ->a5 -> a6) -> NE.NonEmpty a1 -> NE.NonEmpty a2 -> NE.NonEmpty a3 -> NE.NonEmpty a4 -> NE.NonEmpty a5 -> NE.NonEmpty a6
neZipWith5 f ~(x1 NE.:| x1s) ~(x2 NE.:| x2s) ~(x3 NE.:| x3s) ~(x4 NE.:| x4s) ~(x5 NE.:| x5s) = f x1 x2 x3 x4 x5 NE.:| zipWith5 f x1s x2s x3s x4s x5s

neZipWith7 :: (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8)
           -> NE.NonEmpty a1 -> NE.NonEmpty a2 -> NE.NonEmpty a3 -> NE.NonEmpty a4 -> NE.NonEmpty a5 -> NE.NonEmpty a6 -> NE.NonEmpty a7
           -> NE.NonEmpty a8
neZipWith7 f ~(x1 NE.:| x1s) ~(x2 NE.:| x2s) ~(x3 NE.:| x3s) ~(x4 NE.:| x4s) ~(x5 NE.:| x5s)  ~(x6 NE.:| x6s) ~(x7 NE.:| x7s) =
  f x1 x2 x3 x4 x5 x6 x7 NE.:| zipWith7 f x1s x2s x3s x4s x5s x6s x7s

enharmonicPitches :: [[Pitch]]
enharmonicPitches = [[Bs,C,Dff],[Bss,Cs,Df],[Css,D,Eff],[Ds,Ef,Fff],[Dss,E,Ff],[Es,F,Gff],[Ess,Fs,Gf],[Fss,G,Aff],[Gs,Af],[Gss,A,Bff],[As,Bf,Cff],[Ass,B,Cf]]

normPitch :: Pitch -> Int
normPitch = fromJust . flip findIndex enharmonicPitches . elem

normOctave :: Octave -> Int
normOctave = (12 *) . fromEnum

normalizePitchOctave :: Pitch -> Octave -> Int
normalizePitchOctave p o = normPitch p + normOctave o

normalizePitchOctavePair :: (Pitch, Octave) -> Int
normalizePitchOctavePair = uncurry normalizePitchOctave

allClefRanges :: M.Map Clef ((Pitch,Octave),(Pitch,Octave))
allClefRanges = M.fromList
  [(Treble8VA, ((A,COct),(C,TwentyTwoVAOct)))
  ,(Treble,    ((A,EightVBOct),(C,FifteenVAOct)))
  ,(Tenor,     ((D,EightVBOct),(F,EightVAOct)))
  ,(Alto,      ((B,FifteenVBOct),(D,EightVAOct)))
  ,(Bass,      ((C,FifteenVBOct),(E,COct)))
  ,(Bass8VB,   ((C,TwentyTwoVBOct),(E,EightVBOct)))]

allClefNormalizedRanges :: M.Map Clef (Int,Int)
allClefNormalizedRanges = M.map normPair allClefRanges
  where
    normPair = normalizePitchOctavePair *** normalizePitchOctavePair

stdClefRanges :: M.Map Clef ((Pitch,Octave),(Pitch,Octave))
stdClefRanges = M.fromList
  [(Treble8VA, ((A,COct),(C,TwentyTwoVAOct)))
  ,(Treble,    ((A,EightVBOct),(C,FifteenVAOct)))
  ,(Bass,      ((C,FifteenVBOct),(E,COct)))
  ,(Bass8VB,   ((C,TwentyTwoVBOct),(E,EightVBOct)))]

stdClefNormalizedRanges :: M.Map Clef (Int,Int)
stdClefNormalizedRanges = M.map normPair stdClefRanges
  where
    normPair = normalizePitchOctavePair *** normalizePitchOctavePair

sortClefsByRange :: [Clef] -> [Clef]
sortClefsByRange = sortBy compareClefsByRange
  where
    compareClefsByRange :: Clef -> Clef -> Ordering
    compareClefsByRange ca cb = compare ra rb
      where
        ra = allClefNormalizedRanges M.! ca
        rb = allClefNormalizedRanges M.! cb

partitionClefs :: NE.NonEmpty VoiceEvent -> ([Clef], [VoiceEvent])
partitionClefs ves = (clefs,nonClefVes)
  where
    (clefVes,nonClefVes) = NE.partition isClef ves
    clefs = map toClef clefVes
    isClef (VeClef _) = True
    isClef _ = False
    toClef (VeClef clef) = clef
    toClef ve = error $ "partitionClefs unexpected VoiceEvent: " <> show ve <> " is not Clef"

startingClef :: Int -> ([Clef],[VoiceEvent]) -> Clef
startingClef cnt (clefs,ves) = normPit2Clef clefs averageNormPitch
  where
    notes = take cnt $ takeWhile isNote ves
    isNote :: VoiceEvent -> Bool
    isNote (VeNote _) = True
    isNote _ = False
    normPitches = map normVeNote notes
    normVeNote :: VoiceEvent -> Int
    normVeNote (VeNote (Note pit oct _ _ _ _ _)) = normalizePitchOctave pit oct
    normVeNote ve = error $ "startingClef expected only VeNote but got: " <> show ve
    averageNormPitch = sum normPitches `div` length normPitches

normPit2Clef :: [Clef] -> Int -> Clef
normPit2Clef clefs i =
  case matchingRanges of
    [] -> if i > normMiddleC then Bass8VB else Treble8VA
    [(clef,(_,_))] -> clef
    ranges -> fst (minimumBy compareClefRanges ranges)
  where
    normMiddleC = normalizePitchOctave C COct
    matchingRanges = filter (uncurry (&&) . ((i >=) *** (i <=)) . snd) allRanges
    observedRanges = M.restrictKeys allClefNormalizedRanges (S.fromList clefs)
    allRanges = M.toList (stdClefNormalizedRanges <> observedRanges)
    compareClefRanges (_,rangea) (_,rangeb) = compare (measure rangea) (measure rangeb)
      where
        measure :: (Int,Int) -> Double
        measure (lo,hi) = abs ((fromIntegral i - fromIntegral lo) / (fromIntegral hi - fromIntegral i)) - 1
