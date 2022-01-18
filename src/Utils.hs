{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Utils where

import Data.Bifunctor (second)
import Data.List hiding (transpose)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S (fromList)
import Data.Tuple
import Data.Tuple.Extra hiding (second)

import Types

newtype DurationSum = DurationSum { getDurSum :: Int }
  deriving (Eq, Ord, Show, Num) -- Num requires GeneralizedNewTypeDeriving

incrOct :: Octave -> Octave
incrOct o = toEnum $ min (1 + fromEnum o) (fromEnum (maxBound::Octave))

decrOct :: Octave -> Octave
decrOct o = toEnum $ max (fromEnum o - 1) (fromEnum (minBound::Octave))

-- in terms of 128ths, same order as Duration
durVals :: [Int]
durVals = [1,      2,     2 + 1,  4,     4 + 2,  8,    8 + 4, 16,   16 + 8, 32,   32 + 16, 64,   64 + 32, 128,  128 + 64]
       --  HTEDur  SFDur  DSFDur  TSDur  DTSDur  SDur  DSDur  EDur  DEDur   QDur  DQDur    HDur  DHDur    WDur  DWDur
       --  1/128   1/64   1/64.   1/32   1/32.   1/16  1/16.  1/8   1/8.    1/4   1/4.     1/2   1/2.     1     1.

durVal2Duration :: M.Map Int Duration
durVal2Duration = M.fromList (zip durVals [HTEDur .. DWDur])

durVal2Dur :: Int -> Duration
durVal2Dur durVal = fromMaybe err $ M.lookup durVal durVal2Duration 
  where
    err = error $ "durVal2Dur: could not convert durVal: " <> show durVal <> " to integral Duration"

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

-- Aggregation that takes time signature and current length into account
-- Len vars are all in 128th notes
-- beatLen is length for beat (numerator from time signature)
-- barLen is length of bar
-- curLen is length of previous list of VoiceEvent to which we'll be adding rests or spacers
-- addLen is length of rests or spacers to add
-- answers list of durations for spacers or rests, taking position in beat and bar.
-- tbd: take groupings within bar encoded in TimeSignatureGrouping into account,
-- e.g. for (2,2,3) 7/8, full bar rests are quarter quarter dotted-quarter, and the same
-- for a full-bar duration note, maybe treat the bar as though it contained three little
-- bars, 2/8, 2/8, 3/8, trick would be for series of tied notes to tie between little bars
-- (though that's a problem the caller has to solve)
addEndDurs :: TimeSignature -> Int -> Int -> [Duration]
addEndDurs (TimeSignatureSimple numCnt denomDur) curLen addLen =
  accum curLen addLen []
  where
    accum :: Int -> Int -> [Duration] -> [Duration]
    accum cur add acc
      | cur < 0 || add < 0 = error $ "accum cur: " <> show cur <> " add: " <> show add <> " acc: " <> show acc
      | add == 0 = acc
      | beatLen /= remBeat =
          let nextCur = if add < remBeat then cur + add else cur + remBeat
              nextAdd = if add < remBeat then 0 else add - remBeat
              nextAcc = acc <> (reverse . durSum2Durs . DurationSum $ if add < remBeat then add else remBeat)
          in
            accum nextCur nextAdd nextAcc
      | barLen /= remBar =
          let nextCur = if add < remBar then cur + add else cur + remBar
              nextAdd = if add < remBar then 0 else add - remBar
              nextAcc = (acc <> (durSum2Durs . DurationSum $ if add < remBar then add else remBar))
          in
            accum nextCur nextAdd nextAcc
      | add >= barLen =
          let nextCur = cur + barLen
              nextAdd = add - barLen
              nextAcc = acc <> (reverse . durSum2Durs . DurationSum $ barLen)
          in
            accum nextCur nextAdd nextAcc
      | add < barLen =
          let nextCur = cur + add
              nextAdd = 0
              nextAcc = acc <> (durSum2Durs . DurationSum $ add)
          in
            accum nextCur nextAdd nextAcc
      | otherwise = error $ "accum programming error, beatLen: " <> show beatLen <> " barLen: " <> show barLen <>
                            " remBeat: " <> show remBeat <> " remBar: " <> show remBar <>
                            " cur: " <> show cur <> " add: " <> show add <> " acc: " <> show acc
      where
        remBeat = beatLen - (cur `rem` beatLen)
        remBar =  barLen - (cur `rem` barLen)
    beatLen = dur2DurVal denomDur
    barLen  = numCnt * beatLen
-- Split grouped time signature into little internal bars by groupings.
addEndDurs (TimeSignatureGrouping groups numCnt denomDur) curLen addLen =
  concatMap (uncurry3 addEndDurs) (firstTuple:endTuples)
  where
    firstTuple = (firstTimeSig,firstCurLen,firstAddLen)
    endTuples = genTuples (wrapIx firstIx) (addLen - firstAddLen) []
    genTuples thisIndx remAddLen ret
      | remAddLen < 0  = error $ "genTuples remAddLen: " <> show remAddLen <> " < 0"
      | remAddLen == 0 = ret
      | otherwise      = genTuples nextIndx nextRemAddLen (ret <> [(thisTimeSig,0,thisAddLen)])
      where
        thisGroupLen  = groupLens !! thisIndx
        thisTimeSig   = groupTimeSigs !! thisIndx
        thisAddLen    = min remAddLen thisGroupLen
        nextRemAddLen = max 0 (remAddLen - thisGroupLen)
        nextIndx      = wrapIx thisIndx
    wrapIx i = if i == length groups - 1 then 0 else 1 + i
    firstIx = fromMaybe errMsg $ findIndex (> spillOver) groupSums
      where
        errMsg = error $ "addEndDurs findIndex groupSums: " <> show groupSums
    firstTimeSig  = groupTimeSigs !! firstIx
    firstGroupSum = groupSums !! firstIx
    firstGroupLen = groupLens !! firstIx
    firstAddLen   = if 0 == spillOver then min addLen firstGroupLen else min addLen (firstGroupSum - spillOver)
    firstCurLen   = if 0 == spillOver then 0 else firstGroupLen - firstAddLen
    groupTimeSigs = map (`TimeSignatureSimple` denomDur) groups'
    groupSums = scanl1 (+) groupLens
    groupLens = map (beatLen *) groups'
    spillOver = curLen `rem` barLen
    beatLen = dur2DurVal denomDur
    barLen  = numCnt * beatLen
    groups' = NE.toList groups

timeSig2Num :: TimeSignature -> Int
timeSig2Num TimeSignatureSimple{..} = _tsNum
timeSig2Num TimeSignatureGrouping{..} = _tsgNum

timeSig2Denom :: TimeSignature -> Duration
timeSig2Denom TimeSignatureSimple{..} = _tsDenom
timeSig2Denom TimeSignatureGrouping{..} = _tsgDenom

ve2Durs :: VoiceEvent -> [Duration]
ve2Durs (VeNote   Note {..})    = [_noteDur]
ve2Durs (VeRest   Rest {..})    = [_restDur]
ve2Durs (VeChord  Chord {..})   = [_chordDur]
ve2Durs (VeRhythm Rhythm {..})  = [_rhythmDur]
ve2Durs (VeSpacer Spacer {..})  = [_spacerDur]
ve2Durs (VeTuplet t@Tuplet{..}) = replicate (_tupDenom * tup2CntTups t) _tupDur
ve2Durs _                       = []

ve2DurVal :: VoiceEvent -> Int
ve2DurVal ve = sum (dur2DurVal <$> ve2Durs ve)

ves2DurVal :: [VoiceEvent] -> Int
ves2DurVal = sum . fmap ve2DurVal

-- validates _tupNotes contains integral count of tuplet, answers count
tup2CntTups :: Tuplet -> Int
tup2CntTups tup@Tuplet{..}
  | 0 /= (notesDurVal `rem` (_tupNum * unitDurVal)) = error msg
  | otherwise = notesDurVal `div` (_tupNum * unitDurVal)
  where
    unitDurVal  = dur2DurVal _tupDur
    notesDurVal = ves2DurVal (NE.toList _tupNotes)
    msg = "invalid tuplet: sum of dur vals " <> show notesDurVal <> " not divisible by num * durVal of unit: " <> show (_tupNum * unitDurVal) <> "\n" <> show tup 

-- Why doesn't NonEmpty expose this?
singleton :: a -> NE.NonEmpty a
singleton a = a NE.:| []

transpose :: Scale -> (Pitch,Octave) -> [Int] -> [(Pitch,Octave)]
transpose scale pr = map (xp scale pr) . scanl1 (+)

mtranspose :: Scale -> (Pitch,Octave) -> [Maybe Int] -> [Maybe (Pitch,Octave)]
mtranspose scale start = map (xp scale start <$>) . reverse . snd . foldl' f (0,[])
  where
    f (s,l) Nothing  = (s, Nothing:l)
    f (s,l) (Just i) = (s', Just s':l)
      where
        s' = s + i

-- sequentially transpose for Scale from start given mIntList until current (Pitch,Octave) equals or exceeds stop
seqMTranspose :: Scale -> NE.NonEmpty (Maybe Int) -> ((Pitch,Octave),(Pitch,Octave)) -> NE.NonEmpty (Maybe (Pitch,Octave))
seqMTranspose scale mIntList (start,stop)
  | intDir == GT && pitDir == GT = NE.unfoldr (f (<)) (0,0)
  | intDir == LT && pitDir == LT = NE.unfoldr (f (>)) (0,0)
  | intDir == EQ && pitDir == EQ = error $ "seqMTranpose intervals " <> show mIntList <> " and pitches " <> show start <> " " <> show stop <> " are both EQ "
  | otherwise = error $ "seqMTranspose int direction: " <> show intDir <> " " <> show mIntList <> " does not equal pitch direction " <> show pitDir <> " " <> show start <> " " <> show stop
  where
    f _ (0,0) = case NE.head mIntList of
                  Nothing -> (Nothing, Just (1,0))
                  Just i  -> (Just (xp scale start i),Just (1,i))
    f cmp (ix,prv) = case mPitOct of
                       Nothing -> (Nothing,Just (ix',prv'))
                       Just next -> if swap next `cmp` swap stop
                                    then (mPitOct,Just (ix',prv'))
                                    else (mPitOct,Nothing)
      where
        ix' = if ix == NE.length mIntList - 1 then 0 else ix + 1
        mPitOct = xp scale start . (+) prv <$> mInt
        prv' = maybe prv (prv +) mInt
        mInt = mIntList NE.!! ix
    intDir = tot `compare` 0
       where
         tot = sum (fromMaybe 0 <$> mIntList) -- (> 0), (< 0), or 0
    pitDir = start' `compare` stop'
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
    f (Just (p,o)) du a dy = VeNote (Note p o du (singleton a) dy NoSwell "" False)

genNotess :: [[Maybe (Pitch,Octave)]] -> [[Duration]] -> [[Accent]] -> [[Dynamic]] -> [[VoiceEvent]]
genNotess = zipWith4 genNotes

genNotesWithTies :: NE.NonEmpty (Maybe (Pitch,Octave)) -> NE.NonEmpty Duration -> NE.NonEmpty Accent -> NE.NonEmpty Dynamic -> NE.NonEmpty Bool -> NE.NonEmpty VoiceEvent
genNotesWithTies = neZipWith5 f
  where
    f Nothing du _ dy _ = VeRest $ Rest du dy
    f (Just (p,o)) du a dy sl = VeNote $ Note p o du (singleton a) dy NoSwell "" sl 

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

nes2arrs :: NE.NonEmpty (NE.NonEmpty a) -> [[a]]
nes2arrs = map NE.toList . NE.toList

ness2Marrss :: forall a . NE.NonEmpty (NE.NonEmpty (Maybe (Either a (NE.NonEmpty a)))) -> [[Maybe (Either a [a])]]
ness2Marrss = map (map (fmap (second NE.toList)) . NE.toList) . NE.toList

-- Cloned from https://hackage.haskell.org/package/base-4.14.1.0/docs/src/Data.List.NonEmpty.html#zipWith
neZipWith3 :: (a1 -> a2 -> a3 -> a4) -> NE.NonEmpty a1 -> NE.NonEmpty a2 -> NE.NonEmpty a3 -> NE.NonEmpty a4
neZipWith3 f ~(x1 NE.:| x1s) ~(x2 NE.:| x2s) ~(x3 NE.:| x3s) = f x1 x2 x3 NE.:| zipWith3 f x1s x2s x3s

neZipWith4 :: (a1 -> a2 -> a3 -> a4 -> a5) -> NE.NonEmpty a1 -> NE.NonEmpty a2 -> NE.NonEmpty a3 -> NE.NonEmpty a4 -> NE.NonEmpty a5
neZipWith4 f ~(x1 NE.:| x1s) ~(x2 NE.:| x2s) ~(x3 NE.:| x3s) ~(x4 NE.:| x4s) = f x1 x2 x3 x4 NE.:| zipWith4 f x1s x2s x3s x4s

neZipWith5 :: (a1 -> a2 -> a3 -> a4 ->a5 -> a6) -> NE.NonEmpty a1 -> NE.NonEmpty a2 -> NE.NonEmpty a3 -> NE.NonEmpty a4 -> NE.NonEmpty a5 -> NE.NonEmpty a6
neZipWith5 f ~(x1 NE.:| x1s) ~(x2 NE.:| x2s) ~(x3 NE.:| x3s) ~(x4 NE.:| x4s) ~(x5 NE.:| x5s) = f x1 x2 x3 x4 x5 NE.:| zipWith5 f x1s x2s x3s x4s x5s

neZipWith6 :: (a1 -> a2 -> a3 -> a4 ->a5 -> a6 -> a7) -> NE.NonEmpty a1 -> NE.NonEmpty a2 -> NE.NonEmpty a3 -> NE.NonEmpty a4 -> NE.NonEmpty a5 -> NE.NonEmpty a6 -> NE.NonEmpty a7
neZipWith6 f ~(x1 NE.:| x1s) ~(x2 NE.:| x2s) ~(x3 NE.:| x3s) ~(x4 NE.:| x4s) ~(x5 NE.:| x5s) ~(x6 NE.:| x6s) = f x1 x2 x3 x4 x5 x6 NE.:| zipWith6 f x1s x2s x3s x4s x5s x6s

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

-- Give the Int (could be a Nat) as an index counting
-- from the lowest to the highest pitch on the chromatic
-- scale in the range from four octaves below middle C to
-- to three octaves above middle C.
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
    normVeNote (VeNote (Note pit oct _ _ _ _ _ _)) = normalizePitchOctave pit oct
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

chunkByCounts :: [Int] -> [a] -> [[a]]
chunkByCounts counts items = chunk counts items []
  where
    chunk :: [Int] -> [a] -> [[a]] -> [[a]]
    chunk _ [] ret = ret
    chunk [] _ ret = ret
    chunk (n:ns) as ret
      | length as <= n = ret <> [take n as]
      | otherwise      = chunk ns (drop n as) (ret <> [take n as])

chunkByPairCounts :: [(Int,Int)] -> [a] -> [([a],[a])]
chunkByPairCounts counts items = chunk counts items []
  where
    chunk :: [(Int,Int)] -> [a] -> [([a],[a])] -> [([a],[a])]
    chunk _ [] ret = ret
    chunk [] _ ret = ret
    chunk ((x,y):ns) as ret
      | x + y <= length as = chunk ns (drop (x + y) as) (ret <> [(take x as,take y (drop x as))])
      | x     <= length as = ret <> [splitAt x as]
      | otherwise          = ret <> [(as,[])]
