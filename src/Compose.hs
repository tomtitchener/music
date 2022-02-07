
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compose (groupConfig2VoiceEvents
               , alignVoiceEventsDurations
               , mkVesTotDur
               , genSplitStaffVoc
               , tagTempo
               , ve2DurVal
               , ves2DurVal
               )  where

import Data.Bifunctor (second)
import Control.Lens 
import Control.Monad (foldM)
-- import Control.Monad.ListM (mapAccumM)
import Control.Monad.Extra (concatMapM)
import Data.Foldable
import Data.Function (on)
import Data.List (elemIndex, findIndex, groupBy, sort, sortBy, unfoldr, (\\), transpose)
import qualified Data.List.NonEmpty as NE
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Sequence (adjust, fromList)
import Data.Traversable (mapAccumL)
import Data.Tuple (swap)

import ComposeData
import Driver
       (Driver, randomElements, randomizeList, randomIndices, randomWeightedElement, searchConfigParam, searchMConfigParam)
import Types
import Utils hiding (transpose)

type PitOctOrPitOcts = Either (Pitch,Octave) [(Pitch,Octave)]

newtype Range = Range ((Pitch,Octave),(Pitch,Octave)) deriving (Eq, Show)

type ConfigMod = VoiceRuntimeConfig -> VoiceConfig -> Driver VoiceConfig

name2VoiceConfigMods :: M.Map String ConfigMod
name2VoiceConfigMods = M.fromList [("incrRandOcts",incrRandomizeMPitOctssOctaves)
                                   ,("decrRandOcts",decrNormalizeMPitOctssOctaves)
                                   ,("doubleDurs",doubleCfgDurs)]
                      
type VoiceEventsMod = VoiceRuntimeConfig -> [VoiceEvent] -> Driver [VoiceEvent]

name2VoiceEventsMods :: M.Map String VoiceEventsMod
name2VoiceEventsMods = M.fromList [("uniformAccs",uniformAccents)
                                  ,("fadeInAccs",fadeInAccents)
                                  ,("fadeInDyns",fadeInDynamics)
                                  ,("uniformDyns",uniformDynamics)
                                  ,("sectionDyns",sectionDynamics)
                                  ,("fadeOutDyns",fadeOutDynamics)]

-- Weight, action pairs for four segment example, all voices have same weight:
-- [[(0,-1),   (100,0),  (0,1)]      == 4,0 (((100 - (100 - (0 * (50/(4-1))))) / 2),(100 - (0 * (50/(4-1)))),((100 - (100 - (0 * (50/(4-1))))) / 2))
-- ,[(8.3,-1), (88.3,0), (8.3,1)]    == 4,1 (((100 - (100 - (1 * (50/(4-1))))) / 2),(100 - (1 * (50/(4-1)))),((100 - (100 - (1 * (50/(4-1))))) / 2))
-- ,[(16.6,-1),(66.6,0), (16.6,1)]   == 4,2 (((100 - (100 - (2 * (50/(4-1))))) / 2),(100 - (2 * (50/(4-1)))),((100 - (100 - (1 * (50/(4-1))))) / 2))
-- ,[(25,-1),  (50,0),   (25,1)]     == 4,3 (((100 - (100 - (3 * (50/(4-1))))) / 2),(100 - (3 * (50/(4-1)))),((100 - (100 - (1 * (50/(4-1))))) / 2))
-- Randomly tosses octave up, down, or leaves alone.
incrRandomizeMPitOctssOctaves :: ConfigMod
incrRandomizeMPitOctssOctaves = modMPitOctssOctaves mkIdWeightsIncr

-- complement incrNormalizeMPitOctssOctaves should start with maximal randomness and reduce incrementally
decrNormalizeMPitOctssOctaves :: ConfigMod
decrNormalizeMPitOctssOctaves = modMPitOctssOctaves mkIdWeightsDecr 

modMPitOctssOctaves :: (Int -> Int -> Int ) -> ConfigMod
-- modMPitOctssOctaves mkIdWeight vrtCfg vcx@VoiceConfigXPose{..}  = modAnyMPitOctssOctaves mkIdWeight vrtCfg _vcxmPOOrPOsss  <&> \mPitOctsss -> vcx & vcxmPOOrPOsss .~ mPitOctsss
modMPitOctssOctaves mkIdWeight vrtCfg vcx@VoiceConfigXPose{..}  = modAnyMPitOctssOctaves mkIdWeight vrtCfg _vcxmPOOrPOsss  <&> \mPitOctsss -> vcx { _vcxmPOOrPOsss = mPitOctsss}
modMPitOctssOctaves mkIdWeight vrtCfg vcr@VoiceConfigRepeat{..} = modAnyMPitOctssOctaves mkIdWeight vrtCfg _vcrmPOOrPOsss  <&> \mPitOctsss -> vcr { _vcrmPOOrPOsss = mPitOctsss}
modMPitOctssOctaves mkIdWeight vrtCfg vcl@VoiceConfigCell{..}   = modAnyMPitOctssOctaves mkIdWeight vrtCfg _vcclmPOOrPOsss <&> \mPitOctsss -> vcl { _vcclmPOOrPOsss = mPitOctsss}
modMPitOctssOctaves mkIdWeight vrtCfg vcc@VoiceConfigCanon{..}  = modAnyMPitOctssOctaves mkIdWeight vrtCfg _vccmPOOrPOsss  <&> \mPitOctsss -> vcc { _vccmPOOrPOsss = mPitOctsss}

modAnyMPitOctssOctaves :: (Int -> Int -> Int ) -> VoiceRuntimeConfig -> NE.NonEmpty (NE.NonEmpty (Maybe PitOctOrNEPitOcts)) -> Driver (NE.NonEmpty (NE.NonEmpty (Maybe PitOctOrNEPitOcts)))
modAnyMPitOctssOctaves mkIdWeight VoiceRuntimeConfig{..} = 
  traverse (traverse randomizeMPitOcts)
  where
    randomizeMPitOcts Nothing = pure Nothing
    randomizeMPitOcts (Just (Left pr)) = randomizeMPitOct pr <&> Just . Left
    randomizeMPitOcts (Just (Right prs)) = traverse randomizeMPitOct prs <&> Just . Right
    randomizeMPitOct (pit,oct) = randomWeightedElement weights <&> (\f -> (pit,f oct))
    idWeight  = mkIdWeight _vrcCntSegs _vrcNumSeg
    modWeight = (100 - idWeight) `div` 2
    weights   = [(modWeight,pred),(idWeight,id),(modWeight,succ)]

mkIdWeightsIncr :: Int -> Int -> Int
mkIdWeightsIncr      1      _  = 50
mkIdWeightsIncr cntSegs numSeg = 100 - (numSeg * (50 `div` (cntSegs - 1))) -- TBD: magic constant 50% of results to be modified at end

mkIdWeightsDecr :: Int -> Int -> Int
mkIdWeightsDecr      1      _  = 0
mkIdWeightsDecr cntSegs numSeg = 100 - ((cntSegs - (1 + numSeg)) * (50 `div` (cntSegs - 1))) -- TBD: magic constant 50% of results to be modified at end)

doubleCfgDurs :: ConfigMod
doubleCfgDurs vrtCfg vcx@VoiceConfigXPose{}  = mRunMod (vcx & vcxDurss  %~ doubleDurs) vrtCfg vcx
doubleCfgDurs vrtCfg vcr@VoiceConfigRepeat{} = mRunMod (vcr & vcrDurss  %~ doubleDurs) vrtCfg vcr
doubleCfgDurs vrtCfg vcc@VoiceConfigCell{}   = mRunMod (vcc & vcclDurss %~ doubleDurs) vrtCfg vcc
doubleCfgDurs vrtCfg vcc@VoiceConfigCanon{}  = mRunMod (vcc & vccDurss  %~ doubleDurs) vrtCfg vcc

mRunMod :: VoiceConfig -> VoiceRuntimeConfig -> VoiceConfig -> Driver VoiceConfig
mRunMod vcMod VoiceRuntimeConfig{..} vCfg  = do
  mod' <- searchMConfigParam (_vrcSctnPath <> ".dblCfgMod") <&> fromMaybe (_vrcCntSegs `div` _vrcCntVocs)
  pure $ if _vrcNumVoc * mod' <= _vrcNumSeg then vcMod else vCfg

doubleDurs :: NE.NonEmpty (NE.NonEmpty DurOrDurTuplet) -> NE.NonEmpty (NE.NonEmpty DurOrDurTuplet)
doubleDurs = (fmap . fmap) doubleDurOrDurTup

doubleDurOrDurTup :: DurOrDurTuplet -> DurOrDurTuplet
doubleDurOrDurTup = bimap (multDur 2) (multDurTuplet 2)

multDurTuplet :: Int -> DurTuplet -> DurTuplet
multDurTuplet i tup = tup & durtupUnitDuration %~ multDur i & durtupDurations %~ fmap (multDur i)

--homAnyListOfList :: NE.NonEmpty (NE.NonEmpty a) -> Driver (NE.NonEmpty (NE.NonEmpty a))
--homAnyListOfList xss = randomizeList (NE.toList (NE.toList <$> xss)) <&> singleton . NE.fromList . concat

fadeInAccents :: VoiceEventsMod
fadeInAccents VoiceRuntimeConfig{..} ves = do
  acc1 <- searchMConfigParam (_vrcSctnPath <> ".fadeInAcc1") <&> fromMaybe Staccato
  acc2 <- searchMConfigParam (_vrcSctnPath <> ".fadeInAcc2") <&> fromMaybe Staccatissimo
  traverse (fadeInAccent _vrcMNumVoc acc1 acc2) ves
  where
    fadeInAccent :: Maybe Int -> Accent -> Accent -> VoiceEvent -> Driver VoiceEvent
    fadeInAccent (Just _) acc1 _   ve@VeNote{}   = pure (ve & veNote . noteAccs %~ (acc1 NE.<|))
    fadeInAccent Nothing  _   acc2 ve@VeNote{}   = pure (ve & veNote . noteAccs %~ (acc2 NE.<|))
    fadeInAccent _        _   _    (VeRest rest) = pure $ VeRest rest
    fadeInAccent _        _   _    vEvent        = pure vEvent 
    
fadeInDynamics :: VoiceEventsMod
fadeInDynamics VoiceRuntimeConfig{..} ves = do
  dyn1 <- searchMConfigParam (_vrcSctnPath <> ".fadeInDyn1") <&> fromMaybe Forte
  dyn2 <- searchMConfigParam (_vrcSctnPath <> ".fadeInDyn2") <&> fromMaybe PPP
  pure $ maybe (tagFirstSoundDynamic dyn2 ves) (const $ tagFirstSoundDynamic dyn1 ves) _vrcMNumVoc

-- no need to repeat dynamic for each seg
uniformDynamics :: VoiceEventsMod
uniformDynamics VoiceRuntimeConfig{..} ves
  | _vrcNumSeg == 0 = searchMConfigParam (_vrcSctnPath <> ".uniformDyn") <&> fromMaybe PPP <&> flip tagFirstSoundDynamic ves
  | otherwise = pure ves

tagFirstSoundDynamic :: Dynamic -> [VoiceEvent] -> [VoiceEvent]
tagFirstSoundDynamic dyn ves = maybe ves (`tagDynForIdx` ves) (findIndex isVeSound ves)
  where
    tagDynForIdx idx = toList . adjust (tagDyn dyn) idx . fromList
    tagDyn dyn' ve@VeNote{}    = ve & veNote . noteDyn .~ dyn'
    tagDyn dyn' ve@VeRhythm{}  = ve & veRhythm . rhythmDyn .~ dyn'
    tagDyn dyn' ve@VeTuplet{}  = ve & veTuplet . tupNotes %~ (\notes -> tagDyn dyn' (NE.head notes) NE.:| NE.tail notes)
    tagDyn dyn' ve@VeChord{}   = ve & veChord . chordDyn .~ dyn'
    tagDyn dyn' (VeTremolo nt@NoteTremolo{})  = VeTremolo (nt & ntrNote . noteDyn .~ dyn')
    tagDyn dyn' (VeTremolo ct@ChordTremolo{})  = VeTremolo (ct & ctrLeftChord . chordDyn .~ dyn')
    tagDyn _    ve          = error $ "tagFirstDynamicNote: unexpected VoiceEvent: " <> show ve

isVeSound :: VoiceEvent -> Bool
isVeSound VeNote {}    = True
isVeSound VeRhythm {}  = True
isVeSound VeTuplet {}  = True
isVeSound VeChord {}   = True
isVeSound VeTremolo {} = True
isVeSound _            = False

isVeRest :: VoiceEvent -> Bool
isVeRest VeRest {} = True
isVeRest _         = False
    
sectionDynamics :: VoiceEventsMod
sectionDynamics VoiceRuntimeConfig{..} ves = 
  searchConfigParam (_vrcSctnPath <> ".sectionDyns") <&> flip tagFirstSoundDynamic ves . (NE.!! _vrcNumSeg)

fadeOutDynamics :: VoiceEventsMod
fadeOutDynamics _ = pure 

uniformAccents :: VoiceEventsMod
uniformAccents VoiceRuntimeConfig{..} ves = do
  acc <- searchMConfigParam (_vrcSctnPath <> ".uniformAccent") <&> fromMaybe Staccatissimo
  traverse (uniformAccent acc) ves
  where
    uniformAccent :: Accent -> VoiceEvent -> Driver VoiceEvent
    uniformAccent acc ve@VeNote{} = pure $ ve & veNote . noteAccs %~ (acc NE.<|)
    uniformAccent _   vEvent      = pure vEvent

-- Unfold repeated transpositions of [[Maybe Pitch]] across Range
-- matching up with repetitions of [[Duration]] and [[Accent] to generate VoiceEvent.
genXPose :: String -> [[DurOrDurTuplet]] -> [[Accent]] -> [[Maybe PitOctOrPitOcts]] -> Scale -> Range -> Driver [VoiceEvent] -- in practice, VeRest | VeNote | VeChord
genXPose path durOrDurTupss acctss mPrOrPrsss scale (Range (start,stop)) = do
  let rangeOrd   = swap stop `compare` swap start
  showVType::Int   <- searchMConfigParam (path <> ".showVType") <&> fromMaybe 1
  manyMIOrIssDiffs <- randomElements mPrOrPrsss <&> concatMap (mPrOrPrss2MIOrIsDiffs scale)
  manyDurOrDurTups <- randomElements durOrDurTupss  <&> concat
  manyAccts        <- randomElements acctss <&> concat
  let mSteps    = concatMap (map (fmap (either id minimum)) . mPrOrPrss2MIOrIsDiffs scale) mPrOrPrsss
      stepOrd   = sum (fromMaybe 0 <$> mSteps) `compare` 0
      compareOp = if stepOrd == rangeOrd && stepOrd /= EQ
                  then if stepOrd == LT then (<=) else (>=)
                  else error $ "invalid step order " <> show stepOrd <> " compared with range order " <> show rangeOrd
      mPrOrPrss = unfoldr (unfoldMPrOrPrss compareOp) (Left start,manyMIOrIssDiffs)
      ves       = unfoldr unfoldVEs (mPrOrPrss,manyDurOrDurTups,manyAccts)
  if 0 == showVType
  then pure ves 
  else pure $ appendAnnFirstNote "xpose" ves
  where
    unfoldMPrOrPrss cmp (Left prev,Just (Left step):mIOrIss)
      | swap next `cmp` swap stop = Nothing
      | otherwise = Just (Just (Left next),(Left next,mIOrIss))
      where
        next = xp scale prev step
    unfoldMPrOrPrss cmp (Left prev,Just (Right steps):mIOrIss)
      | swap (head nexts) `cmp` swap stop = Nothing
      | otherwise = Just (Just (Right nexts),(Right nexts,mIOrIss))
      where
        nexts = xp scale prev <$> steps
    unfoldMPrOrPrss cmp (Right prevs,Just (Left step):mIOrIss)
      | swap next `cmp` swap stop = Nothing
      | otherwise = Just (Just (Left next),(Left next,mIOrIss))
      where
        next = xp scale (head prevs) step
    unfoldMPrOrPrss cmp (Right prevs,Just (Right steps):mIOrIss)
      | swap (head nexts) `cmp` swap stop = Nothing
      | otherwise = Just (Just (Right nexts),(Right nexts,mIOrIss))
      where
        nexts = xp scale (head prevs) <$> steps
    unfoldMPrOrPrss _ (prev,Nothing:mIOrIss) = Just (Nothing,(prev,mIOrIss))
    unfoldMPrOrPrss _ (prev,mIOrIss) = error $ "invalid list of steps, (" <> show prev <> "," <> show (take 10 mIOrIss) <> ")"

genCell :: String -> [[DurOrDurTuplet]] -> [[Accent]] -> [[Maybe PitOctOrPitOcts]] -> Scale -> (Pitch,Octave) -> Int -> Driver [VoiceEvent]
genCell path durss acctss mPOOrPOsss _ _ maxDurVal = do
  showVType::Int <- searchMConfigParam (path <> ".showVType") <&> fromMaybe 1
  manyIs <- randomIndices (maximum [length durss,length acctss, length mPOOrPOsss])
  let (manyDurs,manyAccts,manyMPOOrPOss) = equalLengthLists ((durss !!) <$> manyIs,(acctss !!) <$> manyIs,(mPOOrPOsss !!) <$> manyIs)
      allDurOrDurTups = unfoldr (unfoldDurOrDurTups maxDurVal) (0,concat manyDurs)
      ves             = unfoldr unfoldVEs (concat manyMPOOrPOss,allDurOrDurTups,concat manyAccts)
  if 0 == showVType
  then pure ves 
  else pure $ appendAnnFirstNote "cell" ves
  where
    equalLengthLists (as:asss,bs:bss,cs:css) = (as':asss,bs':bss,cs':css)
      where
        (as',bs',cs') = equalList (as,bs,cs)
        equalList (xs,ys,zs) = (take l (cycle xs), take l (cycle ys), take l (cycle zs))
          where
            l = maximum [length xs,length ys,length zs]
    equalLengthLists (as,bs,cs) = error $ "equalLists unexpected uneven length lists: " <> show (as,bs,cs)
                                            
genRepeat :: String -> [[DurOrDurTuplet]] -> [[Accent]] -> [[Maybe PitOctOrPitOcts]] -> Int -> Driver [VoiceEvent]
genRepeat path durss acctss mPOOrPOsss maxDurVal = do
  durss'      <- freezeLists durss
  acctss'     <- freezeLists acctss
  mPOOrPOsss' <- freezeLists mPOOrPOsss
  genCanon path durss' acctss' mPOOrPOsss' maxDurVal 0 <&> replaceAnnFirstNote "repeat"

freezeLists :: [[a]] -> Driver [[a]]
freezeLists xss = randomizeList xss <&> (:[]) . concat

genCanon :: String -> [[DurOrDurTuplet]] -> [[Accent]] -> [[Maybe PitOctOrPitOcts]] -> Int -> Int -> Driver [VoiceEvent]
genCanon path durOrDurTupss acctss mPOOrPOsss maxDurVal rotVal = do
  showVType::Int   <- searchMConfigParam (path <> ".showVType") <&> fromMaybe 1
  manyMPOOrPOss    <- randomElements mPOOrPOsss <&> concatMap (rotN rotVal)
  manyDurOrDurTups <- randomElements durOrDurTupss  <&> concat
  manyAccts        <- randomElements acctss <&> concat
  let allDurOrDurTups = unfoldr (unfoldDurOrDurTups maxDurVal) (0,manyDurOrDurTups)
      ves             = unfoldr unfoldVEs (manyMPOOrPOss,allDurOrDurTups,manyAccts)
  if 0 == showVType
  then pure ves 
  else pure $ appendAnnFirstNote "canon" ves

mPrOrPrss2MIOrIsDiffs :: Scale -> [Maybe PitOctOrPitOcts] -> [Maybe (Either Int [Int])]
mPrOrPrss2MIOrIsDiffs scale =
  mIOrIs2MIOrIsDiffs . mPrOrPrss2MIOrIss . map (fmap (orderChords . bimap po2pi (fmap po2pi)))
  where
    po2pi = second octave2Int
    octaveInts = [-4,-3,-2,-1,0,1,2,3]
    octave2Int oct = maybe (error $ "octave2Int unrecognized octave: " <> show oct) (octaveInts !!) $ elemIndex oct [TwentyNineVBOct .. TwentyTwoVAOct]
    orderChords = second (sortBy (compare `on` swap))
    mPrOrPrss2MIOrIss = map (fmap (bimap (pitchInt2ScaleDegree scale) (fmap (pitchInt2ScaleDegree scale))))
    mIOrIs2MIOrIsDiffs = snd . mapAccumL accumDiff 0
      where
        accumDiff prev  Nothing          = (prev,Nothing)
        accumDiff prev (Just (Left i))   = (i,Just (Left (i - prev)))
        accumDiff prev (Just (Right is)) = (minimum is,Just (Right (flip (-) prev <$> is)))
        
type UnfoldVETup = ([Maybe PitOctOrPitOcts],[DurOrDurTuplet],[Accent])

-- Check enough mPrOrOrPrss to fill all _durtupDurations to make sure to always generate full tuplet
unfoldVEs :: UnfoldVETup -> Maybe (VoiceEvent,UnfoldVETup)
unfoldVEs (mPOOrPOs:mPOOrPOss,Left dur:durOrDurTups,accent:accents) = Just (mkNoteChordOrRest mPOOrPOs dur accent,(mPOOrPOss,durOrDurTups,accents))
unfoldVEs (mPOOrPOss,Right durTup:durOrDurTups,accents)
 | length mPOOrPOss < NE.length (_durtupDurations durTup) = Nothing
 | otherwise = Just (veTup,(mPOOrPOss',durOrDurTups,accents'))
  where
    (veTup,mPOOrPOss',accents') = mkVeTuplet mPOOrPOss durTup accents
unfoldVEs (_,_,_) = Nothing

mkNoteChordOrRest :: Maybe PitOctOrPitOcts -> Duration -> Accent -> VoiceEvent
mkNoteChordOrRest (Just (Left (p,o))) d a = VeNote (Note p o d (singleton a) NoDynamic NoSwell "" False)
mkNoteChordOrRest (Just (Right pos))  d a = VeChord (Chord (NE.fromList pos) d (singleton a) NoDynamic NoSwell "" False)
mkNoteChordOrRest Nothing             d _ = VeRest (Rest d NoDynamic "")

unfoldDurOrDurTups :: Int -> (Int, [DurOrDurTuplet]) -> Maybe (DurOrDurTuplet, (Int, [DurOrDurTuplet]))
unfoldDurOrDurTups maxDurVal (totDurVal,Left dur:durOrDurTups)
  | totDurVal > maxDurVal = error $ "unfoldDurs totDurVal: " <> show totDurVal <> " exceeds max: " <> show maxDurVal
  | totDurVal == maxDurVal = Nothing
  | otherwise = Just (adjNextDur,(totDurVal + adjNextDurVal,durOrDurTups))
    where
      nextDurVal = dur2DurVal dur
      adjNextDurVal = if totDurVal + nextDurVal <= maxDurVal then nextDurVal else nextDurVal - ((totDurVal + nextDurVal) - maxDurVal)
      adjNextDur = Left $ durVal2Dur adjNextDurVal
unfoldDurOrDurTups maxDurVal (totDurVal,Right durTup:durOrDurTups)
  | totDurVal > maxDurVal = error $ "unfoldDurs totDurVal: " <> show totDurVal <> " exceeds max: " <> show maxDurVal
  | totDurVal == maxDurVal = Nothing
  | otherwise = Just (adjNextDur,(totDurVal + adjNextDurVal,durOrDurTups))
    where
      nextDurVal = dur2DurVal (durTup2Dur durTup)
      adjNextDurVal = if totDurVal + nextDurVal <= maxDurVal then nextDurVal else nextDurVal - ((totDurVal + nextDurVal) - maxDurVal)
      adjNextDur = if adjNextDurVal == nextDurVal then Right durTup else Left (durVal2Dur adjNextDurVal)
      durTup2Dur DurTuplet{..} = durVal2Dur (getDurSum (sumDurs (replicate _durtupDenominator _durtupUnitDuration)))
unfoldDurOrDurTups _ (_,[]) = error "unfoldDurOrDurTups unexpected empty list of [DurOrDurTuplet]"

appendAnnFirstNote :: String -> [VoiceEvent] -> [VoiceEvent]
appendAnnFirstNote ann = annFirstEvent ann (\new old -> if null old then new else old <> ", " <> new)

prependAnnFirstNote :: String -> [VoiceEvent] -> [VoiceEvent]
prependAnnFirstNote ann = annFirstEvent ann (\new old -> if null old then new else  new <> ", " <> old)

replaceAnnFirstNote :: String -> [VoiceEvent] -> [VoiceEvent]
replaceAnnFirstNote ann = annFirstEvent ann const

annFirstEvent :: String -> (String -> String -> String ) -> [VoiceEvent] -> [VoiceEvent]
annFirstEvent ann append = snd . mapAccumL accumSeen False
  where
    accumSeen False (VeNote note)     = (True,VeNote   (note   & noteAnn   %~ append ann))
    accumSeen False (VeRest rest)     = (True,VeRest   (rest   & restAnn   %~ append ann))
    accumSeen False (VeSpacer spacer) = (True,VeSpacer (spacer & spacerAnn %~ append ann))
    accumSeen False (VeRhythm rhythm) = (True,VeRhythm (rhythm & rhythmAnn %~ append ann))
    accumSeen False (VeChord chord)   = (True,VeChord  (chord  & chordAnn  %~ append ann))
    accumSeen seen  event             = (seen,event)

rotN :: Int -> [a] -> [a]
rotN cnt as
  | cnt >= length as = error $ "rotN cnt: " <> show cnt <> " >= length as " <> show (length as)
  | otherwise = drop cnt as <> take cnt as

pitchInt2ScaleDegree :: Scale -> (Pitch,Int) -> Int
pitchInt2ScaleDegree Scale{..} (pitch,octOff) =
  maybe err (length pitches * octOff +) (elemIndex pitch pitches)
  where
    pitches = NE.toList _scPitches
    err = error $ "mPitch2ScaleDegree no pitch: " <> show pitch <> " in pitches for scale: " <> show pitches

mkVeTuplet :: [Maybe PitOctOrPitOcts] -> DurTuplet -> [Accent] -> (VoiceEvent,[Maybe PitOctOrPitOcts],[Accent])
mkVeTuplet mPOrPrss DurTuplet{..} accents = (verifyVeTuplet veTup,drop cntDurs mPOrPrss,drop cntDurs accents)
  where
    ves = zipWith3 mkNoteChordOrRest mPOrPrss' (NE.toList _durtupDurations) accents'
    veTup = VeTuplet (Tuplet _durtupNumerator _durtupDenominator _durtupUnitDuration (NE.fromList ves))
    mPOrPrss' = take cntDurs mPOrPrss
    accents' = take cntDurs accents
    cntDurs = length _durtupDurations

verifyVeTuplet :: VoiceEvent -> VoiceEvent
verifyVeTuplet (VeTuplet tuplet) = if tup2CntTups tuplet /= 0 then VeTuplet tuplet else error "verifyVeTuplet shouldn't get here"
verifyVeTuplet ve = ve

voiceConfig2VoiceEvents :: String -> VoiceConfig -> Driver [VoiceEvent]
voiceConfig2VoiceEvents path VoiceConfigXPose{..} =
  genXPose path (nes2arrs _vcxDurss) (nes2arrs _vcxAcctss)  (ness2Marrss _vcxmPOOrPOsss) _vcxScale (Range _vcxRange)
voiceConfig2VoiceEvents path VoiceConfigRepeat{..} =
  genRepeat path (nes2arrs _vcrDurss) (nes2arrs _vcrAcctss) (ness2Marrss _vcrmPOOrPOsss) _vcrDurVal
voiceConfig2VoiceEvents path VoiceConfigCell{..} =
  genCell path (nes2arrs _vcclDurss) (nes2arrs _vcclAcctss) (ness2Marrss _vcclmPOOrPOsss) _vcclScale _vcclRegister _vcclDurVal
voiceConfig2VoiceEvents path VoiceConfigCanon{..} =
  genCanon path (nes2arrs _vccDurss) (nes2arrs _vccAcctss) (ness2Marrss _vccmPOOrPOsss) _vccDurVal _vccRotVal

ves2VeRests :: [VoiceEvent] -> [VoiceEvent]
ves2VeRests = fmap ve2VeRest
  where
    ve2VeRest (VeNote Note{..})   = VeRest (Rest _noteDur NoDynamic _noteAnn)
    ve2VeRest (VeChord Chord{..}) = VeRest (Rest _chordDur NoDynamic _chordAnn)
    ve2VeRest ve                  = ve

applyMConfigMods :: Maybe (NE.NonEmpty String) -> (VoiceRuntimeConfig, VoiceConfig) -> Driver (VoiceRuntimeConfig,VoiceConfig)
applyMConfigMods mNames pr = applyMMods mNames pr (applyMod name2VoiceConfigMods) <&> (fst pr,)

applyMVoiceEventsMods :: Maybe (NE.NonEmpty String) -> (VoiceRuntimeConfig,[VoiceEvent]) -> Driver [VoiceEvent]
applyMVoiceEventsMods mNames pr = applyMMods mNames pr (applyMod name2VoiceEventsMods)

applyMMods :: forall a. Maybe (NE.NonEmpty String) -> (VoiceRuntimeConfig,a) -> (String -> (VoiceRuntimeConfig,a) -> Driver a) -> Driver a
applyMMods Nothing (_,as) _ = pure as
applyMMods (Just modNames) pr applyAsMods = foldM foldMf pr (NE.toList modNames) <&> snd
  where
    foldMf :: (VoiceRuntimeConfig,a) -> String -> Driver (VoiceRuntimeConfig,a)
    foldMf pr' modName = applyAsMods modName pr' <&> (fst pr',)

applyMod :: M.Map String (VoiceRuntimeConfig -> a -> Driver a) -> String -> (VoiceRuntimeConfig,a) -> Driver a
applyMod m modName pr =
  case M.lookup modName m of
    Nothing -> error $ "applyMod:  no value for name " <> modName
    Just f  -> uncurry f pr

sectionConfig2VoiceEvents :: SectionConfig -> Driver [[VoiceEvent]]
sectionConfig2VoiceEvents (SectionConfigNeutral scnPath cntSegs mSctnName mConfigMods mVoiceEventsMods voiceConfigs) = do
  traverse (traverse (applyMConfigMods mConfigMods)) prs >>= traverse (concatMapM cvtAndApplyMod) <&> addSecnName scnName
  where
    cvtAndApplyMod (rtTup,cfgTup) = voiceConfig2VoiceEvents scnPath cfgTup >>= applyMVoiceEventsMods mVoiceEventsMods . (rtTup,)
    scnName = fromMaybe "neutral" mSctnName
    cntVocs = length voiceConfigs
    segRuntimeTupss = chunksOf cntSegs [VoiceRuntimeConfig scnPath Nothing cntVocs numVoc cntSegs numSeg | numVoc <- [0..cntVocs - 1], numSeg <- [0..cntSegs - 1]]
    prs = zipWith (\rtups cfgtup -> (,cfgtup) <$> rtups) segRuntimeTupss (NE.toList voiceConfigs)
sectionConfig2VoiceEvents (SectionConfigHomophony scnPath cntSegs mSctnName mConfigMods mVoiceEventsMods voiceConfigs) = do
  traverse (traverse (applyMConfigMods mConfigMods)) prs >>= traverse (concatMapM cvtAndApplyMod) <&> addSecnName scnName
  where
    cvtAndApplyMod (rt,cfg) = freezeConfig cfg >>= voiceConfig2VoiceEvents scnPath >>= applyMVoiceEventsMods mVoiceEventsMods . (rt,)
    scnName = fromMaybe "homophony" mSctnName
    cntVocs = length voiceConfigs
    segRuntimeTupss = chunksOf cntSegs [VoiceRuntimeConfig scnPath Nothing cntVocs numVoc cntSegs numSeg | numVoc <- [0..cntVocs - 1], numSeg <- [0..cntSegs - 1]]
    prs = zipWith (\rtups cfgtup -> (,cfgtup) <$> rtups) segRuntimeTupss (NE.toList voiceConfigs)
sectionConfig2VoiceEvents (SectionConfigFadeIn scnPath fadeIxs mSctnName mConfigMods mVoiceEventsMods voiceConfigs) =
  foldM foldMf ([],replicate cntVocs []) fadeIxs <&> addSecnName scnName . snd
  where
    scnName = fromMaybe "fade in" mSctnName
    cntSegs = length fadeIxs
    cntVocs = length voiceConfigs
    mkVocRTT nSeg nVoc =  VoiceRuntimeConfig scnPath Nothing cntVocs nVoc cntSegs nSeg
    foldMf (seenNumVocs,prevEventss) numVoc = do
      let numSeg = length seenNumVocs
          vocCfgTup = voiceConfigs NE.!! numVoc
          vocRTTup  = VoiceRuntimeConfig scnPath (Just numVoc) cntVocs numVoc cntSegs numSeg
      seenVEs <- applyMConfigMods mConfigMods (vocRTTup,vocCfgTup) >>= voiceConfig2VoiceEvents scnPath . snd >>= applyMVoiceEventsMods mVoiceEventsMods . (vocRTTup,)
      let vocRunTups = mkVocRTT numSeg <$> seenNumVocs
          vocCfgTups = (voiceConfigs NE.!!) <$> seenNumVocs
      seenNumVocsEventss <- traverse (applyMods scnPath  mConfigMods mVoiceEventsMods) (zip vocRunTups vocCfgTups)
      let seenNumVocVEs      = (numVoc,seenVEs)
          allSeenNumVocs     = numVoc:seenNumVocs
          allUnseenNumVocs   = [0..(cntVocs - 1)] \\ allSeenNumVocs
          unseenNumVocEventss = (,ves2VeRests seenVEs) <$> allUnseenNumVocs
          newEventss          = snd <$> sort (seenNumVocVEs:seenNumVocsEventss <> unseenNumVocEventss)
      pure (allSeenNumVocs,zipWith (<>) prevEventss newEventss)
sectionConfig2VoiceEvents (SectionConfigFadeOut scnPath fadeIxs mSctnName mConfigMods mVoiceEventsMods voiceConfigs) = do
  let initNumVocs = [0..cntVocs - 1]
      vocRunTups = mkVocRTT 0 <$> initNumVocs
      vocCfgTups = (voiceConfigs NE.!!) <$> initNumVocs
  inits <- traverse (applyMods scnPath mConfigMods mVoiceEventsMods) (zip vocRunTups vocCfgTups)
  ves   <- foldM foldMf ([],snd <$> inits) fadeIxs <&> addSecnName scnName . snd
  let veDurs = ves2DurVal <$> ves
  pure $ zipWith3 (mkVesTotDur (maximum veDurs)) veDurs timeSigs ves
  where
    cntSegs = length fadeIxs
    cntVocs = length voiceConfigs
    scnName = fromMaybe "fade out" mSctnName
    timeSigs = NE.toList $ _vccTime <$> voiceConfigs
    mkVocRTT nSeg nVoc =  VoiceRuntimeConfig scnPath Nothing cntVocs nVoc cntSegs nSeg
    foldMf (seenNumVocs,prevEventss) numVoc = do
      let numSeg = 1 + length seenNumVocs
          seenNumVocs' = numVoc:seenNumVocs
          unseenNumVocs = [0..(cntVocs - 1)] \\ seenNumVocs'
          vocRunTups = mkVocRTT numSeg <$> unseenNumVocs
          vocCfgTups =  (voiceConfigs NE.!!) <$> unseenNumVocs
      unseenNumVocsEventss <- traverse (applyMods scnPath mConfigMods mVoiceEventsMods) (zip vocRunTups vocCfgTups)
      let seenNumVocsEmpties::[(Int,[VoiceEvent])] = (,[]) <$> seenNumVocs'
          newEventss = snd <$> sort (seenNumVocsEmpties <> unseenNumVocsEventss)
      pure (seenNumVocs',zipWith (<>) prevEventss newEventss)

applyMods :: String -> Maybe (NE.NonEmpty String) -> Maybe (NE.NonEmpty String) -> (VoiceRuntimeConfig,VoiceConfig) -> Driver (Int,[VoiceEvent])
applyMods path mConfigMods mVoiceEventsMods pr =
  applyMConfigMods mConfigMods pr >>= voiceConfig2VoiceEvents path . snd >>= applyMVoiceEventsMods mVoiceEventsMods . (vrt,) <&> (_vrcNumVoc vrt,)
  where
    vrt = fst pr

addSecnName :: String -> [[VoiceEvent]] -> [[VoiceEvent]]
addSecnName scnName voices = prependAnnFirstNote scnName <$> voices

genSplitStaffVoc :: Instrument -> KeySignature -> TimeSignature -> [VoiceEvent] -> Voice
genSplitStaffVoc instr keySig timeSig ves
  = SplitStaffVoice instr (VeKeySignature keySig NE.<| VeTimeSignature timeSig NE.<| NE.fromList ves)

groupConfig2VoiceEvents :: GroupConfig -> Driver [[[VoiceEvent]]]
groupConfig2VoiceEvents (GroupConfigNeutral _ _ secCfgs) =
  traverse sectionConfig2VoiceEvents (NE.toList secCfgs)
groupConfig2VoiceEvents (GroupConfigEvenEnds _ _ secCfgs) =
  traverse sectionConfig2VoiceEvents (NE.toList secCfgs) >>= extendVoicesEvents (NE.last secCfgs)

-- Repeatedly add [[VoiceEvent]] for last section to input until all [[VoiceEvent]] are the same
-- total duration.  Tricky bit is that sectionConfig2VoiceEvents may not add [VoiceEvent] of
-- sufficiently long sum duration to match difference in length needed, in which case, loop.
-- To polish, trim final result to end at the bar line.
extendVoicesEvents :: SectionConfig -> [[[VoiceEvent]]] -> Driver [[[VoiceEvent]]]
extendVoicesEvents sectionConfig vesssIn = 
  let timeSig = sectionCfg2TimeSignature sectionConfig
      go vesss = do
        let vess = concat <$> transpose vesss
            veLens = ves2DurVal <$> vess
        if sum veLens == length veLens * head veLens
        then do
          pure vesss
        else do
          let maxLen = bump2FullBar timeSig $ maximum veLens
              veLenDiffs = (-) maxLen <$> veLens
          vessNew <- sectionConfig2VoiceEvents sectionConfig <&> zipWith maybeTrimVes veLenDiffs
          go $ vesss <> [vessNew]
  in 
    go vesssIn

-- Adjust partial bar to next full bar so len diffs above are always positive
-- and we always end at the beginning of a bar (hack).
bump2FullBar :: TimeSignature -> Int -> Int
bump2FullBar timeSig totLen =
  totLen + bumpLen
  where
    beatLen = dur2DurVal (timeSig2Denom timeSig)
    barLen  = timeSig2Num timeSig * beatLen
    remLen  = totLen `rem` barLen
    bumpLen  = if 0 == remLen then 0 else barLen - remLen

-- No need to trim for [VoiceEvent] that's already the longest (lenDiff is 0)
-- or when sum of duration for new [VoiceEvent] is <= lenDiff
maybeTrimVes :: Int -> [VoiceEvent] -> [VoiceEvent]
maybeTrimVes lenDiff vesIn
  | lenDiff == 0 = []
  | lenDiff <  0 = error $ "maybeTrimVes negative lenDiff: " <> show lenDiff
  | otherwise = 
    case vesLen `compare` lenDiff of
      LT -> vesIn
      EQ -> vesIn
      GT -> trim lenDiff vesIn
    where
      vesLen = ves2DurVal vesIn
      trim :: Int -> [VoiceEvent] -> [VoiceEvent]
      trim lenTot = snd . foldl' tr (lenTot,[])
      tr :: (Int,[VoiceEvent]) -> VoiceEvent -> (Int,[VoiceEvent])
      tr (0,ves) _  = (0,ves)
      tr (n,ves) ve = (n',ves <> [ve'])
        where
          veLen  = ve2DurVal ve
          n'     = if n >= veLen then n - veLen else 0
          veLen' = if n >= veLen then veLen else n
          -- swapVeLens squashes Tuplet to Rest:  only swap when needed
          ve'    = if veLen == veLen' then ve else swapVeLens (durVal2Dur veLen') ve

-- maxLen and vesLen are in 128th notes
-- maxLen is target length so all voices are equal length
-- vesLen is actual length maybe same as maxLen
mkVesTotDur :: Int -> Int -> TimeSignature -> [VoiceEvent] -> [VoiceEvent]
mkVesTotDur maxLen vesLen timeSig ves =
  ves <> (spacerOrRest <$> addEndDurs timeSig vesLen addLen)
  where
    beatLen = dur2DurVal (timeSig2Denom timeSig)
    barLen  = timeSig2Num timeSig * beatLen
    remBar  = if maxLen `rem` barLen == 0 then 0 else barLen - (maxLen `rem` barLen)
    addLen  = if maxLen > vesLen then (maxLen - vesLen) + remBar else remBar
    spacerOrRest = if isSpacer (last ves) then VeSpacer . (\dur -> Spacer dur NoDynamic "") else VeRest . (\dur -> Rest dur NoDynamic "")

isSpacer :: VoiceEvent -> Bool
isSpacer VeSpacer {} = True
isSpacer _           = False

tagTempo :: Tempo -> [Voice] -> [Voice]
tagTempo tempo (v1:rest) = tagVoice v1:rest
  where
    tagVoice ::  Voice -> Voice
    tagVoice PitchedVoice {..}                  = PitchedVoice _ptvInstrument (VeTempo tempo NE.<| _ptvVoiceEvents)
    tagVoice PercussionVoice {..}               = PercussionVoice _pcvInstrument (VeTempo tempo NE.<| _pcvVoiceEvents)
    tagVoice (PolyVoice instr (ves NE.:| vess)) = PolyVoice instr ((VeTempo tempo NE.<| ves) NE.:| vess)
    tagVoice SplitStaffVoice {..}               = SplitStaffVoice _ssvInstrument (VeTempo tempo NE.<| _ssvVoiceEvents)
    tagVoice (VoiceGroup (v1' NE.:| r))         = VoiceGroup (tagVoice v1' NE.:| r)
tagTempo _ vs = vs

-- apportion [VeRest Rest] or [VeNote Note] durations according to place in bar given time signature
-- 1) map [VoiceEvent] to [[VoiceEvent]] by [[Left Note]..[Right Rest]]
-- 2) fold over uniform [VoiceEvent] into [VoiceEvent] according to current position
--    in total [VoiceEvent] by 1/128th notes mapped into position within bar, keeping
--    track of position by first element in pair, output in second element of pair
--    2a) for [VeRest Rest], sum vals for all contiguous rests, call addEndDurs given
--        time signature and current position and map all to Right Rest
--    2b) for [VeNote Note], fold again over each calling addEndDurs to break into tied
--         Left Note given time signature and current position
--    2c) for all other VoiceEvent, just pass through unchanged
--        note the only other VoiceEvent with a tie is Chord, though
--        there should also eventually be one for Rhythm
alignVoiceEventsDurations :: TimeSignature -> [VoiceEvent] -> [VoiceEvent]
alignVoiceEventsDurations timeSig ves =
  concat . snd $ mapAccumL adjVEsDurs 0 (groupBy ((==) `on` isVeRest) ves)
  where
    -- adjust [VoiceEvent] durations by time signature by current length at start of list
    adjVEsDurs :: Int -> [VoiceEvent] -> (Int,[VoiceEvent])
    -- contiguous rests can just use durations from addEndDurs, no need for ties
    adjVEsDurs curLen rests@((VeRest _):_) =
      (curLen + addLen,newRests)
      where
        addLen = ves2DurVal rests
        durs = addEndDurs timeSig curLen addLen
        newRests = VeRest . (\dur -> Rest dur NoDynamic "") <$> durs
    -- for notes and chords (eventually rhythms), add ties 
    adjVEsDurs curLen allVes =
      second concat $ mapAccumL adjVEDur curLen allVes
      where
        adjVEDur :: Int -> VoiceEvent -> (Int,[VoiceEvent])
        adjVEDur curLen' (VeNote note@Note{..}) =
          (curLen' + addLen,stripAnnotations . fixTies $ newNotes)
          where
            addLen = dur2DurVal _noteDur
            durs = addEndDurs timeSig curLen' addLen
            newNotes = VeNote . (\dur -> note {_noteDur = dur}) <$> durs
        adjVEDur curLen' (VeChord chord@Chord{..}) =
          (curLen' + addLen,stripAnnotations . fixTies $ newChords)
          where
            addLen = dur2DurVal _chordDur
            durs = addEndDurs timeSig curLen' addLen
            newChords = VeChord . (\dur -> chord {_chordDur = dur}) <$> durs
        adjVEDur curLen' ve = (curLen' + ve2DurVal ve,[ve])

fixTies :: [VoiceEvent] -> [VoiceEvent]
fixTies ves
  | length ves < 2 = ves
  | length ves == 2 = [firstVE,lastVE]
  | otherwise = [firstVE] <> midVEs <> [lastVE]
  where
    firstVE = fixVoiceEventTie True False (head ves)
    lastVE  = fixVoiceEventTie False True (last ves)
    midVEs  = fixVoiceEventTie False False <$> drop 1 (init ves)
            
stripAnnotations :: [VoiceEvent] -> [VoiceEvent]
stripAnnotations ves
  | length ves < 2 = ves
  | otherwise      = head ves:(stripAnnotation <$> tail ves)
        
-- tied-to voice events have no annotation
stripAnnotation :: VoiceEvent -> VoiceEvent
stripAnnotation (VeNote note@Note{}) = VeNote $ note & noteAccs .~ singleton NoAccent & noteDyn .~ NoDynamic & noteSwell .~ NoSwell & noteAnn .~ ""
stripAnnotation ve                   = ve

-- first bool is True for first element in list, second bool is True for last element in list
-- for first note, want tie and annotations, for middle notes,
-- for middle notes, want tie and no annotations
-- for last note, want no tie and no annotations
fixVoiceEventTie :: Bool -> Bool -> VoiceEvent -> VoiceEvent
fixVoiceEventTie True  False ve@VeNote{}           = ve & veNote . noteTie .~ True 
fixVoiceEventTie False tie   (VeNote note@Note{})  = VeNote $ note & noteAccs .~ singleton NoAccent & noteDyn .~ NoDynamic & noteSwell .~ NoSwell & noteAnn .~ "" & noteTie .~ not tie
fixVoiceEventTie True  False ve@VeChord{}          = ve & veChord . chordTie .~ True
fixVoiceEventTie False tie   (VeChord chd@Chord{}) = VeChord $ chd & chordAccs .~ singleton NoAccent & chordDyn .~ NoDynamic & chordSwell .~ NoSwell & chordTie .~ not tie 
fixVoiceEventTie True  False (VeRest rest)         = VeRest rest {-- TBD: { _restTie = True } --}
fixVoiceEventTie False _     ve@VeRest{}           = ve & veRest . restDyn .~ NoDynamic {-- TBD: ,_restTie = not tie --}
fixVoiceEventTie _     _     ve                    = ve

swapVeLens :: Duration -> VoiceEvent -> VoiceEvent
swapVeLens dur ve@VeNote{}   = ve & veNote   . noteDur   .~ dur
swapVeLens dur ve@VeRest{}   = ve & veRest   . restDur   .~ dur
swapVeLens dur ve@VeChord{}  = ve & veChord  . chordDur  .~ dur
swapVeLens dur ve@VeRhythm{} = ve & veRhythm . rhythmDur .~ dur
swapVeLens dur ve@VeSpacer{} = ve & veSpacer . spacerDur .~ dur
swapVeLens dur VeTuplet{}    = VeRest $ Rest dur NoDynamic ""
swapVeLens _   ve            = ve

freezeNELists :: NE.NonEmpty (NE.NonEmpty a) -> Driver (NE.NonEmpty (NE.NonEmpty a))
freezeNELists xss = randomizeList (nes2arrs xss) <&> singleton . NE.fromList . concat

freezeConfig :: VoiceConfig -> Driver VoiceConfig
freezeConfig vcx@VoiceConfigXPose{..}  = do
  durss      <- freezeNELists _vcxDurss
  acctss     <- freezeNELists _vcxAcctss
  mPOOrPOsss <- freezeNELists _vcxmPOOrPOsss
  pure $ vcx & vcxDurss .~ durss & vcxAcctss .~ acctss & vcxmPOOrPOsss .~ mPOOrPOsss
freezeConfig vcr@VoiceConfigRepeat{..} = do
  durss      <- freezeNELists _vcrDurss
  acctss     <- freezeNELists _vcrAcctss
  mPOOrPOsss <- freezeNELists _vcrmPOOrPOsss
  pure $ vcr & vcrDurss .~ durss & vcrAcctss .~ acctss & vcrmPOOrPOsss .~ mPOOrPOsss 
freezeConfig vcl@VoiceConfigCell{..} = do
  durss      <- freezeNELists _vcclDurss
  acctss     <- freezeNELists _vcclAcctss
  mPOOrPOsss <- freezeNELists _vcclmPOOrPOsss
  pure $ vcl & vcclDurss .~ durss & vcclAcctss .~ acctss & vcclmPOOrPOsss .~ mPOOrPOsss
freezeConfig vcc@VoiceConfigCanon{..}  = do
  durss      <- freezeNELists _vccDurss
  acctss     <- freezeNELists _vccAcctss
  mPOOrPOsss <- freezeNELists _vccmPOOrPOsss
  pure $ vcc & vccDurss .~ durss & vccAcctss .~ acctss & vccmPOOrPOsss .~ mPOOrPOsss
    
sectionCfg2TimeSignature :: SectionConfig -> TimeSignature
sectionCfg2TimeSignature SectionConfigNeutral{..}   = voiceCfg2TimeSignature (NE.head _scnVoices)
sectionCfg2TimeSignature SectionConfigHomophony{..} = voiceCfg2TimeSignature (NE.head _schVoices)
sectionCfg2TimeSignature SectionConfigFadeIn{..}    = voiceCfg2TimeSignature (NE.head _scfiVoices)
sectionCfg2TimeSignature SectionConfigFadeOut{..}   = voiceCfg2TimeSignature (NE.head _scfoVoices)

voiceCfg2TimeSignature :: VoiceConfig -> TimeSignature
voiceCfg2TimeSignature VoiceConfigXPose{..}  = _vcxTime
voiceCfg2TimeSignature VoiceConfigRepeat{..} = _vcrTime
voiceCfg2TimeSignature VoiceConfigCell{..}   = _vcclTime
voiceCfg2TimeSignature VoiceConfigCanon{..}  = _vccTime
