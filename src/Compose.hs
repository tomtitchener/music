{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compose (groupConfig2VoiceEvents
               ,alignVoiceEventsDurations
               ,mkVesTotDur
               ,genSplitStaffVoc
               ,tagTempo
               ,ve2DurVal
               ,ves2DurVal
               ) where

import Data.Bifunctor (second)
import Control.Lens hiding (both)
import Control.Monad (foldM)
import Control.Monad.Extra (concatMapM)
import Data.Foldable
import Data.Function (on)
import Data.List (elemIndex, findIndex, findIndices, groupBy, sortBy, unfoldr, transpose)
import qualified Data.List.NonEmpty as NE
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, catMaybes)
import Data.Sequence (adjust', fromList)
import Data.Traversable (mapAccumL)
import Data.Tuple (swap)
import Data.Tuple.Extra (both, first, fst3, snd3, thd3)
import Safe (lastMay)

import ComposeData
import Driver
       (Driver, randomElements, randomizeList, randomIndices, randomWeightedElement, searchConfigParam, searchMConfigParam)
import Lily (accent2Name)
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
                                  ,("voicesDyn",voicesDynamics)
                                  ,("uniformDyns",uniformDynamics)
                                  ,("sectionDyns",sectionDynamics)
                                  ,("fadeOutDyns",fadeOutDynamics)
                                  ,("sustainNotes",sustainNotes)]

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
modMPitOctssOctaves mkIdWeight vrtCfg vcx@VoiceConfigXPose{..}    = modAnyMPitOctssOctaves mkIdWeight vrtCfg _vcxmPOOrPOss  <&> \mPOOrPOss -> vcx { _vcxmPOOrPOss  = mPOOrPOss }
modMPitOctssOctaves mkIdWeight vrtCfg vcr@VoiceConfigRepeat{..}   = modAnyMPitOctssOctaves mkIdWeight vrtCfg _vcrmPOOrPOss  <&> \mPOOrPOss -> vcr { _vcrmPOOrPOss  = mPOOrPOss }
modMPitOctssOctaves mkIdWeight vrtCfg vcv@VoiceConfigVerbatim{..} = modAnyMPitOctssOctaves mkIdWeight vrtCfg _vcvmPOOrPOss  <&> \mPOOrPOss -> vcv { _vcvmPOOrPOss  = mPOOrPOss }
modMPitOctssOctaves mkIdWeight vrtCfg vcl@VoiceConfigCell{..}     = modAnyMPitOctssOctaves mkIdWeight vrtCfg _vcclmPOOrPOss <&> \mPOOrPOss -> vcl { _vcclmPOOrPOss = mPOOrPOss }
modMPitOctssOctaves mkIdWeight vrtCfg vcc@VoiceConfigCanon{..}    = modAnyMPitOctssOctaves mkIdWeight vrtCfg _vccmPOOrPOss  <&> \mPOOrPOss -> vcc { _vccmPOOrPOss  = mPOOrPOss }

type MPitOctOrNEPitOctsss = NE.NonEmpty (NE.NonEmpty (Maybe PitOctOrNEPitOcts))

modAnyMPitOctssOctaves :: (Int -> Int -> Int ) -> VoiceRuntimeConfig -> MPitOctOrNEPitOctsss -> Driver MPitOctOrNEPitOctsss
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
doubleCfgDurs vrtCfg vcx@VoiceConfigXPose{}    = mRunDoubleCfgMod (vcx & vcxDurss  %~ doubleDurs) vrtCfg vcx
doubleCfgDurs vrtCfg vcr@VoiceConfigRepeat{}   = mRunDoubleCfgMod (vcr & vcrDurss  %~ doubleDurs) vrtCfg vcr
doubleCfgDurs vrtCfg vcv@VoiceConfigVerbatim{} = mRunDoubleCfgMod (vcv & vcvDurss  %~ doubleDurs) vrtCfg vcv
doubleCfgDurs vrtCfg vcc@VoiceConfigCell{}     = mRunDoubleCfgMod (vcc & vcclDurss %~ doubleDurs) vrtCfg vcc
doubleCfgDurs vrtCfg vcc@VoiceConfigCanon{}    = mRunDoubleCfgMod (vcc & vccDurss  %~ doubleDurs) vrtCfg vcc

mRunDoubleCfgMod :: VoiceConfig -> VoiceRuntimeConfig -> VoiceConfig -> Driver VoiceConfig
mRunDoubleCfgMod vcMod VoiceRuntimeConfig{..} vCfg  = do
  mod' <- searchMConfigParam (_vrcSctnPath <> ".dblCfgMod") <&> fromMaybe (_vrcCntSegs `div` _vrcCntVocs)
  pure $ if _vrcNumVoc * mod' <= _vrcNumSeg then vcMod else vCfg

doubleDurs :: NE.NonEmpty (NE.NonEmpty DurOrDurTuplet) -> NE.NonEmpty (NE.NonEmpty DurOrDurTuplet)
doubleDurs = (fmap . fmap) doubleDurOrDurTup

doubleDurOrDurTup :: DurOrDurTuplet -> DurOrDurTuplet
doubleDurOrDurTup = bimap (* mkDurationVal 2) (multDurTuplet 2)

multDurTuplet :: Int -> DurTuplet -> DurTuplet
multDurTuplet i tup = tup & durtupUnitDuration %~ multDur i & durtupDurations %~ fmap (* mkDurationVal i)

--homAnyListOfList :: NE.NonEmpty (NE.NonEmpty a) -> Driver (NE.NonEmpty (NE.NonEmpty a))
--homAnyListOfList xss = randomizeList (NE.toList (NE.toList <$> xss)) <&> singleton . NE.fromList . concat

-- TBD: annotate first note with "sempre <accent>", would be nice not to repeat annotation for non-highlighted segments
-- uniform accents are specific to midi, score gets annotation
fadeInAccents :: VoiceEventsMod
fadeInAccents VoiceRuntimeConfig{..} ves = do
  acc1 <- searchMConfigParam (_vrcSctnPath <> ".fadeInAcc1") <&> fromMaybe Staccato
  acc2 <- searchMConfigParam (_vrcSctnPath <> ".fadeInAcc2") <&> fromMaybe Staccatissimo
  traverse (fadeInAccent _vrcMNumVoc acc1 acc2) ves
  where
    fadeInAccent :: Maybe Int -> Accent -> Accent -> VoiceEvent -> Driver VoiceEvent
    fadeInAccent (Just _) acc1 _    ve@VeNote{}  = pure (ve & veNote . noteMidiCtrls %~ (MidiCtrlAccent acc1 :))
    fadeInAccent Nothing  _    acc2 ve@VeNote{}  = pure (ve & veNote . noteMidiCtrls %~ (MidiCtrlAccent acc2 :))
    fadeInAccent (Just _) acc1 _    ve@VeChord{} = pure (ve & veChord . chordMidiCtrls %~ (MidiCtrlAccent acc1 :))
    fadeInAccent Nothing  _    acc2 ve@VeChord{} = pure (ve & veChord . chordMidiCtrls %~ (MidiCtrlAccent acc2 :))
    fadeInAccent (Just _) acc1 _    ve@(VeTremolo NoteTremolo{})  = pure (ve & veTremolo . ntrNote . noteMidiCtrls %~ (MidiCtrlAccent acc1 :))
    fadeInAccent Nothing  _    acc2 ve@(VeTremolo NoteTremolo{})  = pure (ve & veTremolo . ntrNote . noteMidiCtrls %~ (MidiCtrlAccent acc2 :))
    fadeInAccent (Just _) acc1 _    ve@(VeTremolo ChordTremolo{}) = pure (ve & veTremolo . ntrNote . noteMidiCtrls %~ (MidiCtrlAccent acc1 :))
    fadeInAccent Nothing  _    acc2 ve@(VeTremolo ChordTremolo{}) = pure (ve & veTremolo . ntrNote . noteMidiCtrls %~ (MidiCtrlAccent acc2 :))
    fadeInAccent _        _    _    vEvent = pure vEvent 

fadeInDynamics :: VoiceEventsMod
fadeInDynamics VoiceRuntimeConfig{..} ves = do
  dyn1 <- searchMConfigParam (_vrcSctnPath <> ".fadeInDyn1") <&> fromMaybe Forte
  dyn2 <- searchMConfigParam (_vrcSctnPath <> ".fadeInDyn2") <&> fromMaybe PPP
  pure $ case _vrcMNumVoc of
    Just _  -> tagFirstSoundDynamic dyn1 ves -- fade-in voice signaled via Just <index>
    Nothing -> tagFirstSoundDynamic dyn2 ves -- non fade-in voices get second dynamic

-- no need to repeat dynamic for each seg
uniformDynamics :: VoiceEventsMod
uniformDynamics VoiceRuntimeConfig{..} ves
  | _vrcNumSeg == 0 = searchMConfigParam (_vrcSctnPath <> ".uniformDyn") <&> fromMaybe PPP <&> flip tagFirstSoundDynamic ves
  | otherwise = pure ves

tagFirstSoundDynamic :: Dynamic -> [VoiceEvent] -> [VoiceEvent]
tagFirstSoundDynamic dyn ves = maybe ves (`tagDynForIdx` ves) $ findIndex isVeSound ves
  where
    tagDynForIdx idx = toList . adjust' tagDyn idx . fromList
    tagDyn ve@VeNote{}    = ve & veNote . noteCtrls %~ swapDyn dyn
    tagDyn ve@VeRhythm{}  = ve & veRhythm . rhythmCtrls %~ swapDyn dyn
    tagDyn ve@VeTuplet{}  = ve & veTuplet . tupNotes %~ (\notes -> tagDyn (NE.head notes) NE.:| NE.tail notes)
    tagDyn ve@VeChord{}   = ve & veChord . chordCtrls %~ swapDyn dyn
    tagDyn (VeTremolo nt@NoteTremolo{})  = VeTremolo (nt & ntrNote . noteCtrls %~ swapDyn dyn)
    tagDyn (VeTremolo ct@ChordTremolo{})  = VeTremolo (ct & ctrLeftChord . chordCtrls %~ swapDyn dyn)
    tagDyn ve             = error $ "tagFirstSoundDynamic: unexpected VoiceEvent: " <> show ve
    swapDyn dy = (:) (CtrlDynamic dy) . filter (not . isCtrlDynamic) 

isCtrlDynamic :: Control -> Bool
isCtrlDynamic CtrlDynamic {} = True
isCtrlDynamic _              = False

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

voicesDynamics :: VoiceEventsMod
voicesDynamics vrtc@VoiceRuntimeConfig{..} ves = do
  voicesDyn <- searchMConfigParam (_vrcSctnPath <> ".vocsDyn") <&> fromMaybe MF
  mIdxs::Maybe (NE.NonEmpty Int) <- searchMConfigParam (_vrcSctnPath <> ".vocsDynIdxs")
  voicesDynamics' voicesDyn mIdxs vrtc ves
  where
    voicesDynamics' :: Dynamic -> Maybe (NE.NonEmpty Int) -> VoiceEventsMod
    voicesDynamics' voicesDyn' mIdxs VoiceRuntimeConfig{..} ves'
      | _vrcNumSeg == 0 && isMElem _vrcNumVoc mIdxs = pure $ tagFirstSoundDynamic voicesDyn' ves'
      | otherwise = pure ves'
     where
        isMElem idx = maybe True (idx `elem`) 

fadeOutDynamics :: VoiceEventsMod
fadeOutDynamics _ = error "fadeOutDynamics is not implemented"

sustainNotes :: VoiceEventsMod
sustainNotes vrtc@VoiceRuntimeConfig{..} ves = do
  mIdxs::Maybe (NE.NonEmpty Int) <- searchMConfigParam (_vrcSctnPath <> ".sustainIdxs")
  sustainNotes' mIdxs vrtc ves
  where
    sustainNotes' :: Maybe (NE.NonEmpty Int) -> VoiceEventsMod
    sustainNotes' Nothing VoiceRuntimeConfig{..} ves' 
     | _vrcNumSeg == 0               = pure $ maybe ves' (`tagSustOnForIdx` ves') $ findIndex isVeSound ves'
     | _vrcNumSeg == _vrcCntSegs - 1 = pure $ maybe ves' (`tagSustOffForIdx` ves') $ lastMay (findIndices isVeSound ves')
     | otherwise = pure ves'
    sustainNotes' (Just idxs) VoiceRuntimeConfig{..} ves'
     | _vrcNumSeg == 0 && _vrcNumVoc `elem` idxs               = pure $ maybe ves' (`tagSustOnForIdx` ves') $ findIndex isVeSound ves'
     | _vrcNumSeg == _vrcCntSegs - 1 && _vrcNumVoc `elem` idxs = pure $ maybe ves' (`tagSustOffForIdx` ves') $ lastMay (findIndices isVeSound ves')
     | otherwise = pure ves'
    tagSustOnForIdx idx = toList . adjust' (tagSust SustainOn) idx . fromList
    tagSustOffForIdx idx = toList . adjust' (tagSust SustainOff) idx . fromList
    tagSust sust ve@VeNote{}    = ve & veNote . noteCtrls %~ (CtrlSustain sust :)
    tagSust sust ve@VeRhythm{}  = ve & veRhythm . rhythmCtrls %~ (CtrlSustain sust :)
    tagSust sust ve@VeTuplet{}  = ve & veTuplet . tupNotes %~ (\notes -> tagSust sust (NE.head notes) NE.:| NE.tail notes)
    tagSust sust ve@VeChord{}   = ve & veChord . chordCtrls %~ (CtrlSustain sust :)
    tagSust sust (VeTremolo nt@NoteTremolo{})  = VeTremolo (nt & ntrNote . noteCtrls %~ (CtrlSustain sust :))
    tagSust sust (VeTremolo ct@ChordTremolo{})  = VeTremolo (ct & ctrLeftChord . chordCtrls %~ (CtrlSustain sust :))
    tagSust _ ve                = error $ "sustainNotes: unexpected VoiceEvent: " <> show ve

-- uniform accents are specific to midi, score gets annotation
uniformAccents :: VoiceEventsMod
uniformAccents VoiceRuntimeConfig{..} ves = do
  acc <- searchMConfigParam (_vrcSctnPath <> ".uniformAcc") <&> fromMaybe Staccatissimo
  let ves' = if _vrcNumSeg == 0 then appendAnnFirstNote ("sempre " <> accent2Name acc) ves else ves
  pure $ uniformAccent acc <$> ves'
  where
    uniformAccent :: Accent -> VoiceEvent -> VoiceEvent
    uniformAccent acc ve@VeNote{} = ve & veNote . noteMidiCtrls %~ (MidiCtrlAccent acc :)
    uniformAccent acc ve@VeChord{} = ve & veChord . chordMidiCtrls %~ (MidiCtrlAccent acc :)
    uniformAccent acc ve@(VeTremolo NoteTremolo{}) = ve & veTremolo . ntrNote . noteMidiCtrls %~ (MidiCtrlAccent acc :)
    uniformAccent acc ve@(VeTremolo ChordTremolo{}) = ve & veTremolo . ctrLeftChord . chordMidiCtrls %~ (MidiCtrlAccent acc :)
    uniformAccent _   vEvent      = vEvent

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
      mPOOrPOs  = unfoldr (unfoldMPOOrPOs compareOp) (Left start,manyMIOrIssDiffs)
      ves       = catMaybes $ unfoldr unfoldVEs (mPOOrPOs,manyDurOrDurTups,manyAccts)
  if 0 == showVType
  then pure ves 
  else pure $ appendAnnFirstNote "xpose" ves
  where
    -- Be careful:  mIOrIss is infinite list.
    unfoldMPOOrPOs cmp (Left prev,Just (Left step):mIOrIss)
      | swap next `cmp` swap stop = Nothing
      | otherwise = Just (Just (Left next),(Left next,mIOrIss))
      where
        next = xp scale prev step
    unfoldMPOOrPOs cmp (Left prev,Just (Right steps):mIOrIss)
      | swap (head nexts) `cmp` swap stop = Nothing
      | otherwise = Just (Just (Right nexts),(Right nexts,mIOrIss))
      where
        nexts = xp scale prev <$> steps
    unfoldMPOOrPOs cmp (Right prevs,Just (Left step):mIOrIss)
      | swap next `cmp` swap stop = Nothing
      | otherwise = Just (Just (Left next),(Left next,mIOrIss))
      where
        next = xp scale (head prevs) step
    unfoldMPOOrPOs cmp (Right prevs,Just (Right steps):mIOrIss)
      | swap (head nexts) `cmp` swap stop = Nothing
      | otherwise = Just (Just (Right nexts),(Right nexts,mIOrIss))
      where
        nexts = xp scale (head prevs) <$> steps
    unfoldMPOOrPOs _ (prev,Nothing:mIOrIss) = Just (Nothing,(prev,mIOrIss))
    unfoldMPOOrPOs _ (prev,mIOrIss) = error $ "invalid list of steps, (" <> show prev <> "," <> show (take 10 mIOrIss) <> ")"

mPrOrPrss2MIOrIsDiffs:: Scale -> [Maybe PitOctOrPitOcts] -> [Maybe (Either Int [Int])]
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

-- Be careful:  list of [DurOrDurTuplet] and [Accent] are infinite.
unfoldVEs :: ([Maybe PitOctOrPitOcts],[DurOrDurTuplet],[Accent]) -> Maybe (Maybe VoiceEvent,([Maybe PitOctOrPitOcts],[DurOrDurTuplet],[Accent]))
unfoldVEs (mPOOrPOs:mPOOrPOss,Left dur:durOrDurTups,accent:accents) =
  Just (Just $ mkNoteChordOrRest mPOOrPOs dur accent,(mPOOrPOss,durOrDurTups,accents))
unfoldVEs (mPOOrPOss,Right durTup:durOrDurTups,accents) =
  Just (mVeTup,(mPOOrPOss',durOrDurTups,accents'))
  where
    (mVeTup,mPOOrPOss',accents') = mkMaybeVeTuplet mPOOrPOss durTup accents
unfoldVEs (_,_,_) = Nothing

-- A cell is a vertical slice through the same subset of duration, accent, and pitch/octave lists from the configuration.
-- Reduce complexity by always tupling the same group of three configuration values together, but randomly from list of list.
-- Given list of list of durations, accents and pitches/rests:
-- 1) Verify the outer lists are all of the same length, N (skip this, already happens in path2VoiceConfigCell)
-- 2) Generate an infinite list of indices [0..N-1]
-- 3) Expand durations, accents, and pitches/rests sublists so they're all the same length.
-- 4) Generate infinite length lists of durations, accents and pitches/rests sublists using infinite list of indices.
-- 5) Unfold infinite list of list of durations to finite list of durations, stop when at maxDurVal
-- 6) Unfold tuple of lists of pithes, durations, and accents to [VoiceEvent], stop when list of durations is empty
genCell :: String -> [[DurOrDurTuplet]] -> [[Accent]] -> [[Maybe PitOctOrPitOcts]] -> Int -> Driver [VoiceEvent]
genCell path durss acctss mPOOrPOss maxDurVal = do
  manyIs <- randomIndices (length durss)
  let (eqLenDurss,eqLenAcctss,eqLenmPOOrPOss) = mkEqualLengthSubLists (durss,acctss,mPOOrPOss)
      (manyDurs,manyAccts,manymPOOrPOss) = ((eqLenDurss !!) <$> manyIs,(eqLenAcctss !!) <$> manyIs,(eqLenmPOOrPOss !!) <$> manyIs)
      allDurOrDurTups = unfoldr (unfoldDurOrDurTups maxDurVal) (0,concat manyDurs)
      ves             = catMaybes . snd $ mapAccumL accumDurOrDurTupToMVEs (concat manymPOOrPOss,concat manyAccts) allDurOrDurTups
  showVType::Int <- searchMConfigParam (path <> ".showVType") <&> fromMaybe 1
  pure $ if 0 == showVType then ves else appendAnnFirstNote "cell" ves

mkEqualLengthSubLists :: ([[a]],[[b]],[[c]]) -> ([[a]],[[b]],[[c]])
mkEqualLengthSubLists (as:ass,bs:bss,cs:css) = (as':ass',bs':bss',cs':css')
  where
    (as',bs',cs') = mkEqualLengthLists (as,bs,cs)
    (ass',bss',css') = mkEqualLengthSubLists (ass,bss,css)
mkEqualLengthSubLists (as,bs,cs) = error $ "equalLists unexpected uneven length lists (as,bs,cs): " <> show (length as,length bs,length cs)

mkEqualLengthLists :: ([a],[b],[c]) -> ([a],[b],[c])
mkEqualLengthLists (xs,ys,zs) =
  (take l (cycle xs), take l (cycle ys), take l (cycle zs))
  where
    l = maximum [length xs,length ys,length zs]

genRepeat :: String -> [[DurOrDurTuplet]] -> [[Accent]] -> [[Maybe PitOctOrPitOcts]] -> Int -> Driver [VoiceEvent]
genRepeat path durss acctss mPOOrPOss maxDurVal = do
  durss'      <- freezeLists durss
  acctss'     <- freezeLists acctss
  mPOOrPOss'  <- freezeLists mPOOrPOss 
  genCanon path durss' acctss' mPOOrPOss' maxDurVal 0 <&> replaceAnnFirstNote "repeat"

freezeLists :: [[a]] -> Driver [[a]]
freezeLists xss = randomizeList xss <&> (:[]) . concat

genVerbatim :: String -> [[DurOrDurTuplet]] -> [[Accent]] -> [[Maybe PitOctOrPitOcts]] -> Int -> Driver [VoiceEvent]
genVerbatim path durss acctss mPOOrPOss maxDurVal = do
  genCanon path durss' acctss' mPOOrPOss' maxDurVal 0 <&> replaceAnnFirstNote "verbatim"
  where
    durss'     = [concat durss]
    acctss'    = [concat acctss]
    mPOOrPOss' = [concat mPOOrPOss]

genCanon :: String -> [[DurOrDurTuplet]] -> [[Accent]] -> [[Maybe PitOctOrPitOcts]] -> Int -> Int -> Driver [VoiceEvent]
genCanon path durOrDurTupss acctss mPOOrPOss maxDurVal rotVal = do
  showVType::Int   <- searchMConfigParam (path <> ".showVType") <&> fromMaybe 1
  manymPOOrPOss    <- randomElements mPOOrPOss  <&> concatMap (rotN rotVal)
  manyDurOrDurTups <- randomElements durOrDurTupss  <&> concat
  manyAccts        <- randomElements acctss <&> concat
  let allDurOrDurTups = unfoldr (unfoldDurOrDurTups maxDurVal) (0,manyDurOrDurTups)
      ves             = catMaybes . snd $ mapAccumL accumDurOrDurTupToMVEs (manymPOOrPOss,manyAccts) allDurOrDurTups
  if 0 == showVType
  then pure ves
  else pure $ appendAnnFirstNote "canon" ves

accumDurOrDurTupToMVEs :: ([Maybe PitOctOrPitOcts],[Accent]) -> DurOrDurTuplet -> (([Maybe PitOctOrPitOcts],[Accent]),Maybe VoiceEvent)
accumDurOrDurTupToMVEs (mPitOrPitOct:mPitOrPitOcts,accent:accents) (Left dur) =
  ((mPitOrPitOcts,accents),Just $ mkNoteChordOrRest mPitOrPitOct dur accent)
accumDurOrDurTupToMVEs (mPitOrPitOcts,accents) (Right durTup) =
  ((mPitOrPitOcts',accents'),mVeTup)
  where
    (mVeTup,mPitOrPitOcts',accents') = mkMaybeVeTuplet mPitOrPitOcts durTup accents
accumDurOrDurTupToMVEs (mPitOrPitOcts,accents) durTup = error $ "accumDurOrDurTupToMVEs unexpected vals: " <> show mPitOrPitOcts <> ", " <> show accents <> ", " <> show durTup

mkNoteChordOrRest :: Maybe PitOctOrPitOcts -> DurationVal -> Accent -> VoiceEvent
mkNoteChordOrRest (Just (Left (p,o))) d a = VeNote (Note p o d [] [CtrlAccent a] False)
mkNoteChordOrRest (Just (Right pos))  d a = VeChord (Chord (NE.fromList pos) d [] [CtrlAccent a] False)
mkNoteChordOrRest Nothing             d _ = VeRest (Rest d NoDynamic "")

-- Be careful:  length of [DurOrDurTuplet] is infinite.
unfoldDurOrDurTups :: Int -> (Int, [DurOrDurTuplet]) -> Maybe (DurOrDurTuplet, (Int, [DurOrDurTuplet]))
unfoldDurOrDurTups maxDurVal (totDurVal,Left dur:durOrDurTups)
  | totDurVal > maxDurVal = error $ "unfoldDurs totDurVal: " <> show totDurVal <> " exceeds max: " <> show maxDurVal
  | totDurVal == maxDurVal = Nothing
  | otherwise = Just (adjNextDur,(totDurVal + adjNextDurVal,durOrDurTups))
    where
      nextDurVal = fromVal dur
      adjNextDurVal = if totDurVal + nextDurVal <= maxDurVal then nextDurVal else nextDurVal - ((totDurVal + nextDurVal) - maxDurVal)
      adjNextDur = Left $ mkDurationVal adjNextDurVal
unfoldDurOrDurTups maxDurVal (totDurVal,Right durTup:durOrDurTups)
  | totDurVal > maxDurVal = error $ "unfoldDurs totDurVal: " <> show totDurVal <> " exceeds max: " <> show maxDurVal
  | totDurVal == maxDurVal = Nothing
  | otherwise = Just (adjNextDur,(totDurVal + adjNextDurVal,durOrDurTups))
    where
      nextDurVal = dur2DurVal (durTup2Dur durTup)
      adjNextDurVal = if totDurVal + nextDurVal <= maxDurVal then nextDurVal else nextDurVal - ((totDurVal + nextDurVal) - maxDurVal)
      adjNextDur = if adjNextDurVal == nextDurVal then Right durTup else Left $ mkDurationVal adjNextDurVal
      durTup2Dur DurTuplet{..} = durVal2Dur "3" (getDurSum (sumDurs (replicate _durtupDenominator _durtupUnitDuration)))
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
    accumSeen False (VeNote note)     = (True,VeNote   (note   & noteCtrls   %~ addOrAppendAnn ann append))
    accumSeen False (VeRhythm rhythm) = (True,VeRhythm (rhythm & rhythmCtrls %~ addOrAppendAnn ann append))
    accumSeen False (VeChord chord)   = (True,VeChord  (chord  & chordCtrls  %~ addOrAppendAnn ann append))
    accumSeen False (VeRest rest)     = (True,VeRest   (rest   & restAnn     %~ append ann))
    accumSeen False (VeSpacer spacer) = (True,VeSpacer (spacer & spacerAnn   %~ append ann))
    accumSeen seen  event             = (seen,event)

addOrAppendAnn :: String -> (String -> String -> String) -> [Control] -> [Control]
addOrAppendAnn newAnn append ctrls = maybe (CtrlAnnotation newAnn : ctrls) (`appendAnnForIdx` ctrls) $ findIndex isAnnotation ctrls
  where
    appendAnnForIdx idx = toList . adjust' appendAnn idx . fromList
    appendAnn (CtrlAnnotation oldAnn) = CtrlAnnotation (append newAnn oldAnn)
    appendAnn ctrl = error $ "appendAnn unexpected control: " <> show ctrl
  
isAnnotation :: Control -> Bool
isAnnotation (CtrlAnnotation _) = True
isAnnotation _                  = False

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

-- Be careful, [Maybe PitOctOrPitOcts] and/or [Accent] maybe be infinite lengths.
mkMaybeVeTuplet :: [Maybe PitOctOrPitOcts] -> DurTuplet -> [Accent] -> (Maybe VoiceEvent,[Maybe PitOctOrPitOcts],[Accent])
mkMaybeVeTuplet mPOOrPOs DurTuplet{..} accents
  | cntDurs > length accents' || cntDurs > length mPOOrPOs' = (Nothing,mPOOrPOs,accents) -- not enough left, maybe retry with next DurOrDurTuplet?
  | otherwise = (verifyVeTuplet veTup,drop cntDurs mPOOrPOs,drop cntDurs accents)
  where
    ves         = zipWith3 mkNoteChordOrRest mPOOrPOs' (NE.toList _durtupDurations) accents'
    veTup       = VeTuplet (Tuplet _durtupNumerator _durtupDenominator _durtupUnitDuration (NE.fromList ves))
    cntDurs     = length _durtupDurations
    mPOOrPOs'   = take cntDurs mPOOrPOs
    accents'    = take cntDurs accents

verifyVeTuplet :: VoiceEvent -> Maybe VoiceEvent
verifyVeTuplet (VeTuplet tuplet)
  | 0 == tup2CntTups tuplet = Nothing
  | otherwise = Just (VeTuplet tuplet)
verifyVeTuplet ve = Just ve

voiceConfig2VoiceEvents :: String -> VoiceConfig -> Driver [VoiceEvent]
voiceConfig2VoiceEvents path VoiceConfigXPose{..} =
  genXPose path (nes2arrs _vcxDurss) (nes2arrs _vcxAcctss)  (ness2Marrss _vcxmPOOrPOss ) _vcxScale (Range _vcxRange)
voiceConfig2VoiceEvents path VoiceConfigRepeat{..} =
  genRepeat path (nes2arrs _vcrDurss) (nes2arrs _vcrAcctss) (ness2Marrss _vcrmPOOrPOss ) _vcrDurVal
voiceConfig2VoiceEvents path VoiceConfigVerbatim{..} =
  genVerbatim path (nes2arrs _vcvDurss) (nes2arrs _vcvAcctss) (ness2Marrss _vcvmPOOrPOss ) _vcvDurVal
voiceConfig2VoiceEvents path VoiceConfigCell{..} =
  genCell path (nes2arrs _vcclDurss) (nes2arrs _vcclAcctss) (ness2Marrss _vcclmPOOrPOss ) _vcclDurVal
voiceConfig2VoiceEvents path VoiceConfigCanon{..} =
  genCanon path (nes2arrs _vccDurss) (nes2arrs _vccAcctss) (ness2Marrss _vccmPOOrPOss ) _vccDurVal _vccRotVal

ves2VeRests :: [VoiceEvent] -> [VoiceEvent]
ves2VeRests ves = [VeRest (Rest (mkDurationVal (ves2DurVal ves)) NoDynamic [])]

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

-- Single vertical slice through individual lists of list of: Maybe PitOctOrNEPitOcts, DurOrDurTuplet, and Accent,
-- used with VoiceConfigCell where sublists are guaranteed to be of the same length.  A [Slice]
-- gives all data from three input vals in VoiceConfig but grouped vertically, for blending
-- between VoiceConfig A and VoiceConfig B where both are VoiceConfigCell.
type Slice = ([Maybe PitOctOrNEPitOcts],[DurOrDurTuplet],[Accent])

sectionConfig2VoiceEvents :: SectionConfig -> Driver [[VoiceEvent]]
-- No section-level manipulation beyond segement repetition, and section and voice event mods from configuration.
sectionConfig2VoiceEvents (SectionConfigNeutral scnPath cntSegs mSctnName mConfigMods mVoiceEventsMods voiceConfigs) = do
  traverse (traverse (applyMConfigMods mConfigMods)) prss >>= traverse (concatMapM cvtAndApplyMod) <&> addSecnName scnName
   where
    cvtAndApplyMod (rtTup,cfgTup) = voiceConfig2VoiceEvents scnPath cfgTup >>= applyMVoiceEventsMods mVoiceEventsMods . (rtTup,)
    scnName         = fromMaybe "neutral" mSctnName
    cntVocs         = length voiceConfigs
    voiceRTConfigs  = [VoiceRuntimeConfig scnPath Nothing cntVocs numVoc cntSegs numSeg | numVoc <- [0..cntVocs - 1], numSeg <- [0..cntSegs - 1]]
    segRuntimeTupss = chunksOf cntSegs voiceRTConfigs
    prss = zipWith (\rtups cfgtup -> (,cfgtup) <$> rtups) segRuntimeTupss (NE.toList voiceConfigs)
-- Freeze config by generating single permutation of inner lists from list of list of pitch, duration, accent, and
-- freezing further randomization by concatenating inner lists and wrapping whole thing in a list so there's only one
-- inner list in the list of lists.
-- Not actually homophony, as each segment gets a new frozen config and configs are themselves frozen independently.
-- Repetition happens only with voice configs with durations that exceed sum of durations in configuration.
-- TBD: can be reduced to a config mod.  
sectionConfig2VoiceEvents (SectionConfigHomophony scnPath cntSegs mSctnName mConfigMods mVoiceEventsMods voiceConfigs) = do
  traverse (traverse (applyMConfigMods mConfigMods)) prss >>= traverse (concatMapM cvtAndApplyMod) <&> addSecnName scnName
  where
    cvtAndApplyMod (rt,cfg) = freezeConfig cfg >>= voiceConfig2VoiceEvents scnPath >>= applyMVoiceEventsMods mVoiceEventsMods . (rt,)
    scnName = fromMaybe "homophony" mSctnName
    cntVocs = length voiceConfigs
    voiceRTConfigs = [VoiceRuntimeConfig scnPath Nothing cntVocs numVoc cntSegs numSeg | numVoc <- [0..cntVocs - 1], numSeg <- [0..cntSegs - 1]]
    segRuntimeTupss = chunksOf cntSegs voiceRTConfigs
    prss = zipWith (\rtups cfgtup -> (,cfgtup) <$> rtups) segRuntimeTupss (NE.toList voiceConfigs)
-- Fade in from rests, voice-by-voice, voice event mods fadeinAccs and fadeinDyns background continuing voices: start of a round.
-- The list of indexes in fadeIxs tells the index for the [VoiceEvent] to add, one-by-one.
-- Monadically fold over list if indexes with fade-out order from config, generating new [[VoiceEvent]] for all voices.
sectionConfig2VoiceEvents (SectionConfigFadeIn scnPath fadeIxs mSctnName mConfigMods mVoiceEventsMods voiceConfigs) = do
  foldM foldMf ([], idxVEsPrs) fadeIxs <&> addSecnName scnName . map snd . snd
  where
    cntVocs    = length voiceConfigs
    idxVEsPrs  = (,[]) <$> [0..cntVocs - 1]
    scnName    = fromMaybe "fade in" mSctnName
    foldMf (seenNumVocs,idxVEsPr) numVoc = do
      newVEs <- genVEs numVoc <&> snd
      let restVEs = ves2VeRests newVEs
      traverse (appendVEs newVEs restVEs) idxVEsPr <&> (numVoc:seenNumVocs,)
      where
        cntSegs = length fadeIxs
        numSeg = length seenNumVocs
        appendVEs newVEs' restVEs' (idxVoc,ves)
          | idxVoc == numVoc = pure (idxVoc,ves <> newVEs')
          | idxVoc `elem` seenNumVocs = genVEs idxVoc <&> second (ves <>)
          | otherwise = pure  (idxVoc,ves <> restVEs')
        genVEs idx = 
          applyMods scnPath mConfigMods mVoiceEventsMods (rtTup,cfgTup)
          where
            mIdx = if idx == numVoc then Just idx else Nothing
            rtTup  = VoiceRuntimeConfig scnPath mIdx cntVocs idx cntSegs numSeg
            cfgTup = voiceConfigs NE.!! idx
-- Fade out to rests, voice-by-voice, like end of a round.  
-- The list of indexes in fadeIxs tells the index for the [VoiceEvent] to subtract, one-by-one.
-- Monadically fold over list if indexes with fade-out order from config, generating new [[VoiceEvent]] for all unseen voices,
-- leaving the existing list as it is for all new and seen voices.
-- Then expand all voices to be equal to the duration of the longest voice.
sectionConfig2VoiceEvents (SectionConfigFadeOut scnPath fadeIxs mSctnName mConfigMods mVoiceEventsMods voiceConfigs) = do
  vess <- foldM foldMf ([], idxVEsPrs) fadeIxs <&> addSecnName scnName . map snd . snd
  let veDurs = ves2DurVal <$> vess
  pure $ zipWith3 (mkVesTotDur (maximum veDurs)) veDurs timeSigs vess
  where
    cntVocs   = length voiceConfigs
    idxVEsPrs = (,[]) <$> [0..cntVocs - 1]
    scnName   = fromMaybe "fade out" mSctnName
    timeSigs = NE.toList $ _vccTime <$> voiceConfigs
    foldMf (seenNumVocs,idxVEsPr) numVoc = do
      traverse appendVEs idxVEsPr <&> (numVoc:seenNumVocs,)
      where
        cntSegs = length fadeIxs
        numSeg = length seenNumVocs
        appendVEs (idxVoc,ves)
          | idxVoc == numVoc || idxVoc `elem` seenNumVocs = pure (idxVoc,ves)
          | otherwise = genVEs idxVoc <&> second (ves <>) 
        genVEs idx = 
          applyMods scnPath mConfigMods mVoiceEventsMods (rtTup,cfgTup)
          where
            mIdx = if idx == numVoc then Just idx else Nothing
            rtTup  = VoiceRuntimeConfig scnPath mIdx cntVocs idx cntSegs numSeg
            cfgTup = voiceConfigs NE.!! idx
-- Blend between two [VoiceConfig] by grouping pitches, durations into equal-length [Slice], then substituting slice-by-slice
-- from second [VoiceConfig] into first [VoiceConfig], starting with one voice from first [VoiceConfig] ending with all voices 
-- but one from second [VoiceConfig].
-- All VoiceConfig must be sliceable, meaning outer lists in list of list of pitches, durations, and accents have to be the 
-- same length.
-- As the actual selection of which inner list in the list of list of pitches, durations, and accents gets rendered is
-- randomized by the VoiceConfig, 
sectionConfig2VoiceEvents (SectionConfigFadeAcross scnPath nReps mSctnName mConfigMods mVoiceEventsMods voiceConfigPrs) =
  traverse (traverse (applyMConfigMods mConfigMods)) prss >>= traverse (concatMapM cvtAndApplyMod) <&> addSecnName scnName
  where
    cvtAndApplyMod (rtTup,cfgTup) = voiceConfig2VoiceEvents scnPath cfgTup >>= applyMVoiceEventsMods mVoiceEventsMods . (rtTup,)
    cntVocs         = length voiceConfigPrs
    slicePrs        = both voiceConfig2Slice <$> NE.toList voiceConfigPrs
    blendedSlices   = transpose $ unfoldr (unfoldToSlicesRow nReps slicePrs) (1:replicate (cntVocs - 1) 0)
    voiceConfigs    = NE.toList (fst <$> voiceConfigPrs)
    voiceConfigss   = cfgSlicessPr2Configs <$> zip voiceConfigs blendedSlices
    cntSegs         = length (head voiceConfigss)
    voiceRTConfigs  = [VoiceRuntimeConfig scnPath Nothing cntVocs numVoc cntSegs numSeg | numVoc <- [0..cntVocs - 1], numSeg <- [0..cntSegs - 1]]
    segRuntimeTupss = chunksOf cntSegs voiceRTConfigs
    prss             = zipWith zip segRuntimeTupss voiceConfigss
    scnName          = fromMaybe "fade-cells" mSctnName

voiceConfig2Slice :: VoiceConfig -> [Slice]
voiceConfig2Slice VoiceConfigXPose{..}    = config2Slices _vcxmPOOrPOss  _vcxDurss  _vcxAcctss
voiceConfig2Slice VoiceConfigRepeat{..}   = config2Slices _vcrmPOOrPOss  _vcrDurss  _vcrAcctss
voiceConfig2Slice VoiceConfigVerbatim{..} = config2Slices _vcvmPOOrPOss  _vcvDurss  _vcvAcctss
voiceConfig2Slice VoiceConfigCell{..}     = config2Slices _vcclmPOOrPOss _vcclDurss _vcclAcctss
voiceConfig2Slice VoiceConfigCanon{..}    = config2Slices _vccmPOOrPOss  _vccDurss  _vccAcctss

config2Slices :: NE.NonEmpty (NE.NonEmpty (Maybe PitOctOrNEPitOcts))
                 -> NE.NonEmpty (NE.NonEmpty DurOrDurTuplet)
                 -> NE.NonEmpty (NE.NonEmpty Accent)
                 -> [Slice]
config2Slices mPOOrPOss durss acctss =
  unfoldr unfoldToSlicesTup (nes2arrs mPOOrPOss,nes2arrs durss,nes2arrs acctss)
  where
    unfoldToSlicesTup ([],[],[]) = Nothing
    unfoldToSlicesTup (as:ass,bs:bss,cs:css) = Just ((as,bs,cs),(ass,bss,css))
    unfoldToSlicesTup (as,bs,cs) = error $ "unfoldToSlicesTup uneven length lists: " <> show (length as,length bs, length cs) <> show (as,bs,cs)

cfgSlicessPr2Configs :: (VoiceConfig,[[Slice]]) -> [VoiceConfig]
cfgSlicessPr2Configs (voiceConfig,slicess) = tup2VoiceConfig voiceConfig <$> (slices2Tup <$> slicess)

type ConfigTup = (NE.NonEmpty (NE.NonEmpty (Maybe PitOctOrNEPitOcts))
                 ,NE.NonEmpty (NE.NonEmpty DurOrDurTuplet)
                 ,NE.NonEmpty (NE.NonEmpty Accent))

slices2Tup :: [Slice] -> ConfigTup
slices2Tup slices = (NE.fromList $ fst3 <$> sliceTups
                    ,NE.fromList $ snd3 <$> sliceTups
                    ,NE.fromList $ thd3 <$> sliceTups)
  where
    sliceTups = slice2Tup <$> slices

slice2Tup :: Slice -> (NE.NonEmpty (Maybe PitOctOrNEPitOcts),NE.NonEmpty DurOrDurTuplet,NE.NonEmpty Accent)
slice2Tup (mPitOctss,durs,accents) = (NE.fromList mPitOctss,NE.fromList durs,NE.fromList accents)

tup2VoiceConfig :: VoiceConfig -> ConfigTup -> VoiceConfig
tup2VoiceConfig (VoiceConfigXPose instr keySig scale timSig _ _ _ vcxRange') (mPitOrPits,durss,accents) =
  VoiceConfigXPose instr keySig scale timSig mPitOrPits durss accents vcxRange'
tup2VoiceConfig (VoiceConfigRepeat instr keySig timSig _ _ _ durVal) (mPitOrPits,durss,accents) =
  VoiceConfigRepeat instr keySig timSig mPitOrPits durss accents durVal
tup2VoiceConfig (VoiceConfigVerbatim instr keySig timSig _ _ _ durVal) (mPitOrPits,durss,accents) =
  VoiceConfigVerbatim instr keySig timSig mPitOrPits durss accents durVal
tup2VoiceConfig (VoiceConfigCell instr keySig timSig _ _ _ durVal) (mPitOrPits,durss,accents) =
  VoiceConfigCell instr keySig timSig mPitOrPits durss accents durVal
tup2VoiceConfig (VoiceConfigCanon instr keySig timSig _ _ _ durVal rotVal) (mPitOrPits,durss,accents) =
  VoiceConfigCanon instr keySig timSig mPitOrPits durss accents durVal rotVal

-- unfold instead of map because we don't just map over is, but rather use it to track progress of 
-- replacing config A slices with config B slices.
--   $ incrBlendedIndices [0,0,0,0]
--   [1,0,0,0]
--   $ incrBlendedIndices (incrBlendedIndices [0,0,0,0])
--   [2,1,0,0]
--   $ incrBlendedIndices (incrBlendedIndices (incrBlendedIndices [0,0,0,0]))
--   [3,2,1,0]
--   $ incrBlendedIndices (incrBlendedIndices (incrBlendedIndices (incrBlendedIndices [0,0,0,0])))
--   [4,3,2,1]
-- the end of the unfold is when the last element in the list reaches the count of config slices
unfoldToSlicesRow :: Int -> [([Slice],[Slice])] -> [Int] -> Maybe ([[Slice]],[Int])
unfoldToSlicesRow nReps slicePrs is
  | last is == cntCfgASlices = Nothing
  | otherwise = Just (slicesCol,is')
    where
      cntCfgASlices = length . fst . head $ slicePrs
      slicesCol = blendSlices nReps <$> zip is slicePrs
      is' = incrBlendedIndices is

blendSlices :: Int -> (Int,([Slice],[Slice])) -> [Slice]
blendSlices nReps (i,(cfgASlices,cfgBSlices)) = concat $ replicate nReps $ take i cfgBSlices <> drop i cfgASlices

incrBlendedIndices :: [Int] -> [Int]
incrBlendedIndices is = maybe (fmap succ is) (uncurry (<>) . first (fmap succ) . flip splitAt is . succ) $ elemIndex 0 is

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

-- TBD: regularize SectionConfigs with different counts of VoiceConfigs, fix
-- global time and key signatures and instrument type in Main too.
-- Going to mean pulling processing of [[[VoiceEvent]]] output from 
-- traverse of groupConfig2VoiceEvents to NonEmpty Voice input to writeScore
-- and pipeline from Main::cfg2Score here, dropping globals for time, key, and
-- instrument and sourcing them from VoiceConfig, maybe in parallel lists to
-- [[VoiceEvent]].  May want to reconsider this as it really makes no sense
-- to always repeat same key and time signature and instrument on a voice
-- config level.  But if something were to change, you'd want a way to slip
-- the appropriate VoiceEvent into the stream when it did change.  Maybe a
-- master voice list roster in a new top-level data type?  That could let you 
-- do something dynamic like stitching together tacit voices as you travel down
-- the list of group configs -- assuming you only change orchestration between
-- groups anyway -- which would turn a traverse into a foldM at the top level.
-- Note also pipeline forces genSplitStaffVoc which will need to be configurable
-- either by VoiceConfig or SectionConfig or else associated with an instrument.
-- But *before that* need to deal with variable choirs, instruments that sit out
-- a section by adding rests for tacit voices when they're missing from a Section,
-- preservation of ordering of by voice in [[VoiceEvent]].
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
          ve'    = if veLen == veLen' then ve else swapVeLens (durVal2Dur "maybeTrimVes" veLen') ve

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
    spacerOrRest = if not (null ves) && isSpacer (last ves) then VeSpacer . (\dur -> Spacer dur NoDynamic "") else VeRest . (\dur -> Rest dur NoDynamic "")

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
    -- for notes and chords (eventually rhythms, tuplets?), add ties 
    adjVEsDurs curLen allVes =
      second concat $ mapAccumL adjVEDur curLen allVes
      where
        adjVEDur :: Int -> VoiceEvent -> (Int,[VoiceEvent])
        adjVEDur curLen' (VeNote note@Note{..}) =
          (curLen' + addLen,stripTiedEventsCtrls . fixTies $ newNotes)
          where
            addLen = fromVal _noteDur
            durs = addEndDurs timeSig curLen' addLen
            newNotes = VeNote . (\dur -> note {_noteDur = dur}) <$> durs
        adjVEDur curLen' (VeChord chord@Chord{..}) =
          (curLen' + addLen,stripTiedEventsCtrls . fixTies $ newChords)
          where
            addLen = fromVal _chordDur
            durs = addEndDurs timeSig curLen' addLen
            newChords = VeChord . (\dur -> chord {_chordDur = dur}) <$> durs
        -- VeTuplet
        -- VeRhythm
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
            
stripTiedEventsCtrls :: [VoiceEvent] -> [VoiceEvent]
stripTiedEventsCtrls ves
  | length ves < 2 = ves
  | otherwise      = head ves:(stripTiedEventCtrls <$> tail ves)
        
-- tied-to voice events have no annotation, only VeNote and VeChord can currently be tied-to VoiceEvents
stripTiedEventCtrls :: VoiceEvent -> VoiceEvent
stripTiedEventCtrls (VeNote note@Note{})    = VeNote $ note & noteCtrls .~ [] & noteMidiCtrls .~ []
stripTiedEventCtrls (VeChord chord@Chord{}) = VeChord $ chord & chordCtrls .~ [] & chordMidiCtrls .~ []
stripTiedEventCtrls ve                      = error $ "stripTiedEventCtrls unexpected VoiceEvent: " <> show ve

-- first bool is True for first element in list, second bool is True for last element in list
-- for first note, want tie and annotations,
-- for middle notes, want tie and no annotations
-- for last note, want no tie and no annotations
fixVoiceEventTie :: Bool -> Bool -> VoiceEvent -> VoiceEvent
fixVoiceEventTie True  False ve@VeNote{}           = ve & veNote . noteTie .~ True 
fixVoiceEventTie False tie   (VeNote note@Note{})  = VeNote $ note & noteMidiCtrls .~ [] & noteCtrls .~ [] & noteTie .~ not tie
fixVoiceEventTie True  False ve@VeChord{}          = ve & veChord . chordTie .~ True
fixVoiceEventTie False tie   (VeChord chd@Chord{}) = VeChord $ chd & chordMidiCtrls .~ [] & chordCtrls .~ [] & chordTie .~ not tie 
fixVoiceEventTie True  False (VeRest rest)         = VeRest rest {-- TBD: { _restTie = True } --}
fixVoiceEventTie False _     ve@VeRest{}           = ve & veRest . restDyn .~ NoDynamic {-- TBD: ,_restTie = not tie --}
fixVoiceEventTie _     _     ve                    = ve

swapVeLens :: Duration -> VoiceEvent -> VoiceEvent
swapVeLens dur ve@VeNote{}   = ve & veNote   . noteDur   .~ duration2DurationVal dur
swapVeLens dur ve@VeRest{}   = ve & veRest   . restDur   .~ duration2DurationVal dur
swapVeLens dur ve@VeChord{}  = ve & veChord  . chordDur  .~ duration2DurationVal dur
swapVeLens dur ve@VeRhythm{} = ve & veRhythm . rhythmDur .~ duration2DurationVal dur
swapVeLens dur ve@VeSpacer{} = ve & veSpacer . spacerDur .~ duration2DurationVal dur
swapVeLens dur VeTuplet{}    = VeRest $ Rest (duration2DurationVal dur) NoDynamic ""
swapVeLens _   ve            = ve

freezeNELists :: NE.NonEmpty (NE.NonEmpty a) -> Driver (NE.NonEmpty (NE.NonEmpty a))
freezeNELists xss = randomizeList (nes2arrs xss) <&> singleton . NE.fromList . concat

freezeConfig :: VoiceConfig -> Driver VoiceConfig
freezeConfig vcx@VoiceConfigXPose{..}  = do
  durss      <- freezeNELists _vcxDurss
  acctss     <- freezeNELists _vcxAcctss
  mPOOrPOss  <- freezeNELists _vcxmPOOrPOss 
  pure $ vcx & vcxDurss .~ durss & vcxAcctss .~ acctss & vcxmPOOrPOss  .~ mPOOrPOss 
freezeConfig vcr@VoiceConfigRepeat{..} = do
  durss      <- freezeNELists _vcrDurss
  acctss     <- freezeNELists _vcrAcctss
  mPOOrPOss  <- freezeNELists _vcrmPOOrPOss 
  pure $ vcr & vcrDurss .~ durss & vcrAcctss .~ acctss & vcrmPOOrPOss  .~ mPOOrPOss  
freezeConfig vcr@VoiceConfigVerbatim{} = 
  pure vcr
freezeConfig vcl@VoiceConfigCell{..} = do
  durss      <- freezeNELists _vcclDurss
  acctss     <- freezeNELists _vcclAcctss
  mPOOrPOss  <- freezeNELists _vcclmPOOrPOss 
  pure $ vcl & vcclDurss .~ durss & vcclAcctss .~ acctss & vcclmPOOrPOss  .~ mPOOrPOss 
freezeConfig vcc@VoiceConfigCanon{..}  = do
  durss      <- freezeNELists _vccDurss
  acctss     <- freezeNELists _vccAcctss
  mPOOrPOss  <- freezeNELists _vccmPOOrPOss 
  pure $ vcc & vccDurss .~ durss & vccAcctss .~ acctss & vccmPOOrPOss  .~ mPOOrPOss 
    
sectionCfg2TimeSignature :: SectionConfig -> TimeSignature
sectionCfg2TimeSignature SectionConfigNeutral{..}    = voiceCfg2TimeSignature (NE.head _scnVoices)
sectionCfg2TimeSignature SectionConfigHomophony{..}  = voiceCfg2TimeSignature (NE.head _schVoices)
sectionCfg2TimeSignature SectionConfigFadeIn{..}     = voiceCfg2TimeSignature (NE.head _scfiVoices)
sectionCfg2TimeSignature SectionConfigFadeOut{..}    = voiceCfg2TimeSignature (NE.head _scfoVoices)
sectionCfg2TimeSignature SectionConfigFadeAcross{..} = voiceCfg2TimeSignature (fst (NE.head _scfcVoicesAB))

voiceCfg2TimeSignature :: VoiceConfig -> TimeSignature
voiceCfg2TimeSignature VoiceConfigXPose{..}    = _vcxTime
voiceCfg2TimeSignature VoiceConfigRepeat{..}   = _vcrTime
voiceCfg2TimeSignature VoiceConfigVerbatim{..} = _vcvTime
voiceCfg2TimeSignature VoiceConfigCell{..}     = _vcclTime
voiceCfg2TimeSignature VoiceConfigCanon{..}    = _vccTime
