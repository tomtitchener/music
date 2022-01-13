{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compose (sectionConfig2VoiceEvents
               , alignVoiceEventsDurations
               , mkVesTotDur
               , genSplitStaffVoc
               , tagTempo
               , ve2DurVal
               , ves2DurVal
               , swapVeLens
               )  where

import Data.Bifunctor (second)
-- import Control.Applicative ((<|>))
import Control.Lens hiding (pre,ix)
import Control.Monad (foldM)
import Control.Monad.Extra (concatMapM)
import Data.Foldable
import Data.Function (on)
import Data.List (elemIndex, findIndex, groupBy, sort, sortBy, unfoldr, (\\))
import qualified Data.List.NonEmpty as NE
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Sequence (adjust, fromList)
import Data.Tuple (swap)

import ComposeData
import Driver
       (Driver, randomElements, randomizeList, randomWeightedElement, searchConfigParam, searchMConfigParam)
import Types
import Utils

newtype Range = Range ((Pitch,Octave),(Pitch,Octave)) deriving (Eq, Show)

type ConfigMod = VoiceRuntimeConfig -> VoiceConfig -> Driver VoiceConfig

name2VoiceConfigMods :: M.Map String ConfigMod
name2VoiceConfigMods = M.fromList [("incrRandOcts",incrRandomizeMPitOctssOctaves)
                                   ,("decrRandOcts",decrNormalizeMPitOctssOctaves)]
                      
type VoiceEventsMod = VoiceRuntimeConfig -> [VoiceEvent] -> Driver [VoiceEvent]

name2VoiceVoiceEventsMods :: M.Map String VoiceEventsMod
name2VoiceVoiceEventsMods = M.fromList [("uniformAccs",uniformAccents)
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
modMPitOctssOctaves mkIdWeight vrtCfg vcx@VoiceConfigXPose{..}  = modAnyMPitOctssOctaves mkIdWeight vrtCfg _vcxmPIOrPIsss <&> \mPitOctsss -> vcx { _vcxmPIOrPIsss = mPitOctsss}
modMPitOctssOctaves mkIdWeight vrtCfg vcr@VoiceConfigRepeat{..} = modAnyMPitOctssOctaves mkIdWeight vrtCfg _vcrmPIOrPIsss <&> \mPitOctsss -> vcr { _vcrmPIOrPIsss = mPitOctsss}
modMPitOctssOctaves mkIdWeight vrtCfg vcc@VoiceConfigCanon{..}  = modAnyMPitOctssOctaves mkIdWeight vrtCfg _vccmPIOrPIsss <&> \mPitOctsss -> vcc { _vccmPIOrPIsss = mPitOctsss}

modAnyMPitOctssOctaves :: (Int -> Int -> Int ) -> VoiceRuntimeConfig -> NE.NonEmpty (NE.NonEmpty (Maybe PitIntOrPitInts)) -> Driver (NE.NonEmpty (NE.NonEmpty (Maybe PitIntOrPitInts)))
modAnyMPitOctssOctaves mkIdWeight VoiceRuntimeConfig{..} = 
  traverse (traverse randomizeMPitOcts)
  where
    idWeight  = mkIdWeight _vrcCntSegs _vrcNumSeg
    modWeight = (100 - idWeight) `div` 2
    weights   = [(modWeight,pred),(idWeight,id),(modWeight,succ)]
    randomizeMPitOcts (Just (Left pr)) = randomizeMPitOct pr <&> Just . Left
    randomizeMPitOcts (Just (Right prs)) = traverse randomizeMPitOct prs <&> Just . Right
    randomizeMPitOcts Nothing = pure Nothing
    randomizeMPitOct (pit,oct) = randomWeightedElement weights <&> (\f -> (pit,f oct))

mkIdWeightsIncr :: Int -> Int -> Int
mkIdWeightsIncr      1      _  = 50
mkIdWeightsIncr cntSegs numSeg = 100 - (numSeg * (50 `div` (cntSegs - 1))) -- TBD: magic constant 50% of results to be modified at end

mkIdWeightsDecr :: Int -> Int -> Int
mkIdWeightsDecr      1      _  = 0
mkIdWeightsDecr cntSegs numSeg = 100 - ((cntSegs - (1 + numSeg)) * (50 `div` (cntSegs - 1))) -- TBD: magic constant 50% of results to be modified at end)

--homAnyListOfList :: NE.NonEmpty (NE.NonEmpty a) -> Driver (NE.NonEmpty (NE.NonEmpty a))
--homAnyListOfList xss = randomizeList (NE.toList (NE.toList <$> xss)) <&> singleton . NE.fromList . concat

fadeInAccents :: VoiceEventsMod
fadeInAccents VoiceRuntimeConfig{..} ves = do
  acc1 <- searchMConfigParam (_vrcSctnPath <> ".fadeInAcc1") <&> fromMaybe Staccato
  acc2 <- searchMConfigParam (_vrcSctnPath <> ".fadeInAcc2") <&> fromMaybe Staccatissimo
  traverse (fadeInAccent _vrcMNumVoc acc1 acc2) ves
  where
    fadeInAccent :: Maybe Int -> Accent -> Accent -> VoiceEvent -> Driver VoiceEvent
    fadeInAccent (Just _) acc1 _   (VeNote note@Note{..}) = pure $ VeNote (note { _noteAccs = acc1 NE.<| _noteAccs })
    fadeInAccent Nothing  _   acc2 (VeNote note@Note{..}) = pure $ VeNote (note { _noteAccs = acc2 NE.<| _noteAccs })
    fadeInAccent _        _   _    (VeRest rest)          = pure $ VeRest rest
    fadeInAccent _        _   _    vEvent                 = pure vEvent 
    
fadeInDynamics :: VoiceEventsMod
fadeInDynamics VoiceRuntimeConfig{..} ves = do
  dyn1 <- searchMConfigParam (_vrcSctnPath <> ".fadeInDyn1") <&> fromMaybe Forte
  dyn2 <- searchMConfigParam (_vrcSctnPath <> ".fadeInDyn2") <&> fromMaybe PPP
  pure $ maybe (tagFirstNoteDynamic dyn2 ves) (const $ tagFirstNoteDynamic dyn1 ves) _vrcMNumVoc

uniformDynamics :: VoiceEventsMod
uniformDynamics VoiceRuntimeConfig{..} nOrRs = 
  searchMConfigParam (_vrcSctnPath <> ".uniformDyn") <&> fromMaybe PPP <&> flip tagFirstNoteDynamic nOrRs

tagFirstNoteDynamic :: Dynamic -> [VoiceEvent] -> [VoiceEvent]
tagFirstNoteDynamic dyn ves = maybe ves (`tagDynForIdx` ves) (findIndex isVeNote ves)
  where
    tagDynForIdx idx = toList . adjust (tagDyn dyn) idx . fromList
    tagDyn dyn' (VeNote note) = VeNote $ note { _noteDyn = dyn' }
    tagDyn _    vEvent        = error $ "tagFirstDynamicNote: unexpected VoiceEvent: " <> show vEvent

isVeNote :: VoiceEvent -> Bool
isVeNote VeNote {} = True
isVeNote _         = False

isVeRest :: VoiceEvent -> Bool
isVeRest VeRest {} = True
isVeRest _         = False
    
sectionDynamics :: VoiceEventsMod
sectionDynamics VoiceRuntimeConfig{..} ves = 
  searchConfigParam (_vrcSctnPath <> ".sectionDyns") <&> flip tagFirstNoteDynamic ves . (NE.!! _vrcNumSeg)

fadeOutDynamics :: VoiceEventsMod
fadeOutDynamics _ = pure 

uniformAccents :: VoiceEventsMod
uniformAccents VoiceRuntimeConfig{..} ves = do
  acc <- searchMConfigParam (_vrcSctnPath <> ".uniformAccent") <&> fromMaybe Staccatissimo
  traverse (uniformAccent acc) ves
  where
    uniformAccent :: Accent -> VoiceEvent -> Driver VoiceEvent
    uniformAccent acc (VeNote note@Note{..}) = pure $ VeNote (note { _noteAccs = acc NE.<| _noteAccs })
    uniformAccent _   vEvent                 = pure vEvent

-- Unfold repeated transpositions of [[Maybe Pitch]] across Range
-- matching up with repetitions of [[Duration]] and [[Accent] to generate VoiceEvent.
genXPose :: [[DurOrDurTuplet]] -> [[Accent]] -> [[Maybe (Either (Pitch,Int) [(Pitch,Int)])]] -> Scale -> Range -> Driver [VoiceEvent] -- in practice, VeRest | VeNote | VeChord
genXPose durOrDurTupss acctss mPrOrPrsss scale (Range (start,stop)) = do
  let rangeOrd   = swap stop `compare` swap start
      orderChords = second (sortBy (compare `on` swap))
  manyMIOrIssDiffs <- randomElements mPrOrPrsss <&> concatMap (mIOrIs2MIOrIsDiffs . mPrOrPrss2MIOrIss scale . map (fmap orderChords))
  manyDurOrDurTups <- randomElements durOrDurTupss  <&> concat
  manyAccts        <- randomElements acctss <&> concat
  let mSteps    = concatMap (map (fmap (either id minimum)) . mIOrIs2MIOrIsDiffs . mPrOrPrss2MIOrIss scale . map (fmap orderChords)) mPrOrPrsss
      stepOrd   = sum (fromMaybe 0 <$> mSteps) `compare` 0
      compareOp = if stepOrd == rangeOrd && stepOrd /= EQ
                  then if stepOrd == LT then (<=) else (>=)
                  else error $ "invalid step order " <> show stepOrd <> " compared with range order " <> show rangeOrd
      mPrOrPrss = unfoldr (unfoldMPrOrPrss compareOp) (Left start,manyMIOrIssDiffs)
      ves       = unfoldr unfoldVEs (mPrOrPrss,manyDurOrDurTups,manyAccts)
  pure $ ves & appendAnnFirstNote "xpose"
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

type UnfoldVETup = ([Maybe (Either (Pitch, Octave) [(Pitch, Octave)])],[Either Duration DurTuplet],[Accent])

-- Something going infinite here
unfoldVEs :: UnfoldVETup -> Maybe (VoiceEvent,UnfoldVETup)
unfoldVEs (mPrOrPrs:mPrOrPrss,Left dur:durOrDurTups,accent:accents) = Just (mkNoteChordOrRest mPrOrPrs dur accent,(mPrOrPrss,durOrDurTups,accents))
unfoldVEs (mPrOrPrss,Right durTup:durOrDurTups,accents) = Just (veTup,(mPrOrPrss',durOrDurTups,accents'))
  where
    (veTup,mPrOrPrss',accents') = mkVeTuplet mPrOrPrss durTup accents
unfoldVEs (_,_,_) = Nothing

mkNoteChordOrRest :: Maybe (Either (Pitch,Octave) [(Pitch,Octave)]) -> Duration -> Accent -> VoiceEvent
mkNoteChordOrRest (Just (Left (p,o))) d a = VeNote (Note p o d (singleton a) NoDynamic NoSwell "" False)
mkNoteChordOrRest (Just (Right pos))  d a = VeChord (Chord (NE.fromList pos) d (singleton a) NoDynamic NoSwell False)
mkNoteChordOrRest Nothing             d _ = VeRest (Rest d NoDynamic)
    
mPrOrPrss2MIOrIss :: Scale -> [Maybe (Either (Pitch,Int) [(Pitch,Int)])] -> [Maybe (Either Int [Int])]
mPrOrPrss2MIOrIss scale = map (fmap (bimap (pitchInt2ScaleDegree scale) (fmap (pitchInt2ScaleDegree scale))))

genRepeat :: [[DurOrDurTuplet]] -> [[Accent]] -> [[Maybe (Either (Pitch,Int) [(Pitch,Int)])]] -> Scale -> (Pitch,Octave) -> Int -> Driver [VoiceEvent] -- in practice, VeRest | VeNote | VeChord
genRepeat durss acctss mPrOrPrsss scale register maxDurVal = do
  durss'      <- freezeLists durss
  acctss'     <- freezeLists acctss
  mPrOrPrsss' <- freezeLists mPrOrPrsss
  genCanon durss' acctss' mPrOrPrsss' scale register maxDurVal 0 <&> replaceAnnFirstNote "static"

freezeLists :: [[a]] -> Driver [[a]]
freezeLists xss = randomizeList xss <&> (:[]) . concat

genCanon :: [[DurOrDurTuplet]] -> [[Accent]] -> [[Maybe (Either (Pitch,Int) [(Pitch,Int)])]] -> Scale -> (Pitch,Octave) -> Int -> Int -> Driver [VoiceEvent]
genCanon durOrDurTupss acctss mPrOrPrsss scale register maxDurVal rotVal = do
  let orderChords = second (sortBy (compare `on` swap)) -- for chord e.g. [(Pitch,Int)], order elements from low to high
  manyMIOrIssDiffs <- randomElements mPrOrPrsss <&> fmap (mIOrIs2MIOrIsDiffs . mPrOrPrss2MIOrIss scale . map (fmap orderChords) . rotN rotVal)
  manyDurOrDurTups <- randomElements durOrDurTupss  <&> concat
  manyAccts        <- randomElements acctss <&> concat
  let manyPOs         = repeat register
      manyMPrOrPrss   = concat $ zipWith (mIOrIsTranspose scale) manyPOs manyMIOrIssDiffs
      allDurOrDurTups = unfoldr (unfoldDurOrDurTups maxDurVal) (0,manyDurOrDurTups)
      ves             = unfoldr unfoldVEs (manyMPrOrPrss,allDurOrDurTups,manyAccts)
  pure $ appendAnnFirstNote "canon" ves

mIOrIsTranspose :: Scale -> (Pitch,Octave) -> [Maybe (Either Int [Int])] -> [Maybe (Either (Pitch,Octave) [(Pitch,Octave)])]
mIOrIsTranspose scale start = map (xp' scale start <$>) . reverse . snd . foldl' f (0,[])
  where
    f (s,l) Nothing         = (s, Nothing:l)
    f (s,l) (Just (Left i)) = (s', Just (Left s'):l)
      where
        s' = s + i
    f (s,l) (Just (Right is)) = (minimum is', Just (Right is'):l)
      where
        is' = (s +) <$> is
    xp' :: Scale -> (Pitch,Octave) -> Either Int [Int] -> Either (Pitch,Octave) [(Pitch,Octave)]
    xp' s po (Left off)  = Left (xp s po off)
    xp' s po (Right offs) = Right (xp s po <$> offs)
    
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
unfoldDurOrDurTups _ (_,[]) = error "unfoldDurOrDurTups unexpected empty list of [DurOrDurTuplet]"

durTup2Dur :: DurTuplet -> Duration
durTup2Dur DurTuplet{..} = durVal2Dur (getDurSum (sumDurs (replicate _durtupDenominator _durtupUnitDuration)))

appendAnnFirstNote :: String -> [VoiceEvent] -> [VoiceEvent]
appendAnnFirstNote ann = annFirstNote ann (\a b -> a <> ", " <> b)

prependAnnFirstNote :: String -> [VoiceEvent] -> [VoiceEvent]
prependAnnFirstNote ann = annFirstNote ann (\b a -> a <> ", " <> b)

replaceAnnFirstNote :: String -> [VoiceEvent] -> [VoiceEvent]
replaceAnnFirstNote ann = annFirstNote ann (const id)

annFirstNote :: String -> (String -> String -> String ) -> [VoiceEvent] -> [VoiceEvent]
annFirstNote ann append = reverse . snd . foldl' foldlf (False,[])
  where
    foldlf :: (Bool,[VoiceEvent]) -> VoiceEvent -> (Bool,[VoiceEvent])
    foldlf (True,ret)  (VeNote note) = (True,VeNote note:ret)
    foldlf (False,ret) (VeNote note) = (True,VeNote (annNote note):ret)
    foldlf (seen,ret)  event = (seen,event:ret)
    annNote note@Note{..} = if null _noteAnn
                            then
                              note { _noteAnn = ann }
                            else
                              note { _noteAnn = append _noteAnn ann }
                              
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

-- Same as mInts2MIntDiffs using minimum element in [Int]
mIOrIs2MIOrIsDiffs :: [Maybe (Either Int [Int])] -> [Maybe (Either Int [Int])]
mIOrIs2MIOrIsDiffs =
  snd . foldl foldlf (0,[])
  where
    foldlf (prev,ret) Nothing          = (prev,ret <> [Nothing])
    foldlf (prev,ret) (Just (Left i))  = (i,ret <> [Just (Left (i - prev))])
    foldlf (prev,ret) (Just (Right is)) = (minimum is,ret <> [Just (Right (flip (-) prev <$> is))])

mkVeTuplet :: [Maybe (Either (Pitch,Octave) [(Pitch,Octave)])] -> DurTuplet -> [Accent] -> (VoiceEvent,[Maybe (Either (Pitch,Octave) [(Pitch,Octave)])],[Accent])
mkVeTuplet mPOrPrss DurTuplet{..} accents = (veTup,drop cntDurs mPOrPrss,drop cntDurs accents)
  where
    ves = zipWith3 mkNoteChordOrRest mPOrPrss' (NE.toList _durtupDurations) accents'
    veTup = VeTuplet (Tuplet _durtupNumerator _durtupDenominator _durtupUnitDuration (NE.fromList ves))
    mPOrPrss' = take cntDurs mPOrPrss
    accents' = take cntDurs accents
    cntDurs = NE.length _durtupDurations

voiceConfig2VoiceEvents :: VoiceConfig -> Driver [VoiceEvent]
voiceConfig2VoiceEvents VoiceConfigXPose{..} =
  genXPose (nes2arrs _vcxDurss) (nes2arrs _vcxAcctss)  (ness2Marrss _vcxmPIOrPIsss) _vcxScale (Range _vcxRange)
voiceConfig2VoiceEvents VoiceConfigRepeat{..} =
  genRepeat (nes2arrs _vcrDurss) (nes2arrs _vcrAcctss) (ness2Marrss _vcrmPIOrPIsss) _vcrScale _vcrRegister _vcrDurVal
voiceConfig2VoiceEvents VoiceConfigCanon{..} =
  genCanon (nes2arrs _vccDurss) (nes2arrs _vccAcctss) (ness2Marrss _vccmPIOrPIsss)_vccScale _vccRegister _vccDurVal _vccRotVal

ves2VeRests :: [VoiceEvent] -> [VoiceEvent]
ves2VeRests = fmap ve2VeRest
  where
    ve2VeRest (VeNote Note{..})   = VeRest (Rest _noteDur NoDynamic)
    ve2VeRest (VeChord Chord{..}) = VeRest (Rest _chordDur NoDynamic)
    ve2VeRest ve                  = ve

applyMConfigMods :: Maybe (NE.NonEmpty String) -> (VoiceRuntimeConfig, VoiceConfig) -> Driver (VoiceRuntimeConfig,VoiceConfig)
applyMConfigMods mNames pr = applyMMods mNames pr (applyMod name2VoiceConfigMods) <&> (fst pr,)

applyMVoiceEventsMods :: Maybe (NE.NonEmpty String) -> (VoiceRuntimeConfig,[VoiceEvent]) -> Driver [VoiceEvent]
applyMVoiceEventsMods mNames pr = applyMMods mNames pr (applyMod name2VoiceVoiceEventsMods)

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
    cvtAndApplyMod (rtTup,cfgTup) = voiceConfig2VoiceEvents cfgTup >>= applyMVoiceEventsMods mVoiceEventsMods . (rtTup,)
    scnName = fromMaybe "neutral" mSctnName
    cntVocs = length voiceConfigs
    segRuntimeTupss = chunksOf cntSegs [VoiceRuntimeConfig scnPath Nothing cntVocs numVoc cntSegs numSeg | numVoc <- [0..cntVocs - 1], numSeg <- [0..cntSegs - 1]]
    prs = zipWith (\rtups cfgtup -> (,cfgtup) <$> rtups) segRuntimeTupss (NE.toList voiceConfigs)

sectionConfig2VoiceEvents (SectionConfigHomophony scnPath cntSegs mSctnName mConfigMods mVoiceEventsMods voiceConfigs) = do
  traverse (traverse (applyMConfigMods mConfigMods)) prs >>= traverse (concatMapM cvtAndApplyMod) <&> addSecnName scnName
  where
    cvtAndApplyMod (rt,cfg) = freezeConfig cfg >>= voiceConfig2VoiceEvents >>= applyMVoiceEventsMods mVoiceEventsMods . (rt,)
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
      seenVEs <- applyMConfigMods mConfigMods (vocRTTup,vocCfgTup) >>= voiceConfig2VoiceEvents . snd >>= applyMVoiceEventsMods mVoiceEventsMods . (vocRTTup,)
      let vocRunTups = mkVocRTT numSeg <$> seenNumVocs
          vocCfgTups = (voiceConfigs NE.!!) <$> seenNumVocs
      seenNumVocsEventss <- traverse (applyMods mConfigMods mVoiceEventsMods) (zip vocRunTups vocCfgTups)
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
  inits <- traverse (applyMods mConfigMods mVoiceEventsMods) (zip vocRunTups vocCfgTups)
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
      unseenNumVocsEventss <- traverse (applyMods mConfigMods mVoiceEventsMods) (zip vocRunTups vocCfgTups)
      let seenNumVocsEmpties::[(Int,[VoiceEvent])] = (,[]) <$> seenNumVocs'
          newEventss = snd <$> sort (seenNumVocsEmpties <> unseenNumVocsEventss)
      pure (seenNumVocs',zipWith (<>) prevEventss newEventss)

freezeNELists :: NE.NonEmpty (NE.NonEmpty a) -> Driver (NE.NonEmpty (NE.NonEmpty a))
freezeNELists xss = randomizeList (nes2arrs xss) <&> singleton . NE.fromList . concat

freezeConfig :: VoiceConfig -> Driver VoiceConfig
freezeConfig vcx@VoiceConfigXPose{..}  = do
  durss      <- freezeNELists _vcxDurss
  acctss     <- freezeNELists _vcxAcctss
  mPIOrPIsss <- freezeNELists _vcxmPIOrPIsss
  pure $ vcx { _vcxDurss = durss, _vcxAcctss = acctss, _vcxmPIOrPIsss = mPIOrPIsss }
freezeConfig vcr@VoiceConfigRepeat{..} = do
  durss      <- freezeNELists _vcrDurss
  acctss     <- freezeNELists _vcrAcctss
  mPIOrPIsss <- freezeNELists _vcrmPIOrPIsss
  pure $ vcr { _vcrDurss = durss, _vcrAcctss = acctss, _vcrmPIOrPIsss = mPIOrPIsss }
freezeConfig vcc@VoiceConfigCanon{..}  = do
  durss      <- freezeNELists _vccDurss
  acctss     <- freezeNELists _vccAcctss
  mPIOrPIsss <- freezeNELists _vccmPIOrPIsss
  pure $ vcc { _vccDurss = durss, _vccAcctss = acctss, _vccmPIOrPIsss = mPIOrPIsss }
    
applyMods :: Maybe (NE.NonEmpty String) -> Maybe (NE.NonEmpty String) -> (VoiceRuntimeConfig,VoiceConfig) -> Driver (Int,[VoiceEvent])
applyMods mConfigMods mVoiceEventsMods pr =
  applyMConfigMods mConfigMods pr >>= voiceConfig2VoiceEvents . snd >>= applyMVoiceEventsMods mVoiceEventsMods . (vrt,) <&> (_vrcNumVoc vrt,)
  where
    vrt = fst pr

addSecnName :: String -> [[VoiceEvent]] -> [[VoiceEvent]]
addSecnName scnName voices = prependAnnFirstNote scnName <$> voices

genSplitStaffVoc :: Instrument -> KeySignature -> TimeSignature -> [VoiceEvent] -> Voice
genSplitStaffVoc instr keySig timeSig ves
  = SplitStaffVoice instr (VeKeySignature keySig NE.<| VeTimeSignature timeSig NE.<| NE.fromList ves)

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
    spacerOrRest = if isSpacer (last ves) then VeSpacer . flip Spacer NoDynamic else VeRest . flip Rest NoDynamic

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

ve2Durs :: VoiceEvent -> [Duration]
ve2Durs (VeNote   Note {..})   = [_noteDur]
ve2Durs (VeRest   Rest {..})   = [_restDur]
ve2Durs (VeChord  Chord {..})  = [_chordDur]
ve2Durs (VeRhythm Rhythm {..}) = [_rhythmDur]
ve2Durs (VeSpacer Spacer {..}) = [_spacerDur]
ve2Durs (VeTuplet Tuplet {..}) = replicate _tupDenom _tupDur
ve2Durs _                      = []

ve2DurVal :: VoiceEvent -> Int
ve2DurVal ve = sum (dur2DurVal <$> ve2Durs ve)

ves2DurVal :: [VoiceEvent] -> Int
ves2DurVal = sum . fmap ve2DurVal

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
alignVoiceEventsDurations timeSig =
  snd . foldl' adjVEsDurs (0,[]) . groupBy ((==) `on` isVeRest)
  where
    -- adjust [VoiceEvent] durations by time signature by current length at start of list
    adjVEsDurs :: (Int,[VoiceEvent]) -> [VoiceEvent] -> (Int,[VoiceEvent])
    -- contiguous rests can just use durations from addEndDurs, no need for ties
    adjVEsDurs (curLen,ret) rests@((VeRest _):_) =
      (curLen + addLen,ret <> newRests)
      where
        addLen = ves2DurVal rests
        durs = addEndDurs timeSig curLen addLen
        newRests = VeRest . flip Rest NoDynamic <$> durs
    -- fo notes and chords (eventually rhythms), add ties 
    adjVEsDurs (curLen,ret) allVes =
      foldl' adjVEDur (curLen,ret) allVes
      where
        adjVEDur :: (Int,[VoiceEvent]) -> VoiceEvent -> (Int,[VoiceEvent])
        adjVEDur (curLen',ret') (VeNote note@Note{..}) =
          (curLen' + addLen,ret' <> (stripAnnotations . fixTies $ newNotes))
          where
            addLen = dur2DurVal _noteDur
            durs = addEndDurs timeSig curLen' addLen
            newNotes = VeNote . (\dur -> note {_noteDur = dur}) <$> durs
        adjVEDur (curLen',ret') (VeChord chord@Chord{..}) =
          (curLen' + addLen,ret' <> (stripAnnotations . fixTies $ newChords))
          where
            addLen = dur2DurVal _chordDur
            durs = addEndDurs timeSig curLen' addLen
            newChords = VeChord . (\dur -> chord {_chordDur = dur}) <$> durs
        adjVEDur (curLen',ret') ve = (curLen' + ve2DurVal ve,ret' <> [ve])
        
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
stripAnnotation (VeNote Note{..}) = VeNote $ Note _notePit _noteOct _noteDur (singleton NoAccent) NoDynamic NoSwell "" _noteTie
stripAnnotation ve                = ve

-- first bool is True for first element in list, second bool is True for last element in list
-- for first note, want tie and annotations, for middle notes,
-- for middle notes, want tie and no annotations
-- for last note, want no tie and no annotations
fixVoiceEventTie :: Bool -> Bool -> VoiceEvent -> VoiceEvent
fixVoiceEventTie True  False (VeNote note)   = VeNote  $ note { _noteTie = True }
fixVoiceEventTie False tie   (VeNote note)   = VeNote  $ note { _noteAccs = singleton NoAccent, _noteDyn = NoDynamic, _noteSwell = NoSwell, _noteAnn = "", _noteTie = not tie }
fixVoiceEventTie True  False (VeChord chord) = VeChord $ chord { _chordTie = True }
fixVoiceEventTie False tie   (VeChord chord) = VeChord $ chord { _chordAccs = singleton NoAccent, _chordDyn = NoDynamic, _chordSwell = NoSwell, _chordTie = not tie }
fixVoiceEventTie True  False (VeRest rest)   = VeRest    rest {-- TBD: { _restTie = True } --}
fixVoiceEventTie False _     (VeRest rest)   = VeRest  $ rest { _restDyn = NoDynamic {-- TBD: ,_restTie = not tie --} }
fixVoiceEventTie _     _     ve              = ve

swapVeLens :: Duration -> VoiceEvent -> VoiceEvent
swapVeLens dur (VeNote   note)   = VeNote   $ note { _noteDur = dur }
swapVeLens dur (VeRest   rest)   = VeRest   $ rest { _restDur = dur }
swapVeLens dur (VeChord  chord)  = VeChord  $ chord { _chordDur = dur }
swapVeLens dur (VeRhythm rhythm) = VeRhythm $ rhythm { _rhythmDur = dur }
swapVeLens dur (VeSpacer spacer) = VeSpacer $ spacer { _spacerDur = dur }
swapVeLens dur (VeTuplet _)      = VeRest   $ Rest dur NoDynamic
swapVeLens _   ve                = ve

