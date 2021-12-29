{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compose where

import Control.Applicative ((<|>))
import Control.Lens hiding (pre,ix)
import Control.Monad (foldM)
import Control.Monad.Extra (concatMapM)
import Data.Either (isRight, isLeft)
import Data.Foldable
import Data.Function (on)
import Data.List (elemIndex, findIndex, groupBy, sort, unfoldr, (\\))
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

type NoteOrRest = Either Note Rest

newtype Range = Range ((Pitch,Octave),(Pitch,Octave)) deriving (Eq, Show)

type ConfigMod = VoiceRuntimeConfig -> VoiceConfig -> Driver VoiceConfig

name2VoiceConfigMods :: M.Map String ConfigMod
name2VoiceConfigMods = M.fromList [("incrRandOcts",incrRandomizeMPitOctssOctaves)
                                   ,("decrRandOcts",decrNormalizeMPitOctssOctaves)]
                      
type NOrRsMod = VoiceRuntimeConfig -> [NoteOrRest] -> Driver [NoteOrRest]

name2VoiceNOrRsMods :: M.Map String NOrRsMod
name2VoiceNOrRsMods = M.fromList [("uniformAccs",uniformAccents)
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
modMPitOctssOctaves mkIdWeight vrtCfg vcx@VoiceConfigXPose{..}  = modAnyMPitOctssOctaves mkIdWeight vrtCfg _vcxMPitOctss <&> \mPitOctss -> vcx { _vcxMPitOctss =  mPitOctss}
modMPitOctssOctaves mkIdWeight vrtCfg vcr@VoiceConfigRepeat{..} = modAnyMPitOctssOctaves mkIdWeight vrtCfg _vcrMPitOctss <&> \mPitOctss -> vcr { _vcrMPitOctss =  mPitOctss}
modMPitOctssOctaves mkIdWeight vrtCfg vcc@VoiceConfigCanon{..}  = modAnyMPitOctssOctaves mkIdWeight vrtCfg _vccMPitOctss <&> \mPitOctss -> vcc { _vccMPitOctss =  mPitOctss}

modAnyMPitOctssOctaves :: (Int -> Int -> Int ) -> VoiceRuntimeConfig -> NE.NonEmpty (NE.NonEmpty (Maybe Pitch,Int)) -> Driver (NE.NonEmpty (NE.NonEmpty (Maybe Pitch,Int)))
modAnyMPitOctssOctaves mkIdWeight VoiceRuntimeConfig{..} = 
  traverse (traverse randomizeMPitOctsOctave)
  where
    idWeight  = mkIdWeight _vrcCntSegs _vrcNumSeg
    modWeight = (100 - idWeight) `div` 2
    weights   = [(modWeight,pred),(idWeight,id),(modWeight,succ)]
    randomizeMPitOctsOctave (Just pit,oct) = randomWeightedElement weights <&> (\weight -> (Just pit,weight oct))
    randomizeMPitOctsOctave pr = pure pr

mkIdWeightsIncr :: Int -> Int -> Int
mkIdWeightsIncr      1      _  = 50
mkIdWeightsIncr cntSegs numSeg = 100 - (numSeg * (50 `div` (cntSegs - 1))) -- TBD: magic constant 50% of results to be modified at end

mkIdWeightsDecr :: Int -> Int -> Int
mkIdWeightsDecr      1      _  = 0
mkIdWeightsDecr cntSegs numSeg = 100 - ((cntSegs - (1 + numSeg)) * (50 `div` (cntSegs - 1))) -- TBD: magic constant 50% of results to be modified at end)

--homAnyListOfList :: NE.NonEmpty (NE.NonEmpty a) -> Driver (NE.NonEmpty (NE.NonEmpty a))
--homAnyListOfList xss = randomizeList (NE.toList (NE.toList <$> xss)) <&> singleton . NE.fromList . concat

fadeInAccents :: NOrRsMod
fadeInAccents VoiceRuntimeConfig{..} nOrRs = do
  acc1 <- searchMConfigParam (_vrcSctnPath <> ".fadeInAcc1") <&> fromMaybe Staccato
  acc2 <- searchMConfigParam (_vrcSctnPath <> ".fadeInAcc2") <&> fromMaybe Staccatissimo
  traverse (fadeInAccent _vrcMNumVoc acc1 acc2) nOrRs
  where
    fadeInAccent :: Maybe Int -> Accent -> Accent -> NoteOrRest -> Driver NoteOrRest
    fadeInAccent (Just _) acc1 _   (Left note@Note{..}) = pure $ Left (note { _noteAccs = acc1 NE.<| _noteAccs })
    fadeInAccent Nothing  _   acc2 (Left note@Note{..}) = pure $ Left (note { _noteAccs = acc2 NE.<| _noteAccs })
    fadeInAccent _        _   _    (Right rest)         = pure $ Right rest

fadeInDynamics :: NOrRsMod
fadeInDynamics VoiceRuntimeConfig{..} nOrRs = do
  dyn1 <- searchMConfigParam (_vrcSctnPath <> ".fadeInDyn1") <&> fromMaybe Forte
  dyn2 <- searchMConfigParam (_vrcSctnPath <> ".fadeInDyn2") <&> fromMaybe PPP
  pure $ maybe (tagFirstNoteDynamic dyn2 nOrRs) (const $ tagFirstNoteDynamic dyn1 nOrRs) _vrcMNumVoc

uniformDynamics :: NOrRsMod
uniformDynamics VoiceRuntimeConfig{..} nOrRs = 
  searchMConfigParam (_vrcSctnPath <> ".uniformDyn") <&> fromMaybe PPP <&> flip tagFirstNoteDynamic nOrRs

tagFirstNoteDynamic :: Dynamic -> [NoteOrRest] -> [NoteOrRest]
tagFirstNoteDynamic dyn nOrRs = maybe nOrRs (`tagDynForIdx` nOrRs) (findIndex isLeft nOrRs)
  where
    tagDynForIdx idx = toList . adjust (tagDyn dyn) idx . fromList
    tagDyn dyn' (Left note) = Left $ note { _noteDyn = dyn' }
    tagDyn _    (Right oops) = error $ "tagDyn unexpected rest: " <> show oops
    
sectionDynamics :: NOrRsMod
sectionDynamics VoiceRuntimeConfig{..} nOrRs = 
  searchConfigParam (_vrcSctnPath <> ".sectionDyns") <&> flip tagFirstNoteDynamic nOrRs . (NE.!! _vrcNumSeg)

fadeOutDynamics :: NOrRsMod
fadeOutDynamics _ = pure 

uniformAccents :: NOrRsMod
uniformAccents VoiceRuntimeConfig{..} nOrRs = do
  acc <- searchMConfigParam (_vrcSctnPath <> ".uniformAccent") <&> fromMaybe Staccatissimo
  traverse (uniformAccent acc) nOrRs
  where
    uniformAccent :: Accent -> NoteOrRest -> Driver NoteOrRest
    uniformAccent acc (Left note@Note{..}) = pure $ Left (note { _noteAccs = acc NE.<| _noteAccs })
    uniformAccent _ (Right rest) = pure $ Right rest

-- Unfold repeated transpositions of [[Maybe Pitch]] across Range
-- matching up with repetitions of [[Duration]] and [[Accent] to generate NoteOrRest.
genXPose :: [[Duration]] -> [[Accent]] -> [[(Maybe Pitch,Int)]] -> Scale -> Range -> Driver [NoteOrRest]
genXPose durss acctss mPitOctss scale (Range (start,stop)) = do
  manySteps <- randomElements mPitOctss <&> concatMap (mInts2IntDiffs . fmap (mPitchInt2MScaleDegree scale))
  manyDurs  <- randomElements durss  <&> concat
  manyAccts <- randomElements acctss <&> concat
  let mSteps    = concatMap (mInts2IntDiffs . fmap (mPitchInt2MScaleDegree scale)) mPitOctss
      stepOrd   = sum (fromMaybe 0 <$> mSteps) `compare` 0
      rangeOrd  = swap stop `compare` swap start
      compareOp = if stepOrd == rangeOrd && stepOrd /= EQ
                  then if stepOrd == LT then (<=) else (>=)
                  else error $ "invalid step order " <> show stepOrd <> " compared with range order " <> show rangeOrd
      mPOs = unfoldr (unfoldf compareOp) (start,manySteps)
  pure $ zipWith3 mkNoteOrRest mPOs manyDurs manyAccts & appendAnnFirstNote "xpose"
  where
    unfoldf cmp (prev,(Just step1):mSteps)
      | swap next `cmp` swap stop = Nothing
      | otherwise = Just (Just next,(next, mSteps))
      where
        next = xp scale prev step1
    unfoldf _ (prev, Nothing:mSteps) = Just (Nothing,(prev, mSteps))
    unfoldf _ steps = error $ "invalid list of steps, (" <> show (fst steps) <> "," <> show (take 10 (snd steps)) <> ")"

genStatic :: [[Duration]] -> [[Accent]] -> [[(Maybe Pitch,Int)]] -> Scale -> (Pitch,Octave) -> Int -> Driver [NoteOrRest]
genStatic durss acctss mPitOctss scale register maxDurVal = do
  durs <- randomizeList durss <&> concat
  accts <- randomizeList acctss <&> concat
  mPitOcts <- randomizeList mPitOctss <&> concat
  genCanon [durs] [accts] [mPitOcts] scale register maxDurVal 0 <&> replaceAnnFirstNote "static"

genCanon :: [[Duration]] -> [[Accent]] -> [[(Maybe Pitch,Int)]] -> Scale -> (Pitch,Octave) -> Int -> Int -> Driver [NoteOrRest]
genCanon durss acctss mPitOctss scale register maxDurVal rotVal = do
  manyMIntss <- randomElements mPitOctss <&> fmap (mInts2IntDiffs . fmap (mPitchInt2MScaleDegree scale) . rotN rotVal)
  manyDurs   <- randomElements durss  <&> concat
  manyAccts  <- randomElements acctss <&> concat
  let manyPOs    = repeat register
      manyMPOs   = concat $ zipWith (mtranspose scale) manyPOs manyMIntss
      allDurs  = unfoldr (unfoldDurs maxDurVal) (0,manyDurs)
  pure $ zipWith3 mkNoteOrRest manyMPOs allDurs manyAccts & appendAnnFirstNote "canon"

unfoldDurs :: Int -> (Int, [Duration]) -> Maybe (Duration, (Int, [Duration]))
unfoldDurs maxDurVal (totDurVal,durs)
  | totDurVal > maxDurVal = error $ "unfoldDurs totDurVal: " <> show totDurVal <> " exceeds max: " <> show maxDurVal
  | totDurVal == maxDurVal = Nothing
  | otherwise = Just (adjNextDur,(totDurVal + adjNextDurVal,tail durs))
    where
      nextDurVal = dur2DurVal (head durs)
      adjNextDurVal = if totDurVal + nextDurVal <= maxDurVal then nextDurVal else nextDurVal - ((totDurVal + nextDurVal) - maxDurVal)
      adjNextDur = durVal2Dur adjNextDurVal
  
appendAnnFirstNote :: String -> [NoteOrRest] -> [NoteOrRest]
appendAnnFirstNote ann = annFirstNote ann (\a b -> a <> ", " <> b)

prependAnnFirstNote :: String -> [NoteOrRest] -> [NoteOrRest]
prependAnnFirstNote ann = annFirstNote ann (\b a -> a <> ", " <> b)

replaceAnnFirstNote :: String -> [NoteOrRest] -> [NoteOrRest]
replaceAnnFirstNote ann = annFirstNote ann (const id)

annFirstNote :: String -> (String -> String -> String ) -> [NoteOrRest] -> [NoteOrRest]
annFirstNote ann append = reverse . snd . foldl' foldlf (False,[])
  where
    foldlf :: (Bool,[NoteOrRest]) -> NoteOrRest -> (Bool,[NoteOrRest])
    foldlf (seen,ret) nor@(Right _) = (seen,nor:ret)
    foldlf (False,ret)   (Left note) = (True,Left (annNote note):ret)
    foldlf (True,ret) nor@(Left _) = (True,nor:ret)
    annNote note@Note{..} = if null _noteAnn
                            then
                              note { _noteAnn = ann }
                            else
                              note { _noteAnn = append _noteAnn ann }

rotN :: Int -> [a] -> [a]
rotN cnt as
  | cnt >= length as = error $ "rotN cnt: " <> show cnt <> " >= length as " <> show (length as)
  | otherwise = drop cnt as <> take cnt as

mPitchInt2MScaleDegree :: Scale -> (Maybe Pitch,Int) -> Maybe Int
mPitchInt2MScaleDegree _ (Nothing,_) = Nothing
mPitchInt2MScaleDegree Scale{..} (Just pitch,octOff) =
  ((+) (NE.length _scPitches * octOff) <$> elemIndex pitch pitches) <|> err
  where
    pitches = NE.toList _scPitches
    err = error $ "mPitch2ScaleDegree no pitch: " <> show pitch <> " in pitches for scale: " <> show pitches

mInts2IntDiffs :: [Maybe Int] -> [Maybe Int]
mInts2IntDiffs = snd . foldl foldlf (0,[])
  where
    foldlf (prev,ret) Nothing  = (prev,ret <> [Nothing])
    foldlf (prev,ret) (Just i) = (i,ret <> [Just (i - prev)])

mkNoteOrRest :: Maybe (Pitch, Octave) -> Duration -> Accent -> Either Note Rest
mkNoteOrRest (Just (p,o)) d a = Left $ Note p o d (singleton a) NoDynamic  NoSwell "" False
mkNoteOrRest Nothing d _ = Right $ Rest d  NoDynamic

voiceConfig2NoteOrRests :: VoiceConfig -> Driver [NoteOrRest]
voiceConfig2NoteOrRests VoiceConfigXPose{..} =
  genXPose (nes2arrs _vcxDurss) (nes2arrs _vcxAcctss) (nes2arrs _vcxMPitOctss) _vcxScale (Range _vcxRange)
voiceConfig2NoteOrRests VoiceConfigRepeat{..} =
  genStatic (nes2arrs _vcrDurss) (nes2arrs _vcrAcctss) (nes2arrs _vcrMPitOctss) _vcrScale _vcrRegister _vcrDurVal
voiceConfig2NoteOrRests VoiceConfigCanon{..} =
  genCanon (nes2arrs _vccDurss) (nes2arrs _vccAcctss) (nes2arrs _vccMPitOctss)_vccScale _vccRegister _vccDurVal _vccRotVal

voiceConfig2Rests :: VoiceConfig -> Driver [NoteOrRest]
voiceConfig2Rests voiceConfig = voiceConfig2NoteOrRests voiceConfig <&> notes2Rests

notes2Rests :: [NoteOrRest] -> [NoteOrRest]
notes2Rests = fmap (either note2Rest Right)
  where
    note2Rest Note{..} = Right (Rest _noteDur NoDynamic)

applyMConfigMods :: Maybe (NE.NonEmpty String) -> (VoiceRuntimeConfig, VoiceConfig) -> Driver (VoiceRuntimeConfig,VoiceConfig)
applyMConfigMods mNames pr = applyMMods mNames pr (applyMod name2VoiceConfigMods) <&> (fst pr,)

applyMNOrRsMods :: Maybe (NE.NonEmpty String) -> (VoiceRuntimeConfig,[NoteOrRest]) -> Driver [NoteOrRest]
applyMNOrRsMods mNames pr = applyMMods mNames pr (applyMod name2VoiceNOrRsMods)

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

sectionConfig2NoteOrRests :: SectionConfig -> Driver [[NoteOrRest]]
sectionConfig2NoteOrRests (SectionConfigNeutral scnPath cntSegs mSctnName mConfigMods mNOrRsMods voiceConfigs) = do
  traverse (traverse (applyMConfigMods mConfigMods)) prs >>= traverse (concatMapM cvtAndApplyMod) <&> addSecnName scnName
  where
    cvtAndApplyMod (rtTup,cfgTup) = voiceConfig2NoteOrRests cfgTup >>= applyMNOrRsMods mNOrRsMods . (rtTup,)
    scnName = fromMaybe "neutral" mSctnName
    cntVocs = length voiceConfigs
    segRuntimeTupss = chunksOf cntSegs [VoiceRuntimeConfig scnPath Nothing cntVocs numVoc cntSegs numSeg | numVoc <- [0..cntVocs - 1], numSeg <- [0..cntSegs - 1]]
    prs = zipWith (\rtups cfgtup -> (,cfgtup) <$> rtups) segRuntimeTupss (NE.toList voiceConfigs)
sectionConfig2NoteOrRests (SectionConfigFadeIn scnPath fadeIxs mSctnName mConfigMods mNOrRsMods voiceConfigs) =
  foldM foldMf ([],replicate cntVocs []) fadeIxs <&> addSecnName scnName . snd
  where
    scnName = fromMaybe "fade in" mSctnName
    cntSegs = length fadeIxs
    cntVocs = length voiceConfigs
    mkVocRTT nSeg nVoc =  VoiceRuntimeConfig scnPath Nothing cntVocs nVoc cntSegs nSeg
    foldMf (seenNumVocs,prevNOrRss) numVoc = do
      let numSeg = length seenNumVocs
          vocCfgTup = voiceConfigs NE.!! numVoc
          vocRTTup  = VoiceRuntimeConfig scnPath (Just numVoc) cntVocs numVoc cntSegs numSeg
      seenNOrRs <- applyMConfigMods mConfigMods (vocRTTup,vocCfgTup) >>= voiceConfig2NoteOrRests . snd >>= applyMNOrRsMods mNOrRsMods . (vocRTTup,)
      let vocRunTups = mkVocRTT numSeg <$> seenNumVocs
          vocCfgTups = (voiceConfigs NE.!!) <$> seenNumVocs
      seenNumVocsNOrRss <- traverse (applyMods mConfigMods mNOrRsMods) (zip vocRunTups vocCfgTups)
      let seenNumVocNOrRs    = (numVoc,seenNOrRs)
          allSeenNumVocs     = numVoc:seenNumVocs
          allUnseenNumVocs   = [0..(cntVocs - 1)] \\ allSeenNumVocs
          unseenNumVocNOrRss = (,notes2Rests seenNOrRs) <$> allUnseenNumVocs
          newNOrRss          = snd <$> sort (seenNumVocNOrRs:seenNumVocsNOrRss <> unseenNumVocNOrRss)
      pure (allSeenNumVocs,zipWith (<>) prevNOrRss newNOrRss)
sectionConfig2NoteOrRests (SectionConfigFadeOut scnPath fadeIxs mSctnName mConfigMods mNOrRsMods voiceConfigs) = do
  let initNumVocs = [0..cntVocs - 1]
      vocRunTups = mkVocRTT 0 <$> initNumVocs
      vocCfgTups = (voiceConfigs NE.!!) <$> initNumVocs
  inits <- traverse (applyMods mConfigMods mNOrRsMods) (zip vocRunTups vocCfgTups)
  nOrRss <- foldM foldMf ([],snd <$> inits) fadeIxs <&> addSecnName scnName . snd
  let nOrRsDurs = nOrRs2DurVal <$> nOrRss
  pure $ zipWith3 (mkNoRsTotDur (maximum nOrRsDurs)) nOrRsDurs timeSigs nOrRss
  where
    cntSegs = length fadeIxs
    cntVocs = length voiceConfigs
    scnName = fromMaybe "fade out" mSctnName
    timeSigs = NE.toList $ _vccTime <$> voiceConfigs
    mkVocRTT nSeg nVoc =  VoiceRuntimeConfig scnPath Nothing cntVocs nVoc cntSegs nSeg
    foldMf (seenNumVocs,nOrRss) numVoc = do
      let numSeg = 1 + length seenNumVocs
          seenNumVocs' = numVoc:seenNumVocs
          unseenNumVocs = [0..(cntVocs - 1)] \\ seenNumVocs'
          vocRunTups = mkVocRTT numSeg <$> unseenNumVocs
          vocCfgTups =  (voiceConfigs NE.!!) <$> unseenNumVocs
      unseenNumVocsNOrRss <- traverse (applyMods mConfigMods mNOrRsMods) (zip vocRunTups vocCfgTups)
      let seenNumVocsEmpties = (,[]) <$> seenNumVocs'
          newNOrRss = snd <$> sort (seenNumVocsEmpties <> unseenNumVocsNOrRss)
      pure (seenNumVocs',zipWith (<>) nOrRss newNOrRss)

applyMods :: Maybe (NE.NonEmpty String) -> Maybe (NE.NonEmpty String) -> (VoiceRuntimeConfig,VoiceConfig) -> Driver (Int,[NoteOrRest])
applyMods mConfigMods mNOrRsMods pr =
  applyMConfigMods mConfigMods pr >>= voiceConfig2NoteOrRests . snd >>= applyMNOrRsMods mNOrRsMods . (vrt,) <&> (numVoc,)
  where
    vrt = fst pr
    numVoc = _vrcNumVoc vrt

addSecnName :: String -> [[NoteOrRest]] -> [[NoteOrRest]]
addSecnName scnName voices = prependAnnFirstNote scnName <$> voices

genSplitStaffVoc :: Instrument -> KeySignature -> TimeSignature -> [VoiceEvent] -> Voice
genSplitStaffVoc instr keySig timeSig ves
  = SplitStaffVoice instr (VeKeySignature keySig NE.<| VeTimeSignature timeSig NE.<| NE.fromList ves)

-- to avoid cluttering score with repeats of the same dynamic, accent,
tagFirstNotes :: (Accent,Dynamic) -> ([VoiceEvent],[VoiceEvent]) -> ([VoiceEvent],[VoiceEvent])
tagFirstNotes (acc,dyn) = bimap tagFirstNote tagFirstNote
  where
    tagFirstNote ves = maybe ves (`tagNoteForIdx` ves) (findIndex isNote ves)
    tagNoteForIdx idx = toList . adjust tagNote idx .  fromList
    tagNote (VeNote (Note p o d _ _ swell ann tie)) = VeNote (Note p o d (singleton acc) dyn swell ann tie)
    tagNote ve = error $ "tagNote, VoiceEvent is not VeNote: " <> show ve
    isNote VeNote {} = True
    isNote _ = False

-- maxLen and vesLen are in 128th notes
-- maxLen is target length so all voices are equal length
-- vesLen is actual length maybe same as maxLen
mkVesPrTotDur :: Int -> Int -> TimeSignature -> [VoiceEvent] -> [VoiceEvent]
mkVesPrTotDur maxLen vesLen timeSig ves =
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

mkNoRsTotDur :: Int -> Int -> TimeSignature -> [NoteOrRest] -> [NoteOrRest]
mkNoRsTotDur maxLen nOrRsLen timeSig =
  addLenToNoRs
  where
    beatLen = dur2DurVal (timeSig2Denom timeSig)
    barLen  = timeSig2Num timeSig * beatLen
    remBar  = if maxLen `rem` barLen == 0 then 0 else barLen - (maxLen `rem` barLen)
    addLen  = if maxLen > nOrRsLen then (maxLen - nOrRsLen) + remBar else remBar
    addLenToNoRs = flip (<>) (Right . flip Rest NoDynamic <$> addEndDurs timeSig nOrRsLen addLen)

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

-- first bool is True for first element in list, second bool is True for last element in list
-- for first note, want tie and annotations, for middle notes,
-- for middle notes, want tie and no annotations
-- for last note, want no tie and no annotations
fixNoteOrRestTie :: Bool -> Bool -> NoteOrRest -> NoteOrRest
fixNoteOrRestTie True False (Left note)     = Left note { _noteTie = True }
fixNoteOrRestTie False tie  (Left Note{..}) = Left $ Note _notePit _noteOct _noteDur (singleton NoAccent) NoDynamic NoSwell _noteAnn (not tie)
fixNoteOrRestTie True True  _               = error "stripAccentAndDynamic unexpected True for both first and last flags"
fixNoteOrRestTie _    _     (Right rest)    = Right rest

-- tied-to notes have no annotation
stripAnnotation :: NoteOrRest -> NoteOrRest
stripAnnotation (Left Note{..}) = Left $ Note _notePit _noteOct _noteDur (singleton NoAccent) NoDynamic NoSwell "" _noteTie
stripAnnotation (Right rest) = Right rest

nOrRs2DurVal :: [NoteOrRest] -> Int
nOrRs2DurVal = sum . fmap nOrR2DurVal

nOrR2DurVal :: NoteOrRest -> Int
nOrR2DurVal (Left Note{..}) = dur2DurVal _noteDur
nOrR2DurVal (Right Rest{..}) = dur2DurVal _restDur

-- apportion [Right Rest] or [Left Note] durations according to place in bar given time signature
-- 1) map [NoteOrRest] to [[NoteOrRest]] by [[Left Note]..[Right Rest]]
-- 2) fold over uniform [NoteOrRest] into [NoteOrRest] according to current position
--    in total [NoteorRest] by 1/128th notes mapped into position within bar, keeping
--    track of position by first element in pair, output in second element of pair
--    2a) for [Right Rest], sum vals for all contiguous rests, call addEndDurs given
--        time signature and current position and map all to Right Rest
--    2b) for [Left Note], fold again over each calling addEndDurs to break into tied
--         Left Note given time signature and current position
alignNoteOrRestsDurations :: TimeSignature -> [NoteOrRest] -> [VoiceEvent]
alignNoteOrRestsDurations timeSig =
  fmap (either VeNote VeRest) . snd . foldl' foldlf (0,[]) . groupBy ((==) `on` isRight)
  where
    foldlf (curLen,ret) allRests@((Right _):_) =
      (curLen + addLen,ret <> newRests)
      where
        addLen = nOrRs2DurVal allRests
        durs = addEndDurs timeSig curLen addLen
        newRests = Right . flip Rest NoDynamic <$> durs
    foldlf (curLen,ret) allNotes@((Left _):_) =
      foldl' foldlf' (curLen,ret) allNotes
      where
        foldlf' (curLen',ret') (Left note@Note{..}) =
          (curLen' + addLen,ret' <> (stripAnnotations . fixTies $ newNotes))
          where
            addLen = dur2DurVal _noteDur
            durs = addEndDurs timeSig curLen' addLen
            newNotes = Left . (\dur -> note {_noteDur = dur}) <$> durs
            fixTies nOrRs
              | length nOrRs < 2 = nOrRs
              | length nOrRs == 2 = [firstNOrR,lastNOrR]
              | otherwise = [firstNOrR] <> midNOrRs <> [lastNOrR]
              where
                firstNOrR = fixNoteOrRestTie True False (head nOrRs)
                lastNOrR  = fixNoteOrRestTie False True (last nOrRs)
                midNOrRs  = fixNoteOrRestTie False False <$> drop 1 (init nOrRs)
            stripAnnotations nOrRs
              | length nOrRs < 2 = nOrRs
              | otherwise        = head nOrRs:(stripAnnotation <$> tail nOrRs)
        foldlf' (_,_) (Right rest) = error $ "alignNoteOrRestsDurations foldlf' unexpected Rest: " <> show rest
    foldlf (_,_) l = error $ "alignNoteOrRestsDurations unexpected list: " <> show l

