{-# Language PackageImports      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compose (config2MonoVoices
               ,groupAndTrimDursAndAddTempo
               ) where

import Data.Bifunctor (second)
import Control.Lens hiding (both)
import Control.Monad (foldM)
import "extra" Control.Monad.Extra (concatMapM)
import "monad-extras" Control.Monad.Extra (unfoldM)
import Data.Foldable
import Data.Function (on)
import Data.List (elemIndex, findIndex, findIndices, groupBy, isPrefixOf, unfoldr, transpose)
import qualified Data.List.NonEmpty as NE
import Data.List.Split (chunksOf, splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, catMaybes)
import Data.Sequence (adjust', fromList)
import Data.Traversable (mapAccumL)
import Data.Tuple.Extra (both, dupe, first, fst3, secondM, snd3, thd3, (&&&))
import Safe (lastMay)

import ConfigData
import Driver
import Lily (accent2Name)
import Types
import Utils

------------------------------------------
-- Section Config and Voice Config Mods --
------------------------------------------

-- Voice mod(ifier)s keep track of state
data VoiceRuntimeConfig =
  VoiceRuntimeConfig {
  _vrcSctnPath :: String
  ,_vrcMNumVoc  :: Maybe Int -- Voice index for fade-in accents, fade-in dynamcs voice mods
  ,_vrcMSpotIdx :: Maybe Int -- Voice index for spotDynamics config mod.
  ,_vrcCntVocs  :: Int       -- Total number of voices
  ,_vrcNumVoc   :: Int       -- Index for this voice
  ,_vrcCntSegs  :: Int       -- Total number of segments (reps from section config)
  ,_vrcNumSeg   :: Int       -- Index of this segment
  } deriving Show

-- Maps to lookup and invoke config and voice during sectionConfig2VoiceEventss

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
                                  ,("spotDyns",spotDynamics)
                                  ,("sustainNotes",sustainNotes)]

----------------------------------------
-- Section Config Mod Implementations --
----------------------------------------

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
modMPitOctssOctaves mkIdWeight vrtCfg vcv@VoiceConfigVerbatim{..} =
  modAnyMPitOctssOctaves mkIdWeight vrtCfg (_vcmPOOrPOss _vcvCore) <&> \mPOOrPOss -> vcv { _vcvCore =  _vcvCore { _vcmPOOrPOss = mPOOrPOss } }
modMPitOctssOctaves mkIdWeight vrtCfg vcr@VoiceConfigRepeat{..}   =
  modAnyMPitOctssOctaves mkIdWeight vrtCfg (_vcmPOOrPOss _vcrCore) <&> \mPOOrPOss -> vcr { _vcrCore = _vcrCore { _vcmPOOrPOss = mPOOrPOss } }
modMPitOctssOctaves mkIdWeight vrtCfg vcl@VoiceConfigSlice{..}     =
  modAnyMPitOctssOctaves mkIdWeight vrtCfg (_vcmPOOrPOss _vcsCore) <&> \mPOOrPOss -> vcl { _vcsCore = _vcsCore { _vcmPOOrPOss = mPOOrPOss } }
modMPitOctssOctaves mkIdWeight vrtCfg vcc@VoiceConfigBlend{..}    =
  modAnyMPitOctssOctaves mkIdWeight vrtCfg (_vcmPOOrPOss _vcbCore) <&> \mPOOrPOss -> vcc { _vcbCore = _vcbCore { _vcmPOOrPOss  = mPOOrPOss } }
modMPitOctssOctaves mkIdWeight vrtCfg vcx@VoiceConfigXPose{..}    =
  modAnyMPitOctssOctaves mkIdWeight vrtCfg (_vcmPOOrPOss _vcxCore) <&> \mPOOrPOss -> vcx { _vcxCore = _vcxCore { _vcmPOOrPOss = mPOOrPOss } }
modMPitOctssOctaves mkIdWeight vrtCfg vca@VoiceConfigAccrete{..}    =
  modAnyMPitOctssOctaves mkIdWeight vrtCfg (_vcmPOOrPOss _vcaCore) <&> \mPOOrPOss -> vca { _vcaCore = _vcaCore { _vcmPOOrPOss = mPOOrPOss } }

type MPitOctOrNEPitOctsss = NE.NonEmpty (NE.NonEmpty (Maybe PitOctOrNEPitOcts))

modAnyMPitOctssOctaves :: (Int -> Int -> Int ) -> VoiceRuntimeConfig -> MPitOctOrNEPitOctsss -> Driver MPitOctOrNEPitOctsss
modAnyMPitOctssOctaves mkIdWeight VoiceRuntimeConfig{..} = 
  traverse (traverse randomizeMPitOcts)
  where
    randomizeMPitOcts Nothing            = pure Nothing
    randomizeMPitOcts (Just (Left pr))   = randomizeMPitOct pr <&> Just . Left
    randomizeMPitOcts (Just (Right prs)) = traverse randomizeMPitOct prs <&> Just . Right
    randomizeMPitOct (PitOct pit oct) = randomWeightedElement weights <&> (\f -> PitOct pit (f oct))
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
doubleCfgDurs vrtCfg vcv@VoiceConfigVerbatim{} = mRunDoubleCfgMod (vcv & vcvCore . vcDurss %~ doubleDurs) vrtCfg vcv
doubleCfgDurs vrtCfg vcr@VoiceConfigRepeat{}   = mRunDoubleCfgMod (vcr & vcrCore . vcDurss %~ doubleDurs) vrtCfg vcr
doubleCfgDurs vrtCfg vcc@VoiceConfigSlice{}    = mRunDoubleCfgMod (vcc & vcsCore . vcDurss %~ doubleDurs) vrtCfg vcc
doubleCfgDurs vrtCfg vcc@VoiceConfigBlend{}    = mRunDoubleCfgMod (vcc & vcbCore . vcDurss %~ doubleDurs) vrtCfg vcc
doubleCfgDurs vrtCfg vcx@VoiceConfigXPose{}    = mRunDoubleCfgMod (vcx & vcxCore . vcDurss %~ doubleDurs) vrtCfg vcx
doubleCfgDurs vrtCfg vca@VoiceConfigAccrete{}  = mRunDoubleCfgMod (vca & vcaCore . vcDurss %~ doubleDurs) vrtCfg vca

mRunDoubleCfgMod :: VoiceConfig -> VoiceRuntimeConfig -> VoiceConfig -> Driver VoiceConfig
mRunDoubleCfgMod vcMod VoiceRuntimeConfig{..} vCfg  = do
  mod' <- searchMConfigParam (_vrcSctnPath <> ".dblCfgMod") <&> fromMaybe (_vrcCntSegs `div` _vrcCntVocs)
  pure $ if _vrcNumVoc * mod' <= _vrcNumSeg then vcMod else vCfg

doubleDurs :: NE.NonEmpty (NE.NonEmpty DurValOrDurTuplet) -> NE.NonEmpty (NE.NonEmpty DurValOrDurTuplet)
doubleDurs = (fmap . fmap) doubleDurOrDurTup

doubleDurOrDurTup :: DurValOrDurTuplet -> DurValOrDurTuplet
doubleDurOrDurTup = bimap (* mkDurationVal 2) (multDurTuplet 2)

multDurTuplet :: Int -> DurTuplet -> DurTuplet
multDurTuplet i tup = tup & durtupUnitDuration %~ multDur i & durtupDurations %~ fmap (* mkDurationVal i)

--------------------------------------
-- Voice Config Mod Implementations --
--------------------------------------

-- Uniform accents are specific to midi, score gets annotation
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

-- TBD: annotate first note with "sempre <accent>", would be nice not to repeat annotation for non-highlighted segments
-- uniform accents are specific to midi, score gets annotation.  As this is, there's no indication of accents in score.
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

-- Specify one dynamic for fade-in voice vs. another dynamic for all other voices.
fadeInDynamics :: VoiceEventsMod
fadeInDynamics VoiceRuntimeConfig{..} ves = do
  dyn1 <- searchMConfigParam (_vrcSctnPath <> ".fadeInDyn1") <&> fromMaybe Forte
  dyn2 <- searchMConfigParam (_vrcSctnPath <> ".fadeInDyn2") <&> fromMaybe PPP
  pure $ case _vrcMNumVoc of
    Just _  -> tagFirstSoundDynamic dyn1 ves -- fade-in voice signaled via Just <index>
    Nothing -> tagFirstSoundDynamic dyn2 ves -- non fade-in voices get second dynamic

-- TBD: need a more robust way of specifying swells and dynamics over the 
-- course of an entire voice.  This is a voice mod, which means it applies on
-- a segment-by-segment (i.e. repetition) basis for a given voice, which in
-- effect means the only easily recognizeable points are the first segment,
-- the last segment, and all segments that are neither.  For full flexibility,
-- I'd need a tagged list of dynamic, swell+dynamic, or dynamic+sell pairs
-- with the tag specifying the segment number.  And change voice mod method name.
uniformDynamics :: VoiceEventsMod
uniformDynamics VoiceRuntimeConfig{..} inVes = do
  ves1 <- searchMConfigParam (_vrcSctnPath <> ".uniformDyn") <&> maybeInsertUniDyn inVes
  ves2 <- searchMConfigParam (_vrcSctnPath <> ".crescDurVal") >>= maybeInsertSwell Crescendo ves1
  -- This is a hack to fix a bug overwriting cresc with decresc when both are specified.
  -- But it means within a given segment there can't be both cresc and decresc.
  -- Also, swellDyn only at end of swell that starts at the beginning of the segment.
  if ves1 == ves2
  then searchMConfigParam (_vrcSctnPath <> ".decrescDurVal") >>= maybeInsertSwell Decrescendo ves2
  else pure ves2
  where
    maybeInsertUniDyn vs Nothing    = vs
    maybeInsertUniDyn vs (Just dyn) = if isFirstSeg then tagFirstSoundDynamic dyn vs else vs
    maybeInsertSwell _     vs Nothing = pure vs
    maybeInsertSwell swell vs (Just durVal) 
      | durVal < 0 = pure $ maybeTagSwell swell durVal vs Nothing -- last segment, new dyn at start of next section
      | otherwise  = searchConfigParam (_vrcSctnPath <> ".swellDyn") <&> maybeTagSwell swell durVal vs . Just
    maybeTagSwell swell dur vs mDyn =
      if (dur >= 0 && isFirstSeg) || isLastSeg
         then tagSwell swell dur vs mDyn
         else vs
    isFirstSeg = _vrcNumSeg == 0
    isLastSeg  = _vrcNumSeg == pred _vrcCntSegs
              
-- Spread a list of per-section dynamics across all voices, section-by-section.                     
sectionDynamics :: VoiceEventsMod
sectionDynamics VoiceRuntimeConfig{..} ves = 
  searchConfigParam (_vrcSctnPath <> ".sectionDyns") <&> flip tagFirstSoundDynamic ves . (NE.!! _vrcNumSeg)

-- Spread a dynamic across a list of voices by voice index, unless there's no list configured (TBD: should be an error).
voicesDynamics :: VoiceEventsMod
voicesDynamics vrtc@VoiceRuntimeConfig{..} ves = do
  voicesDyn <- searchMConfigParam (_vrcSctnPath <> ".vocsDyn") <&> fromMaybe MF
  mIdxs     <- searchMConfigParam (_vrcSctnPath <> ".vocsDynIdxs")
  voicesDynamics' voicesDyn mIdxs vrtc ves
  where
    voicesDynamics' :: Dynamic -> Maybe (NE.NonEmpty Int) -> VoiceEventsMod
    voicesDynamics' voicesDyn' mIdxs VoiceRuntimeConfig{..} ves'
      | _vrcNumSeg == 0 && isMElem _vrcNumVoc mIdxs = pure $ tagFirstSoundDynamic voicesDyn' ves'
      | otherwise = pure ves'
     where
        isMElem idx = maybe True (idx `elem`)

-- spotlight dynamics: bring random voice generated 
-- in sectionConfig2VoiceEventss to foreground 
-- with cresc to foreDyn that lasts crescDurVal,
-- decresc to backDyn that lasts decrescDurVal,
-- all other voices stay at backDyn
--
-- insert actions:
--  * backDyn at very beginning
--  * cresc after current duration >= delayDurVal
--  * foreDyn after current duration plus duration of next VE >= delayDurVal + crescDurVal
--  * descresc after current duration >= total duration - (decrescDurVal + delayDurVal)
--  * backDyn after current duration plus duration of next VE >= total duration - delayDurVal
spotDynamics :: VoiceEventsMod
spotDynamics VoiceRuntimeConfig{..} ves = do
  foreDyn       <- searchConfigParam (_vrcSctnPath <> ".foreDyn")
  backDyn       <- searchConfigParam (_vrcSctnPath <> ".backDyn")
  delayDurVal   <- searchConfigParam (_vrcSctnPath <> ".delayDurVal")
  crescDurVal   <- searchConfigParam (_vrcSctnPath <> ".crescDurVal")
  decrescDurVal <- searchConfigParam (_vrcSctnPath <> ".decrescDurVal")
  pure $ spotDyns _vrcNumVoc foreDyn backDyn delayDurVal crescDurVal decrescDurVal (tagFirstSoundDynamic backDyn ves)
  where
    totDurVal = ves2DurVal ves
    spotIdx = fromMaybe (error "spotDyns missing index") _vrcMSpotIdx
    spotDyns vocIdx foreDyn backDyn delay cresc decresc  ves'
      | vocIdx == spotIdx =  snd $ mapAccumL mapAccumF (0,offFuns) ves'
      | otherwise = ves'
      where
        after  testOff totOff _       = totOff >= testOff
        before testOff totOff thisOff = totOff + thisOff >= testOff
        mInsDyn dyn test totOff thisOff ve = if test totOff thisOff then Just (tagControl (CtrlDynamic dyn) ve) else Nothing
        mInsSwell swell test totOff thisOff ve = if test totOff thisOff then Just (tagControl (CtrlSwell swell) ve) else Nothing
        offFuns :: [Int -> Int -> VoiceEvent -> Maybe VoiceEvent]
        offFuns = [mInsSwell Crescendo (after delay)
                  ,mInsDyn foreDyn (before (delay + cresc)) 
                  ,mInsSwell Decrescendo (after (totDurVal - (decresc + delay)))
                  ,mInsDyn backDyn (before (totDurVal - delay))]
        mapAccumF (totOff,funs) ve
          | null funs = ((totOff,[]),ve)
          | otherwise = maybe ((totOff',funs),ve) ((totOff',tail funs),) (head funs totOff thisOff ve)
          where
            thisOff = ve2DurVal ve
            totOff' = totOff + thisOff

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

--------------------------------------------------------------------------------
-- VoiceConfig gen[XPose | Slice | Verbatim | Repeat | Blend] helper routines --
--------------------------------------------------------------------------------

type Mottos   = ([Maybe PitOctOrPitOcts],[DurValOrDurTuplet],[Accent])
type Mottoss = ([[Maybe PitOctOrPitOcts]],[[DurValOrDurTuplet]],[[Accent]])

type GenMottos = VoiceConfigCore -> Int -> Driver Mottos
type GenMottoss = VoiceConfigCore -> Int -> Driver Mottoss

accumVoiceEventsByRange :: Scale -> Range -> Mottoss -> [VoiceEvent]
accumVoiceEventsByRange scale (start,stop) (mpitss,durss,acctss) =
  accumVoiceEventsForFiniteMPits (mpits',concat durss,concat acctss)
  where
    mpits' = takeWhile testRange $ concat (xposeFromMPitOctOrPitOctss scale start mpitss)
    testRange = maybe True (poInRange . poOrPOsToPO)
    poInRange po = if start < stop then po <= stop else po >= stop

-- mpits is finite, durs and accs are infinite,
-- accumulate by one DurOrDurTuplet at a time,
-- each iteration may consume one or more mpits and accs
-- but mpits is finite so it's going to run out first
accumVoiceEventsForFiniteMPits :: Mottos -> [VoiceEvent]
accumVoiceEventsForFiniteMPits ([],_,_) = []
accumVoiceEventsForFiniteMPits (mpits,dur:durs,accs) =
  ve : accumVoiceEventsForFiniteMPits (mpits',durs,accs')
  where
    ((mpits',accs'),ve) = mkve mpits accs dur
    mkve :: [Maybe PitOctOrPitOcts] -> [Accent] -> DurValOrDurTuplet -> (([Maybe PitOctOrPitOcts],[Accent]),VoiceEvent)
    mkve mps acs (Left d)  = ((tail mps,tail acs),mkNoteChordOrRest (head mps) d (head acs))
    mkve mps acs (Right t) = mkTuplet mps t acs
accumVoiceEventsForFiniteMPits  (_,_,_) = error "accumVoiceEventsForFiniteMPits"

accumVoiceEventsForFiniteDurs :: Mottos -> [VoiceEvent]
accumVoiceEventsForFiniteDurs (mpits,durs,accts) =
  snd $ mapAccumL mapAccumF (mpits,accts) durs
   where
     mapAccumF (mp:mps,ac:accs) (Left dur)     = ((mps,accs),mkNoteChordOrRest mp dur ac)
     mapAccumF (mps,accs)       (Right durtup) = mkTuplet mps durtup accs
     mapAccumF (mps,accs) _ = error $ "accumVoiceEvents unexpected inputs, mpits: " <> show mps <> " accts: " <> show accs

genVoiceEventsByRange :: String -> GenMottoss -> String -> VoiceConfigCore -> Scale -> Range -> Driver [VoiceEvent]
genVoiceEventsByRange vtName core2Mottoss path core scale range = do
  showVType::Int <- searchMConfigParam (path <> ".showVType") <&> fromMaybe 1
  rotVal::Int    <- searchMConfigParam (path <> ".rotVal") <&> fromMaybe 0
  ves <- core2Mottoss core rotVal <&> accumVoiceEventsByRange scale range 
  pure $ if 0 == showVType then ves else appendAnnFirstNote vtName ves

genVoiceEventsByAccrete :: String -> TimeSignature -> String -> VoiceConfigCore -> Int -> (KeySignature,PitOct)  -> Driver [VoiceEvent]
genVoiceEventsByAccrete vaName timeSig path VoiceConfigCore{..} numBars (keySig,start) = do
  showVType::Int <- searchMConfigParam (path <> ".showVType") <&> fromMaybe 1
  ves <- accreteVoiceByMotif barDurVal totDurVal mIOrIsss dVAOrDTAss (keySig,start)
  pure $ if 0 == showVType then ves else appendAnnFirstNote vaName ves
  where
    scale      = keySig2Scale M.! keySig
    barDurVal  = timeSig2BarDurVal timeSig
    totDurVal  = DurationVal numBars * barDurVal
    mIOrIsss   = mPOOrPOsToMIOrIsDiffs scale <$> neMXss2MArrsXss _vcmPOOrPOss
    dVAOrDTAss = zipWith mkDVAOrDTAss (nes2arrs _vcAcctss) (nes2arrs _vcDurss)

--------------------------------
-- VoiceConfigAccrete helpers --
--------------------------------

type DurVals  = (DurationVal,DurationVal)
type Motif    = ([Maybe IntOrInts],[DurValAccOrDurTupletAccs])
type MotifTup = (PitOct,DurVals,[Motif])

-- Accumulate a [Motif] appending or prepending a Motif at a time until filling duration totDur, then convert [Motif] to [VoiceEvent].
accreteVoiceByMotif :: DurationVal -> DurationVal -> [[Maybe IntOrInts]] -> [[DurValAccOrDurTupletAccs]] -> (KeySignature,PitOct) -> Driver [VoiceEvent]
accreteVoiceByMotif (DurationVal barDur) (DurationVal totDur) intss durss (keySig,startPit) = 
  unfoldM (unfold2MotifTup scale barDur totDur intss durss) (False,initMotifTup) <&> concatMap (scaleAndMotifTup2VoiceEvents scale) -- TBD: keySig, then VeEvent KeySig
  where
    initMotifTup = (startPit,(DurationVal 0,DurationVal 0),[])
    scale = keySig2Scale M.! keySig
        
-- unfold2MotifTup generates the next MotifTup by adding one more Motif to the start or end (by isAppend) of the [Motif] contained in the MotifTup
unfold2MotifTup :: Scale -> Int -> Int -> [[Maybe IntOrInts]] -> [[DurValAccOrDurTupletAccs]] -> (Bool,MotifTup) -> Driver (Maybe (MotifTup,(Bool,MotifTup)))
unfold2MotifTup scale barDurVal totDurVal intss durss (isAppend,motifTup) = do
  motif <- (,) <$> randomElement intss <*> randomElement durss <&> mkEqLenLists
  let motifTup' = addMotif2MotifTup scale barDurVal isAppend motifTup motif
  pure $
    if totDurVal > motifTup2DurInt motifTup'
    then Just (motifTup',(not isAppend, motifTup'))
    else Nothing
  where
    -- make length of intervals match count of durations
    mkEqLenLists (iss, dss) = (take cntDurs (cycle iss),dss)
      where
        cntDurs = sum $ either (const 1) (length . _durtupDurations . fst) <$> dss

motifTup2DurInt :: MotifTup -> Int
motifTup2DurInt (_,(beg,end),motifs) = fromVal beg + fromVal end + motifs2DurInt motifs

motifs2DurInt :: [Motif] -> Int
motifs2DurInt motifs = fromVal . sum $ durValAccOrDurTupletAccs2DurVal <$> concatMap snd motifs

durValAccOrDurTupletAccs2DurVal :: DurValAccOrDurTupletAccs -> DurationVal
durValAccOrDurTupletAccs2DurVal = either fst (durTuplet2DurVal . fst)

-- Given scale and isAppend flag, either append or prepend Motif to MotifTup.
-- Appending is easy as it doesn't affect the starting pitch.
-- Prepending means counting backward to transpose the starting pitch so the
-- MotifTup ends with the correct starting pitch to successively transpose [Motif].
addMotif2MotifTup :: Scale -> Int -> Bool -> MotifTup -> Motif -> MotifTup
addMotif2MotifTup _     barDurInt True  (start,durs,motifs) motif = (start, durs',motifs')
  where
    motifs' = motifs ++ [motif]
    durs'   = mots2Durs True barDurInt durs motifs'
addMotif2MotifTup scale barDurInt False (start,durs,motifs) motif = (start',durs',motif : motifs)
  where
    start'  = xp scale start (negate . sumInts $ motif)
    sumInts = sum . map (either id head) . catMaybes . fst
    motifs' = motif : motifs
    durs'   = mots2Durs False barDurInt durs motifs'

-- Take the duration of a bar and the list of motifs, sum the duration vals in the motifs
-- to get their total duration in 128th notes, then compute the amount left over to reach
-- the next bar, and split that amount between the rest at the start and the rest at the end.
--
-- At start, want motif to start after reasonable rest at start, e.g. quarter, dotted quarter.
-- So just watch for (0,0) for DurationVals, make first as random choice of first, second, or
-- third quarter note.  Could eventually try to do something like look at how long the Motif
-- is and start after a shorter duration if there's a way to fit it into a single bar.  
-- Then terminating duration is just distance to the next bar.
--
-- Then when isAppend is True, leave first duration as is, and make last duration enough to
-- fill out the current bar.
--
-- When isAppend is False, leave last duration as is, and make first duration enough to fill
-- back to the start of the previous bar.
--
mots2Durs :: Bool -> Int -> (DurationVal,DurationVal) -> [Motif] -> (DurationVal,DurationVal)
mots2Durs _ barDurInt (DurationVal 0,DurationVal 0) motifs =
  (DurationVal initBegDurInt,DurationVal initEndDurInt)
  where
  initBegDurInt = dur2DurVal QDur
  initEndDurInt = remDurVal barDurInt initBegDurInt motifs
mots2Durs True barDurInt (DurationVal prevBegDurInt,_) motifs =
  (DurationVal prevBegDurInt,DurationVal newEndDurInt)
  where
    newEndDurInt = remDurVal barDurInt prevBegDurInt motifs
mots2Durs False barDurInt (_,DurationVal prevEndDurInt) motifs =
  (DurationVal newBegDurInt,DurationVal prevEndDurInt)
  where
    newBegDurInt = remDurVal barDurInt prevEndDurInt motifs

-- Remaining count of 128ths to reach start or end of bar given count of 
-- 128ths in a bar, count of 128ths for start or end offset, list of Motifs.
-- If the sum of the offset and the length of the motifs is exactly divisible
-- by the bar count, then answer 0 to avoid answering a full bar.
remDurVal :: Int -> Int -> [Motif] -> Int
remDurVal barDurInt offDurInt motifs =
  if 0 == remBar then 0 else barDurInt - remBar
  where
  remBar = (`rem` barDurInt) . (+ offDurInt) . motifs2DurInt $ motifs

-- Use the start from input (Pitch,Octave), the initial and ending rests, and to create a [VoiceEvent] for this MotifTup.
scaleAndMotifTup2VoiceEvents ::  Scale -> MotifTup -> [VoiceEvent]
scaleAndMotifTup2VoiceEvents scale (motStart,(startRest,stopRest),motifs) = 
  VeRest (Rest startRest []) : (concat . snd $ mapAccumL mapAccumF1 motStart motifs) <> [VeRest (Rest stopRest [])]
  where
    -- Generate [VoiceEvent] using starting (Pitch,Octave), carrying updated (Pitch,Octave) for next Motif
    mapAccumF1 :: PitOct -> Motif -> (PitOct,[VoiceEvent])
    mapAccumF1 start (ints,durs) = (start',concat vess)
      where
        ((start',_),vess) = mapAccumL mapAccumF2 (start,ints) durs
        -- Pair as many [Maybe IntOrInts] as needed for next DurValAccOrDurTupletAccs to generate next [VoiceEvent]
        mapAccumF2 :: (PitOct,[Maybe IntOrInts]) -> DurValAccOrDurTupletAccs -> ((PitOct,[Maybe IntOrInts]),[VoiceEvent])
        -- Trivial case:  (Left (DurationVal,Accent)) only requires one Maybe IntOrInts.
        mapAccumF2 (strt,i:is) (Left (durVal,accent)) = ((strt',is),[ve])
          where
            (strt',ve) = nextVE strt i durVal accent
        mapAccumF2 (strt,is) (Right (DurTuplet{..},accents)) = ((strt',is'),concat vess')
          where
            ((strt',is'),vess') = mapAccumL mapAccumF2 (strt,is) durs'
            durs' = zipWith (curry Left) (NE.toList _durtupDurations) accents
        mapAccumF2 x y = error $ "mapAccumF2 unexpected input: " <> show x  <> " " <> show y
        -- Generate Rest, Note, or Chord  depending on Maybe IntOrInts, DurationVal, and Accent, carry forward input (Pitch,Oct).
        nextVE :: PitOct -> Maybe IntOrInts -> DurationVal -> Accent -> (PitOct,VoiceEvent)
        -- Trivial case:  Nothing maps to Rest.
        nextVE pitOct Nothing durVal acc = (pitOct,VeRest $ Rest durVal (acc2Ctrls acc))
        -- Trivial case:  (Just (Left Int)) maps to a Note, carry updated (Pitch,Oct) for next transpose.
        nextVE pitOct (Just (Left int)) durVal acc = (PitOct pit' oct',VeNote $ Note pit' oct' durVal [] (acc2Ctrls acc) False)
          where
            PitOct pit' oct' = xp scale pitOct int
        -- Complex case: (Just (Right [Int])) maps to a Chord, carry updated (Pitch,Oct) for root to next transpose.
        nextVE pitOct (Just (Right is)) durVal acc = (head pitOcts,VeChord $ Chord (NE.fromList pitOcts) durVal [] (acc2Ctrls acc) False)
          where
            pitOcts = xp scale pitOct <$> is
        -- Swallow NoAccent when generating [Control]
        acc2Ctrls :: Accent -> [Control]
        acc2Ctrls NoAccent = []
        acc2Ctrls acc = [CtrlAccent acc]

mkDVAOrDTAss :: [Accent] -> [DurValOrDurTuplet] -> [DurValAccOrDurTupletAccs]
mkDVAOrDTAss as = snd . mapAccumL mapAccumF (cycle as)
  where
    mapAccumF :: [Accent] -> DurValOrDurTuplet -> ([Accent],DurValAccOrDurTupletAccs)
    mapAccumF (a:as') (Left durVal)  = (as',Left (durVal,a))
    mapAccumF as'     (Right durTup) = (restAs,Right (durTup,tupAs))
      where
        cntDurs = length $ _durtupDurations durTup
        restAs  = drop cntDurs as'
        tupAs   = take cntDurs as'
    mapAccumF as' _                  = error $ "mkDVAOrDTAss unexpected accents: " <> show as'

-- Generate a [DurValOrDurTuplet] that's exactly as long as the input target
-- in 128th notes from an infinite length input [DurValOrDurTuplet].
-- When the duration of final DurValOrDurTuplet exceeds the target, 
-- trim it to a Left DurationVal with the remainding length.
trimDurValOrDurTups :: Int -> [DurValOrDurTuplet] -> [DurValOrDurTuplet]
trimDurValOrDurTups targ = accum 0
  where
    accum tot (valOrTup:valOrTups)
      | tot == targ = []
      | tot + valOrTupLen > targ = [Left . mkDurationVal $ targ - tot]
      | otherwise = valOrTup : accum (tot + valOrTupLen) valOrTups
      where
        valOrTupLen = either fromVal fromDurTup valOrTup
    accum tot [] = error $ "trimDurValOrDurTups underflow, total duration: " <> show tot <> " < target duration: " <> show targ
    fromDurTup tup@DurTuplet{..} = getDurSum (sumDurs (replicate (durTup2CntTups tup * _durtupDenominator) _durtupUnitDuration))
    
accumVoiceEventsByDur :: Int -> Mottos -> [VoiceEvent]
accumVoiceEventsByDur maxDurVal (mpitss,durs,acctss) =
  accumVoiceEventsForFiniteDurs (mpitss,durs',acctss)
  where
    durs' = trimDurValOrDurTups maxDurVal durs

genVoiceEventsByDur :: String -> GenMottos -> String -> VoiceConfigCore -> Int -> Driver [VoiceEvent]
genVoiceEventsByDur vtName core2Mottos path core maxDurVal = do
  showVType::Int <- searchMConfigParam (path <> ".showVType") <&> fromMaybe 1
  rotVal::Int    <- searchMConfigParam (path <> ".rotVal") <&> fromMaybe 0
  ves <- core2Mottos core rotVal <&> accumVoiceEventsByDur maxDurVal
  pure $ if 0 == showVType then ves else appendAnnFirstNote vtName ves

-- For each of the core list of lists:
-- a) convert nonempty list of lists to ordinary list of lists
-- b) cycle to create an endless list in order, e.g.
--    cycle [[1],[2]] -> [[1],[2],[1],[2],[1],[2],..]
-- c) concat: [[1],[2],[1],[2],[1],[2],..] -> [1,2,1,2,1,2,..]
-- Note: verbatim means literal, so if the sublists are of different
-- lengths, then the patterns between e.g. pitches a, b, and c won't
-- overlap with the durations of eighth and eighth or accents >, >, >, +
-- For regular groupings of all three then the sublists in the config
-- file must be all of the same lengths.
core2InfVerbatimMottos :: GenMottos
core2InfVerbatimMottos VoiceConfigCore{..} _ =
  pure (mkCycle neMXss2MArrsXss _vcmPOOrPOss, mkCycle nes2arrs _vcDurss, mkCycle nes2arrs _vcAcctss)
  where
    mkCycle cnv = concat . cycle . cnv

-- For each of the core list of lists:
-- a) convert nonempty list of lists to ordinary list of lists
-- b) randomly reorder the inside lists,
--    e.g. [[1,2],[3],[4,5]] -> [[3],[1,2],[4,5]]
-- b) cycle to create an endless list of list in order
-- c) concat to create an endless list
core2InfRepeatMottos :: GenMottos
core2InfRepeatMottos VoiceConfigCore{..} _ =
  (,,) <$> mkCycle neMXss2MArrsXss _vcmPOOrPOss <*> mkCycle nes2arrs _vcDurss <*> mkCycle nes2arrs _vcAcctss
  where
    mkCycle cnv xss = randomizeList (cnv xss) <&> concat . cycle

core2InfSliceMottos :: GenMottos
core2InfSliceMottos core@VoiceConfigCore{..} _ =
  randomIndices (length _vcDurss) <&> mkSlices core

core2InfBlendMottos :: GenMottos
core2InfBlendMottos VoiceConfigCore{..} rotVal =
  (,,) <$> mkCycle neMXss2MArrsXss _vcmPOOrPOss <*> mkCycle nes2arrs _vcDurss <*> mkCycle nes2arrs _vcAcctss
  where
    mkCycle cnv xss = randomElements (cnv xss) <&> concatMap (rotN rotVal)

core2InfBlendMottoss :: GenMottoss
core2InfBlendMottoss VoiceConfigCore{..} rotVal =
  (,,) <$> mkCycle neMXss2MArrsXss _vcmPOOrPOss <*> mkCycle nes2arrs _vcDurss <*> mkCycle nes2arrs _vcAcctss
  where
    mkCycle cnv xss = randomElements (cnv xss) <&> map (rotN rotVal)

-- Helpers

mkSlices :: VoiceConfigCore -> [Int] -> Mottos
mkSlices VoiceConfigCore{..} manyIs =
  (ixByManyIs pitss',ixByManyIs durss',ixByManyIs acctss')
  where
    ixByManyIs ss = concatMap (ss !!) manyIs
    (pitss',durss',acctss') = mkEqLenMottoss (neMXss2MArrsXss _vcmPOOrPOss, nes2arrs _vcDurss,nes2arrs _vcAcctss)

-- Make sublists of equal counts of items, allowing for possible multiple durs in DurTuplet.
mkEqLenMottoss :: Mottoss -> Mottoss
mkEqLenMottoss (mpos:mposs,durs:durss,accs:accss) =
  (mpos':mposs',durs':durss',accs':accss')
  where
    (mpos',durs',accs') = mkEqLenMottos (mpos,durs,accs)
    (mposs',durss',accss') = mkEqLenMottoss (mposs,durss,accss)
mkEqLenMottoss (mposs,durss,accss) = error $ "mkEqLenMottoss unexpected uneven length lists (mpos,durs,accs): " <> show (length mposs,length durss,length accss)

-- Make lists of equal counts of items, allowing for possible multiple durs in DurTuplet.
-- Incrementally add to numDurs beyond cntDurs durs until numDurs is >= maximum [length mpos,length accs]
mkEqLenMottos :: Mottos -> Mottos
mkEqLenMottos (mpos,durs,accs) =
  (take numDurs (cycle mpos), takeNDurs numDurs (cycle durs), take numDurs (cycle accs))
  where
    numDurs = sumFromMinToNext cntDurValOrDurTup (max (length mpos) (length accs)) (cntDurValOrDurTups durs) (cycle durs)

cntDurValOrDurTup :: DurValOrDurTuplet -> Int
cntDurValOrDurTup = either (const 1) (length . _durtupDurations)

cntDurValOrDurTups :: [DurValOrDurTuplet] -> Int
cntDurValOrDurTups = sum . fmap cntDurValOrDurTup

sumFromMinToNext :: (a -> Int) -> Int -> Int -> [a] -> Int
sumFromMinToNext a2i lim = sum'
  where
    sum' cnt (a:as) 
      | cnt >= lim = cnt
      | otherwise = sum' (cnt + a2i a) as
    sum' cnt [] = error $ "sumFromMinToNext underflow, lim: " <> show lim <> " cnt: " <> show cnt

takeNDurs :: Int -> [DurValOrDurTuplet] -> [DurValOrDurTuplet]
takeNDurs 0 _      = []
takeNDurs n (d:ds) = d : takeNDurs (n - cntDurValOrDurTup d) ds
takeNDurs _ []     = error "takeNDurs unexpected end of list"

appendAnnFirstNote :: String -> [VoiceEvent] -> [VoiceEvent]
appendAnnFirstNote ann = annFirstEvent ann (\new old -> if null old then new else old <> ", " <> new)

prependAnnFirstNote :: String -> [VoiceEvent] -> [VoiceEvent]
prependAnnFirstNote ann = annFirstEvent ann (\new old -> if null old then new else  new <> ", " <> old)

annFirstEvent :: String -> (String -> String -> String ) -> [VoiceEvent] -> [VoiceEvent]
annFirstEvent ann append = snd . mapAccumL mapAccumF False
  where
    mapAccumF False (VeNote note)     = (True,VeNote   (note   & noteCtrls   %~ addOrAppendAnn ann append))
    mapAccumF False (VeRhythm rhythm) = (True,VeRhythm (rhythm & rhythmCtrls %~ addOrAppendAnn ann append))
    mapAccumF False (VeChord chord)   = (True,VeChord  (chord  & chordCtrls  %~ addOrAppendAnn ann append))
    mapAccumF False (VeRest rest)     = (True,VeRest   (rest   & restCtrls   %~ addOrAppendAnn ann append))
    mapAccumF False (VeSpacer spacer) = (True,VeSpacer (spacer & spacerAnn   %~ append ann))
    mapAccumF seen  event             = (seen,event)
    
addOrAppendAnn :: String -> (String -> String -> String) -> [Control] -> [Control]
addOrAppendAnn newAnn append ctrls = maybe (CtrlAnnotation newAnn : ctrls) (`appendAnnForIdx` ctrls) $ findIndex isAnnotation ctrls
  where
    appendAnnForIdx idx = toList . adjust' appendAnn idx . fromList
    appendAnn (CtrlAnnotation oldAnn) = CtrlAnnotation (append newAnn oldAnn)
    appendAnn ctrl = error $ "appendAnn unexpected control: " <> show ctrl
    isAnnotation (CtrlAnnotation _) = True
    isAnnotation _                  = False

rotN :: Int -> [a] -> [a]
rotN cnt as = drop cnt as <> take cnt as

----------------------------------------------------------------------------
-- VoiceConfig[XPose | Slice | Verbatim | Repeat | Blend] implementations --
----------------------------------------------------------------------------

genVerbatim :: TimeSignature -> String -> VoiceConfigCore -> Int -> Driver [VoiceEvent]
genVerbatim _ = genVoiceEventsByDur "verbatim" core2InfVerbatimMottos

genRepeat :: TimeSignature -> String -> VoiceConfigCore -> Int -> Driver [VoiceEvent]
genRepeat _ = genVoiceEventsByDur "repeat" core2InfRepeatMottos

genSlice :: TimeSignature -> String -> VoiceConfigCore -> Int -> Driver [VoiceEvent]
genSlice _ = genVoiceEventsByDur "cell" core2InfSliceMottos

genBlend :: TimeSignature -> String -> VoiceConfigCore -> Int -> Driver [VoiceEvent]
genBlend _ = genVoiceEventsByDur "blend" core2InfBlendMottos

genXPose :: TimeSignature -> String -> VoiceConfigCore -> Scale -> Range -> Driver [VoiceEvent]
genXPose _ = genVoiceEventsByRange "xpose" core2InfBlendMottoss

genAccrete :: TimeSignature -> String -> VoiceConfigCore -> Int -> (KeySignature,PitOct) -> Driver [VoiceEvent]
genAccrete = genVoiceEventsByAccrete "accrete" 

voiceConfig2VoiceEvents :: String -> TimeSignature -> VoiceConfig -> Driver [VoiceEvent]
voiceConfig2VoiceEvents path ts VoiceConfigVerbatim{..} = genVerbatim ts path _vcvCore _vcvDurVal
voiceConfig2VoiceEvents path ts VoiceConfigRepeat{..}   = genRepeat   ts path _vcrCore _vcrDurVal
voiceConfig2VoiceEvents path ts VoiceConfigSlice{..}    = genSlice    ts path _vcsCore _vcsDurVal
voiceConfig2VoiceEvents path ts VoiceConfigBlend{..}    = genBlend    ts path _vcbCore _vcbDurVal
voiceConfig2VoiceEvents path ts VoiceConfigXPose{..}    = genXPose    ts path _vcxCore _vcxScale _vcxRange
voiceConfig2VoiceEvents path ts VoiceConfigAccrete{..}  = genAccrete  ts path _vcaCore _vcaNumBars _vcaInit

--------------------------------------------------------------------
-- SectionConfig[Neutral | FadeIn | FadeOut | FadeAcross] helpers --
--------------------------------------------------------------------

ves2VeRests :: [VoiceEvent] -> [VoiceEvent]
ves2VeRests ves = [VeRest (Rest (mkDurationVal (ves2DurVal ves)) [])]

applyMConfigMods :: Maybe (NE.NonEmpty String) -> (VoiceRuntimeConfig, VoiceConfig) -> Driver (VoiceRuntimeConfig,VoiceConfig)
applyMConfigMods mNames pr = applyMMods mNames pr (applyMod name2VoiceConfigMods) <&> (fst pr,)

monoVoice2VEs :: Voice -> [VoiceEvent]
monoVoice2VEs (PitchedVoice    _ ves) = NE.toList ves
monoVoice2VEs (PercussionVoice _ ves) = NE.toList ves
monoVoice2VEs (SplitStaffVoice _ ves) = NE.toList ves
monoVoice2VEs voice                   = error $ "monoVoice2VEs voice is not mono type: " <> show voice

replaceMonoVoiceVEs :: Voice -> [VoiceEvent] -> Voice
replaceMonoVoiceVEs (PitchedVoice    instr _) ves = PitchedVoice    instr (NE.fromList ves)
replaceMonoVoiceVEs (PercussionVoice instr _) ves = PercussionVoice instr (NE.fromList ves)
replaceMonoVoiceVEs (SplitStaffVoice instr _) ves = SplitStaffVoice instr (NE.fromList ves)
replaceMonoVoiceVEs voice                     _   = error $ "replaceMonoVoiceVEs voice is not mono type: " <> show voice

voice2DurVal :: Voice -> Int
voice2DurVal = ves2DurVal . monoVoice2VEs

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

applyMods :: String -> TimeSignature -> Maybe (NE.NonEmpty String) -> Maybe (NE.NonEmpty String) -> (VoiceRuntimeConfig,VoiceConfig) -> Driver (Int,[VoiceEvent])
applyMods path timeSig mConfigMods mVoiceEventsMods pr =
  applyMConfigMods mConfigMods pr >>= voiceConfig2VoiceEvents path timeSig . snd >>= applyMVoiceEventsMods mVoiceEventsMods . (vrt,) <&> (_vrcNumVoc vrt,)
  where
    vrt = fst pr

addSecnName :: String -> [[VoiceEvent]] -> [[VoiceEvent]]
addSecnName scnName voiceEventss = prependAnnFirstNote scnName <$> voiceEventss

-------------------------------------
-- SectionConfigFadeAcross helpers --
-------------------------------------

-- Single vertical slice through individual lists of list of: Maybe PitOctOrNEPitOcts, DurValOrDurTuplet, and Accent,
-- used with VoiceConfigSlice where sublists are guaranteed to be of the same length.  A [Slice]
-- gives all data from three input vals in VoiceConfig but grouped vertically, for blending
-- between VoiceConfig A and VoiceConfig B where both are VoiceConfigSlice.
type Slice = ([Maybe PitOctOrNEPitOcts],[DurValOrDurTuplet],[Accent])

voiceConfig2Slice :: VoiceConfig -> [Slice]
voiceConfig2Slice VoiceConfigVerbatim{..} = config2Slices _vcvCore
voiceConfig2Slice VoiceConfigRepeat{..}   = config2Slices _vcrCore
voiceConfig2Slice VoiceConfigSlice{..}    = config2Slices _vcsCore
voiceConfig2Slice VoiceConfigBlend{..}    = config2Slices _vcbCore
voiceConfig2Slice VoiceConfigXPose{..}    = config2Slices _vcxCore
voiceConfig2Slice VoiceConfigAccrete{..}  = config2Slices _vcaCore

config2Slices :: VoiceConfigCore -> [Slice]
config2Slices VoiceConfigCore{..} =
  unfoldr unfoldToSlicesTup (nes2arrs _vcmPOOrPOss,nes2arrs _vcDurss,nes2arrs _vcAcctss)
  where
    unfoldToSlicesTup ([],[],[]) = Nothing
    unfoldToSlicesTup (as:ass,bs:bss,cs:css) = Just ((as,bs,cs),(ass,bss,css))
    unfoldToSlicesTup (as,bs,cs) = error $ "unfoldToSlicesTup uneven length lists: " <> show (length as,length bs, length cs) <> show (as,bs,cs)

cfgSlicessPr2Configs :: (VoiceConfig,[[Slice]]) -> [VoiceConfig]
cfgSlicessPr2Configs (voiceConfig,slicess) = tup2VoiceConfig voiceConfig <$> (slices2Tup <$> slicess)

type ConfigTup = (NE.NonEmpty (NE.NonEmpty (Maybe PitOctOrNEPitOcts))
                 ,NE.NonEmpty (NE.NonEmpty DurValOrDurTuplet)
                 ,NE.NonEmpty (NE.NonEmpty Accent))

slices2Tup :: [Slice] -> ConfigTup
slices2Tup slices =
  (NE.fromList $ fst3 <$> sliceTups, NE.fromList $ snd3 <$> sliceTups, NE.fromList $ thd3 <$> sliceTups)
  where
    sliceTups = slice2Tup <$> slices

slice2Tup :: Slice -> (NE.NonEmpty (Maybe PitOctOrNEPitOcts),NE.NonEmpty DurValOrDurTuplet,NE.NonEmpty Accent)
slice2Tup (mPitOctss,durs,accents) = (NE.fromList mPitOctss,NE.fromList durs,NE.fromList accents)

tup2VoiceConfig :: VoiceConfig -> ConfigTup -> VoiceConfig
tup2VoiceConfig (VoiceConfigVerbatim VoiceConfigCore{..} durVal) (mPitOrPits,durss,accents) =
  VoiceConfigVerbatim (VoiceConfigCore mPitOrPits durss accents _vcInstr _vcVoiceType) durVal
tup2VoiceConfig (VoiceConfigRepeat VoiceConfigCore{..} durVal) (mPitOrPits,durss,accents) =
  VoiceConfigRepeat (VoiceConfigCore mPitOrPits durss accents _vcInstr _vcVoiceType) durVal
tup2VoiceConfig (VoiceConfigSlice VoiceConfigCore{..} durVal) (mPitOrPits,durss,accents) =
  VoiceConfigSlice (VoiceConfigCore mPitOrPits durss accents _vcInstr _vcVoiceType) durVal
tup2VoiceConfig (VoiceConfigBlend VoiceConfigCore{..} durVal rotVal) (mPitOrPits,durss,accents) =
  VoiceConfigBlend (VoiceConfigCore mPitOrPits durss accents _vcInstr _vcVoiceType) durVal rotVal
tup2VoiceConfig (VoiceConfigXPose VoiceConfigCore{..} scale vcxRange') (mPitOrPits,durss,accents) =
  VoiceConfigXPose (VoiceConfigCore mPitOrPits durss accents _vcInstr _vcVoiceType) scale vcxRange'
tup2VoiceConfig (VoiceConfigAccrete VoiceConfigCore{..} scale vcxRange') (mPitOrPits,durss,accents) =
  VoiceConfigAccrete (VoiceConfigCore mPitOrPits durss accents _vcInstr _vcVoiceType) scale vcxRange'

-- TBD: always blends voices in order, e.g. for four voices 1, 2, 3, 4.  Randomize?
-- unfold instead of map because we don't just map over is, but rather use it to track progress of 
-- replacing config A slices with config B slices.
-- the end of the unfold is when the last element in the list reaches the count of config slices
unfoldToSlicesRow :: Int -> Int -> [([Slice],[Slice])] -> [Int] -> Maybe ([[Slice]],[Int])
unfoldToSlicesRow nReps cntCfgASlices slicePrs is
  | last is == cntCfgASlices = Nothing
  | otherwise = Just (slicesCol,incrBlendedIndices is)
    where
      slicesCol = blendSlices nReps <$> zip is slicePrs

blendSlices :: Int -> (Int,([Slice],[Slice])) -> [Slice]
blendSlices nReps (i,(cfgASlices,cfgBSlices)) = concat $ replicate nReps $ take i cfgBSlices <> drop i cfgASlices

-- $ incrBlendedIndices [0,0,0,0]
-- [1,0,0,0]
-- $ incrBlendedIndices (incrBlendedIndices [0,0,0,0])
-- [2,1,0,0]
-- $ incrBlendedIndices (incrBlendedIndices (incrBlendedIndices [0,0,0,0]))
-- [3,2,1,0]
-- $ incrBlendedIndices (incrBlendedIndices (incrBlendedIndices (incrBlendedIndices [0,0,0,0])))
-- [4,3,2,1]
incrBlendedIndices :: [Int] -> [Int]
incrBlendedIndices is = maybe (fmap succ is) (uncurry (<>) . first (fmap succ) . flip splitAt is . succ) $ elemIndex 0 is

---------------------------------------------------------------------
-- SectionConfig[Neutral | FadeIn | FadeOut | FadeAcross] handlers --
---------------------------------------------------------------------

path2Name :: String -> String
path2Name = last . splitOn "."

sectionConfig2VoiceEventss :: TimeSignature -> SectionConfig -> Driver [[VoiceEvent]]
sectionConfig2VoiceEventss timeSig (SectionConfigNeutral (SectionConfigCore scnPath mConfigMods mVoiceEventsMods) cntSegs voiceConfigs) = do
  spotIxs <- randomizeList [0..cntVocs - 1] <&> cycle
  traverse (traverse (applyMConfigMods mConfigMods)) (mkPrss spotIxs) >>= traverse (concatMapM cvtAndApplyMod) <&> addSecnName scnName
    where
      cntVocs = length voiceConfigs
      scnName = drop (length "section") (path2Name scnPath) <> " (neutral)"
      cvtAndApplyMod (rtTup,cfgTup) = voiceConfig2VoiceEvents scnPath timeSig cfgTup >>= applyMVoiceEventsMods mVoiceEventsMods . (rtTup,)
      mkPrss spotIxs = zipWith (\rtups cfgtup -> (,cfgtup) <$> rtups) segRuntimeTupss voiceConfigs
        where
          segRuntimeTupss = chunksOf cntSegs voiceRTConfigs
          voiceRTConfigs  = [VoiceRuntimeConfig scnPath Nothing (Just spotIx) cntVocs numVoc cntSegs numSeg |
                             numVoc <- [0..cntVocs - 1], (numSeg,spotIx) <- zip [0..cntSegs - 1] spotIxs]
                            
-- Fade in from rests, voice-by-voice, voice event mods fadeinAccs and fadeinDyns background continuing voices: start of a round.
-- The list of indexes in fadeIxs tells the index for the [VoiceEvent] to add, one-by-one.
-- Monadically fold over list if indexes with fade-out order from config, generating new [[VoiceEvent]] for all voices.
sectionConfig2VoiceEventss timeSig (SectionConfigFadeIn (SectionConfigCore scnPath mConfigMods mVoiceEventsMods) fadeIxs voiceConfigs) =
  foldM foldMf ([], idxVEsPrs) fadeIxs <&> addSecnName scnName . map snd . snd
  where
    cntVocs    = length voiceConfigs
    idxVEsPrs  = (,[]) <$> [0..cntVocs - 1]
    scnName    = drop (length "section") (path2Name scnPath) <> " (fade in)"
    foldMf (seenNumVocs,idxVEsPr) numVoc = do
      newVEs <- genVEsFromMods numVoc <&> snd
      let restVEs = ves2VeRests newVEs
      traverse (appendVEs newVEs restVEs) idxVEsPr <&> (numVoc:seenNumVocs,)
      where
        cntSegs = length fadeIxs
        numSeg = length seenNumVocs
        appendVEs newVEs' restVEs' (idxVoc,ves)
          | idxVoc == numVoc = pure (idxVoc,ves <> newVEs')
          | idxVoc `elem` seenNumVocs = genVEsFromMods idxVoc <&> second (ves <>)
          | otherwise = pure  (idxVoc,ves <> restVEs')
        genVEsFromMods idx = applyMods scnPath timeSig mConfigMods mVoiceEventsMods (rtTup,cfgTup)
          where
            mIdx = if idx == numVoc then Just idx else Nothing
            rtTup  = VoiceRuntimeConfig scnPath mIdx Nothing cntVocs idx cntSegs numSeg
            cfgTup = voiceConfigs !! idx
-- Fade out to rests, voice-by-voice, like end of a round.  
-- The list of indexes in fadeIxs tells the index for the [VoiceEvent] to subtract, one-by-one.
-- Monadically fold over list if indexes with fade-out order from config, generating new [[VoiceEvent]] for all unseen voices,
-- leaving the existing list as it is for all new and seen voices.
-- Then expand all voices to be equal to the duration of the longest voice.
sectionConfig2VoiceEventss timeSig (SectionConfigFadeOut (SectionConfigCore scnPath mConfigMods mVoiceEventsMods) fadeIxs voiceConfigs) = do
  vess <- foldM foldMf ([], idxVEsPrs) fadeIxs <&> addSecnName scnName . map snd . snd
  let veDurs = ves2DurVal <$> vess
  pure $ zipWith (mkVesTotDur timeSig (maximum veDurs)) veDurs vess
  where
    cntVocs   = length voiceConfigs
    idxVEsPrs = (,[]) <$> [0..cntVocs - 1]
    scnName   = drop (length "section") (path2Name scnPath) <> " (fade out)"
    foldMf (seenNumVocs,idxVEsPr) numVoc = do
      traverse appendVEs idxVEsPr <&> (numVoc:seenNumVocs,)
      where
        cntSegs = length fadeIxs
        numSeg = length seenNumVocs
        appendVEs (idxVoc,ves)
          | idxVoc == numVoc || idxVoc `elem` seenNumVocs = pure (idxVoc,ves)
          | otherwise = genVEsFromMods idxVoc <&> second (ves <>) 
        genVEsFromMods idx = applyMods scnPath timeSig mConfigMods mVoiceEventsMods (rtTup,cfgTup)
          where
            mIdx = if idx == numVoc then Just idx else Nothing
            rtTup  = VoiceRuntimeConfig scnPath mIdx Nothing cntVocs idx cntSegs numSeg
            cfgTup = voiceConfigs !! idx
-- Blend between two [VoiceConfig] by grouping pitches, durations into equal-length [Slice], then substituting slice-by-slice
-- from second [VoiceConfig] into first [VoiceConfig], starting with one voice from first [VoiceConfig] ending with all voices 
-- but one from second [VoiceConfig].
-- All VoiceConfig must be sliceable, meaning outer lists in list of list of pitches, durations, and accents have to be the 
-- same length.
-- As the actual selection of which inner list in the list of list of pitches, durations, and accents gets rendered is
-- randomized by the VoiceConfig, 
sectionConfig2VoiceEventss timeSig (SectionConfigFadeAcross (SectionConfigCore scnPath mConfigMods mVoiceEventsMods) nReps voiceConfigPrs) = do
  spotIxs <- traverse (const (randomIndex cntVocs)) [0..cntSegs - 1]
  traverse (traverse (applyMConfigMods mConfigMods)) (mkPrss spotIxs) >>= traverse (concatMapM cvtAndApplyMod) <&> addSecnName scnName
    where
      scnName         = drop (length "section") (path2Name scnPath) <> " (fade cells)"
      cntVocs         = length voiceConfigPrs
      slicePrs        = both voiceConfig2Slice <$> voiceConfigPrs
      cntCfgASlices   = length . fst . head $ slicePrs
      blendedSlices   = transpose $ unfoldr (unfoldToSlicesRow nReps cntCfgASlices slicePrs) (1:replicate (cntVocs - 1) 0)
      voiceConfigs    = fst <$> voiceConfigPrs
      voiceConfigss   = cfgSlicessPr2Configs <$> zip voiceConfigs blendedSlices
      cntSegs         = length (head voiceConfigss)
      cvtAndApplyMod (rtTup,cfgTup) = voiceConfig2VoiceEvents scnPath timeSig cfgTup >>= applyMVoiceEventsMods mVoiceEventsMods . (rtTup,)
      mkPrss spotIxs = zipWith zip segRuntimeTupss voiceConfigss
        where
          segRuntimeTupss = chunksOf cntSegs voiceRTConfigs
          voiceRTConfigs  = [VoiceRuntimeConfig scnPath Nothing (Just spotIx) cntVocs numVoc cntSegs numSeg |
                             numVoc <- [0..cntVocs - 1], (numSeg,spotIx) <- zip [0..cntSegs - 1] spotIxs]
-- Experimental starting point, deprecate?
sectionConfig2VoiceEventss _ (SectionConfigExp _ keySig mScale start numCycles motifss) = 
  pure [map nDOrNDTup2VE cycles]
  where
    scale = fromMaybe (keySig2Scale M.! keySig) mScale
    ndTupInArrs = map (map nDOrNDTup2Arrs) (nes2arrs motifss)
    ndTupOutArr = concat $ xposeFromNoteDurOrNoteDurTupss scale start ndTupInArrs
    cycles = take (numCycles * length ndTupOutArr) $ cycle ndTupOutArr
-- Experimental ostinato, issues:
-- - by chunking each motto into it's own [[]] (see zipWith), there's
--   no transposition that takes place through xposeFromNoteDurOrNoteDurTupss
--   so what you get is just verbatim repetition of ostinatos, rather than
--   ostinatos used as transpose patterns.
sectionConfig2VoiceEventss _ (SectionConfigExpOst  SectionConfigCore{..} keySig mScale numCycles motifNames startNames) = do
  motMap   <- traverse (secondM motName2Mot . dupe) motNames <&> M.fromList
  startMap <- traverse (secondM startName2Start . dupe) strtNames <&> M.fromList
  mNames   <- randomizeList $ take (length motNames * numCycles) (cycle motNames)
  stNames  <- randomizeList $ take (length motNames * numCycles) (cycle strtNames)
  let mots   = (M.!) motMap <$> mNames
      starts = (M.!) startMap <$> stNames
      prs    = zipWith (\s m -> (s,[m])) starts mots
      nDOrNDTupss  = concatMap (uncurry (xposeFromNoteDurOrNoteDurTupss scale)) prs
  pure [concatMap (map nDOrNDTup2VE) nDOrNDTupss]
  where
    scale = fromMaybe (keySig2Scale M.! keySig) mScale
    motNames = NE.toList motifNames
    strtNames = NE.toList startNames
    keyAndName2Val key name = getConfigParam (_sccPath <> "." <> key <> "." <> name)
    motName2Mot motName = keyAndName2Val "motifs" motName <&> map nDOrNDTup2Arrs . NE.toList
    startName2Start = keyAndName2Val "starts"
-- To be deprecated
sectionConfig2VoiceEventss _ _  = pure []
    
-------------------------------------------------------
-- GroupConfig[Neutral | EvenEnds | Ordered] helpers --
-------------------------------------------------------

secCfg2SecName :: SectionConfig -> String
secCfg2SecName SectionConfigNeutral{..}    = path2Name (_sccPath _scnCore)
secCfg2SecName SectionConfigFadeIn{..}     = path2Name (_sccPath _scfiCore)
secCfg2SecName SectionConfigFadeOut{..}    = path2Name (_sccPath _scfoCore)
secCfg2SecName SectionConfigFadeAcross{..} = path2Name (_sccPath _scfaCore)
secCfg2SecName SectionConfigExp{..}        = path2Name (_sccPath _sceCore)
secCfg2SecName SectionConfigExpOst{..}     = path2Name (_sccPath _sceoCore)
secCfg2SecName SectionConfigExpGuides{..}  = path2Name (_sccPath _scegCore)

-- Repeatedly add [VoiceEvent] for last section to Voice until all [Voice] are the same
-- total duration.  Tricky bit is that sectionConfig2Voices may not add [VoiceEvent] of
-- sufficiently long sum duration to match difference in length needed, in which case, loop.
-- To polish, trim final result to end at the bar line.
extendVoices :: TimeSignature  -> SectionConfig -> [Voice] -> Driver [Voice]
extendVoices timeSig sectionConfig voices =
  let go vess = do
        let veLens = ves2DurVal <$> vess
        if sum veLens == length veLens * head veLens
        then do
          pure $ zipWith replaceMonoVoiceVEs voices vess
        else do
          let maxLen = bump2FullBar timeSig $ maximum veLens
              veLenDiffs = (-) maxLen <$> veLens
          vessNew <- sectionConfig2VoiceEventss timeSig sectionConfig <&> zipWith maybeTrimVes veLenDiffs
          go $ zipWith (<>) vess vessNew
  in 
    go $ monoVoice2VEs <$> voices

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

isSpacer :: VoiceEvent -> Bool
isSpacer VeSpacer {} = True
isSpacer _           = False

---------
-- API --
---------

-----------------------
-- config2MonoVoices --
-----------------------

-- Strip duplicate time and key signatures, though it looks like Lilypond only renders time signatures.
-- Could be done in one pass with multiple tests per VoiceEvent but it's probably not worth the complexity.
concatMonoVoices :: [Voice] -> Voice
concatMonoVoices voices = replaceMonoVoiceVEs (head voices) ves
  where
    ves = filterDuplicateControls isVeKeySig $ filterDuplicateControls isVeTimeSig $ concatMap monoVoice2VEs voices

isVeKeySig :: VoiceEvent -> Bool
isVeKeySig (VeKeySignature _) = True
isVeKeySig _                  = False

isVeTimeSig :: VoiceEvent -> Bool
isVeTimeSig (VeTimeSignature _) = True
isVeTimeSig _                   = False

filterDuplicateControls :: (VoiceEvent -> Bool) -> [VoiceEvent] -> [VoiceEvent]
filterDuplicateControls veIsCtrl = reverse . snd . foldl' f (Nothing,[])
  where
    f (Nothing,ves)   ve
      |  veIsCtrl ve = (Just ve,ve:ves)
      |  otherwise   = (Nothing,ve:ves)
    f (Just ctrl,ves) ve
      | ve == ctrl   = (Just ctrl,ves)
      | veIsCtrl ve  = (Just ve,ve:ves)
      | otherwise    = (Just ctrl,ve:ves)

groupConfig2Voices :: TimeSignature -> KeySignature -> GroupConfig -> Driver [Voice]
groupConfig2Voices timeSig keySig (GroupConfigNeutral _ _ sectionConfigs) = 
  traverse (sectionConfig2Voices timeSig keySig) (NE.toList sectionConfigs) <&> map concatMonoVoices . transpose
groupConfig2Voices timeSig keySig (GroupConfigEvenEnds _ _ sectionConfigs) = 
  traverse (sectionConfig2Voices timeSig keySig) (NE.toList sectionConfigs) >>=
    extendVoices timeSig (NE.last sectionConfigs) . map concatMonoVoices . transpose
groupConfig2Voices timeSig keySig (GroupConfigOrdered _ _ orderedSectionNames sectionConfigs) = 
  traverse (sectionConfig2Voices timeSig keySig) orderedConfigs <&> map concatMonoVoices . transpose
  where
    name2ConfigMap = M.fromList ((secCfg2SecName &&& id) <$> NE.toList sectionConfigs)
    orderedConfigs = (M.!) name2ConfigMap <$> NE.toList orderedSectionNames
    
sectionConfig2Voices :: TimeSignature -> KeySignature -> SectionConfig -> Driver [Voice]
sectionConfig2Voices timeSig keySig sectionConfig@SectionConfigNeutral{..} =
  sectionConfig2VoiceEventss timeSig sectionConfig <&> zipWith (ves2Voice timeSig keySig) _scnVoices
sectionConfig2Voices timeSig  keySig sectionConfig@SectionConfigFadeIn{..} =
  sectionConfig2VoiceEventss timeSig sectionConfig <&> zipWith (ves2Voice timeSig keySig) _scfiVoices
sectionConfig2Voices timeSig keySig sectionConfig@SectionConfigFadeOut{..} = 
  sectionConfig2VoiceEventss timeSig sectionConfig <&> zipWith (ves2Voice timeSig keySig) _scfoVoices
sectionConfig2Voices timeSig keySig sectionConfig@SectionConfigFadeAcross{..} = 
  sectionConfig2VoiceEventss timeSig sectionConfig <&> zipWith (ves2Voice timeSig keySig) (fst <$> _scfaVoicesAB)
sectionConfig2Voices _       _       sectionConfig = error $ "sectionConfig2Voices no voices for sectionConfig " <> show sectionConfig

ves2Voice :: TimeSignature -> KeySignature -> VoiceConfig -> [VoiceEvent] -> Voice
ves2Voice timeSig keySig voiceConfig = voiceEvents2MonoVoice vType instr timeSig keySig
  where
    vType = voiceConfig2VoiceType voiceConfig
    instr = voiceConfig2Instrument voiceConfig

voiceConfig2VoiceConfigCore :: VoiceConfig -> VoiceConfigCore
voiceConfig2VoiceConfigCore VoiceConfigVerbatim{..} = _vcvCore
voiceConfig2VoiceConfigCore VoiceConfigRepeat{..}   = _vcrCore
voiceConfig2VoiceConfigCore VoiceConfigSlice{..}    = _vcsCore
voiceConfig2VoiceConfigCore VoiceConfigBlend{..}    = _vcbCore
voiceConfig2VoiceConfigCore VoiceConfigXPose{..}    = _vcxCore
voiceConfig2VoiceConfigCore VoiceConfigAccrete{..}  = _vcaCore

voiceConfig2VoiceType :: VoiceConfig -> String
voiceConfig2VoiceType = _vcVoiceType . voiceConfig2VoiceConfigCore

voiceConfig2Instrument :: VoiceConfig -> Instrument
voiceConfig2Instrument = _vcInstr . voiceConfig2VoiceConfigCore 

voiceEvents2MonoVoice :: String -> Instrument -> TimeSignature -> KeySignature -> [VoiceEvent] -> Voice
voiceEvents2MonoVoice "pitched"    i ts ks ves = genPitchedVoice i ts ks ves
voiceEvents2MonoVoice "percussion" i ts ks ves = genPercussionVoice i ts ks ves
voiceEvents2MonoVoice "splitstaff" i ts ks ves = genSplitStaffVoice i ts ks ves
voiceEvents2MonoVoice vType        _ _  _  _   = error $ "voiceEvents2MonoVoice unknown voice type " <> vType

voiceConfig2MonoVoice :: String -> TimeSignature -> KeySignature -> VoiceConfig -> Driver Voice
voiceConfig2MonoVoice path timeSig keySig voiceConfig =
  voiceConfig2VoiceEvents path timeSig voiceConfig <&> ves2Voice timeSig keySig voiceConfig

genPitchedVoice :: Instrument -> TimeSignature -> KeySignature -> [VoiceEvent] -> Voice
genPitchedVoice instr timeSig keySig ves = 
    PitchedVoice instr (VeKeySignature keySig NE.<| VeTimeSignature timeSig NE.<| NE.fromList ves)
    
genPercussionVoice :: Instrument -> TimeSignature -> KeySignature -> [VoiceEvent] -> Voice
genPercussionVoice instr timeSig keySig ves = 
    PercussionVoice instr (VeKeySignature keySig NE.<| VeTimeSignature timeSig NE.<| NE.fromList ves)
    
genSplitStaffVoice :: Instrument -> TimeSignature -> KeySignature -> [VoiceEvent] -> Voice
genSplitStaffVoice instr timeSig keySig ves = 
    SplitStaffVoice instr (VeKeySignature keySig NE.<| VeTimeSignature timeSig NE.<| NE.fromList ves)

title2Voices :: String -> TimeSignature -> KeySignature -> Driver [Voice]
title2Voices path timeSig keySig =
  cfgPath2Keys ("group" `isPrefixOf`) path <&> fmap ((path <> ".") <>) >>= grpNames2Voices timeSig keySig

group2Voices :: String -> TimeSignature -> KeySignature -> Driver [Voice]
group2Voices path timeSig keySig = grpNames2Voices timeSig keySig [path] 

grpNames2Voices :: TimeSignature -> KeySignature -> [String] -> Driver [Voice]
grpNames2Voices timeSig keySig grpNames = do
  grpScnsPrs <- traverse (secondM (cfgPath2Keys ("section" `isPrefixOf`)) . dupe) grpNames
  grpCfgs    <- traverse (uncurry cfg2GroupConfig) (second NE.fromList <$> grpScnsPrs)
  voicess    <- traverse (groupConfig2Voices timeSig keySig) grpCfgs
  pure $ concatMonoVoices <$> transpose voicess

section2Voices :: String -> TimeSignature -> KeySignature -> Driver [Voice]
section2Voices path timeSig keySig = path2SectionConfig path >>= sectionConfig2Voices timeSig keySig 

voice2Voices :: String -> TimeSignature -> KeySignature -> Driver [Voice]
voice2Voices path timeSig keySig = path2VoiceConfig path >>= voiceConfig2MonoVoice path timeSig keySig <&> (:[])

-- The tempo, time signature and key signature, are all from the top level config data.
config2MonoVoices :: String -> TimeSignature -> KeySignature -> Driver [Voice]
config2MonoVoices path timeSig keySig =
  case length (splitOn "." path) of
    1 -> title2Voices   path timeSig keySig
    2 -> group2Voices   path timeSig keySig
    3 -> section2Voices path timeSig keySig
    4 -> voice2Voices   path timeSig keySig
    n ->  error $ "config2MonoVoices unexpected count of keys " <> show n <> " in " <> path

---------------------------------
-- groupAndTrimDursAndAddTempo --
---------------------------------

-- Coordinate durations with time signature, bar divisions.
-- Add rests at the end of the voices so they end on the same bar.
-- Add a tempo to the top voice.
groupAndTrimDursAndAddTempo :: Tempo -> TimeSignature -> [Voice] -> [Voice]
groupAndTrimDursAndAddTempo tempo timeSig voices =
  fmap (alignVoiceDurations timeSig) voices
  & zipWith (mkVoiceTotDur timeSig (maximum voiceLens)) voiceLens
  & tagTempo tempo
  where
    voiceLens = voice2DurVal <$> voices
    
alignVoiceDurations :: TimeSignature -> Voice -> Voice
alignVoiceDurations timeSig voice =
  replaceMonoVoiceVEs voice . alignVoiceEventsDurations timeSig $ monoVoice2VEs voice

-- Apportion [VeRest Rest] or [VeNote Note] durations according to place in bar given
-- time signature, i.e. do barring explicitly instead of relying on LilyPond.
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
    -- contiguous rests can just use durations from addEndDurs, no need for ties
    adjVEsDurs :: Int -> [VoiceEvent] -> (Int,[VoiceEvent])
    adjVEsDurs curLen rests@((VeRest _):_) =
      (curLen + addLen,newRests)
      where
        addLen = ves2DurVal rests
        durs = addEndDurs timeSig curLen addLen
        newRests = VeRest . (`Rest` []) <$> durs
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
fixVoiceEventTie False _     ve@VeRest{}           = ve & veRest . restCtrls .~ [] {-- TBD: ,_restTie = not tie --}
fixVoiceEventTie _     _     ve                    = ve

swapVeLens :: Duration -> VoiceEvent -> VoiceEvent
swapVeLens dur ve@VeNote{}   = ve & veNote   . noteDur   .~ duration2DurationVal dur
swapVeLens dur ve@VeRest{}   = ve & veRest   . restDur   .~ duration2DurationVal dur
swapVeLens dur ve@VeChord{}  = ve & veChord  . chordDur  .~ duration2DurationVal dur
swapVeLens dur ve@VeRhythm{} = ve & veRhythm . rhythmDur .~ duration2DurationVal dur
swapVeLens dur ve@VeSpacer{} = ve & veSpacer . spacerDur .~ duration2DurationVal dur
swapVeLens dur VeTuplet{}    = VeRest $ Rest (duration2DurationVal dur) []
swapVeLens _   ve            = ve
    
mkVoiceTotDur :: TimeSignature -> Int -> Int -> Voice -> Voice
mkVoiceTotDur timeSig maxLen vesLen voice = replaceMonoVoiceVEs voice . mkVesTotDur timeSig maxLen vesLen $ monoVoice2VEs voice
  
-- Make the total length of all [VoiceEvent] in [[VoiceEvent]] called from
-- cfg2SplitStaffScore to be equal by filling out the rests at the end of
-- each [VoiceEvent] to match the length of the longest [VoiceEvent], choosing
-- spacer or rest depending on the context (left over from before split staff).
-- Also called from sectionConfig2VoiceEventss for SectionConfigFadeOut.
-- maxLen and vesLen are in 128th notes
-- maxLen is target length so all voices are equal length
-- vesLen is actual length maybe same as maxLen
mkVesTotDur :: TimeSignature -> Int -> Int -> [VoiceEvent] -> [VoiceEvent]
mkVesTotDur timeSig maxLen vesLen ves =
  ves <> (spacerOrRest <$> addEndDurs timeSig vesLen addLen)
  where
    beatLen = dur2DurVal (timeSig2Denom timeSig)
    barLen  = timeSig2Num timeSig * beatLen
    remBar  = if maxLen `rem` barLen == 0 then 0 else barLen - (maxLen `rem` barLen)
    addLen  = if maxLen > vesLen then (maxLen - vesLen) + remBar else remBar
    spacerOrRest = if not (null ves) && isSpacer (last ves) then VeSpacer . (\dur -> Spacer dur NoDynamic "") else VeRest . (`Rest` [])

