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
import Safe (headMay)

import Driver
       (Driver, cfg2Tups, randomElements, randomWeightedElement, searchConfigParam, searchMConfigParam)
import Types
import Utils

type NoteOrRest = Either Note Rest

newtype Range = Range ((Pitch,Octave),(Pitch,Octave)) deriving (Eq, Show)

-- What do I want to configure a homophonic voice?
-- What do I want to configure a homophonic section?
-- Is it enough to use SectionConfigNeutral with a bunch
--   of repeats and have a VoiceConfigRepeat configured
--   with explicit durations?
-- Most basic homophonic texture is just a pulse, with all
--   voices repeating the same duration, need that as a start.
-- Transition I've already played with is then to repeatedly
--   stagger voice sub-groups, for choirs in competition.
-- Might be interesting process to proceed in stages from 
--   polyphonic to homophonic, as voices line up, could be
--   a section-by-section transition similar to fade-out but
--   with a new config (or something) for the fade-out voices.
-- In this case, we'd have a new transition section FadeOver
--   with two sets of voices of of the same length or maybe
--   one list of VoiceConfig pairs.
-- And there'd be a new VoiceConfig for the homophonic
--   texture.  '
--
-- Could make a new section with a single list of voices and
-- a list of list of voice indexes where each list would have
-- same length and number of items in list would be number of
-- voices.  The meaning of an index would be a fixed instance
-- of durations list from that voice for this voice's
-- generator.  So e.g. for a list of three voices with three
-- sections, to fix all voices with repetitions of their individual
-- durations would look like this:
--   ((0,0,0)
--   ,(1,1,1)
--   ,(2,2,2))
-- But to have all voices follow the rhythms of the voices
-- in sequence:
--   ((0,1,2)
--   ,(0,1,2)
--   ,(0,1,2))
-- Or this could entirely be done by a config mod, where e.g.
-- there'd be one for fixing each of the three inputs, and a
-- unique name for the config.yml param, and for a sequence,
-- you'd use the neutral config.  The trick there would be 
-- to make sure the count of items in the list matched the
-- _sctnReps input.
-- 
-- Note issue: what if I wanted to manipulate different inputs
-- at the same time, e.g. one list for fixing durations, one for
-- fixing accents, one for fixing pitch/octave pairs?  And what
-- about pitch/octave pairs?  Note the VoiceConfig<foo> inputs
-- vary, though maybe XPose and Repeat could be brought in line
-- with Canon e.g. NE.NonEmpty (NE.NonEmpty (Maybe Pitch,Int))
-- instead of NE.NonEmpty (NE.NonEmpty (Maybe Int))?  Note these
-- are all list of list which is what I need to flatten consistently.
-- Also note the flattening technique wouuld be uniform as it would
-- be to replace a list of list where the inner list contains multiple
-- items, into a list of list where the inner list is a single item
-- only, which means this could all be a pre-processor to the standard
-- generation procedure, as the types for the config data don't change
-- from type to type.

-- TBD: put these in Config.hs?

data SectionConfig =
  SectionConfigNeutral {
                       _sctnPath        :: String
                      ,_sctnReps        :: Int
                      ,_sctnMName       :: Maybe String
                      ,_sctnMConfigMods :: Maybe (NE.NonEmpty String)
                      ,_sctnMNOrRsMods  :: Maybe (NE.NonEmpty String)
                      ,_sctnVoices      :: NE.NonEmpty VoiceConfig
                 }
  | SectionConfigFadeIn {
                       _sctfiPath        :: String
                      ,_sctfiOrder       :: NE.NonEmpty Int
                      ,_sctfiMName       :: Maybe String
                      ,_sctfiMConfigMods :: Maybe (NE.NonEmpty String)
                      ,_sctfiMNOrRsMods  :: Maybe (NE.NonEmpty String)
                      ,_sctfiVoices      :: NE.NonEmpty VoiceConfig
                      }
  | SectionConfigFadeOut {
                       _sctfoPath        :: String
                      ,_sctfoOrder       :: NE.NonEmpty Int
                      ,_sctfoMName       :: Maybe String
                      ,_sctfoMConfigMods :: Maybe (NE.NonEmpty String)
                      ,_sctfoMNOrRsMods  :: Maybe (NE.NonEmpty String)
                      ,_sctfoVoices      :: NE.NonEmpty VoiceConfig
                      }
data VoiceConfig =
  VoiceConfigXPose {
                 _vctxInstr      :: Instrument
                 ,_vctxKey       :: KeySignature
                 ,_vctxScale     :: Scale
                 ,_vctxTime      :: TimeSignature
                 ,_vctxMPitOctss :: NE.NonEmpty (NE.NonEmpty (Maybe Pitch,Int)) 
                 ,_vctxDurss     :: NE.NonEmpty (NE.NonEmpty Duration)
                 ,_vctxAcctss    :: NE.NonEmpty (NE.NonEmpty Accent)
                 ,_vctxRange     :: ((Pitch,Octave),(Pitch,Octave))
                 } 
  | VoiceConfigRepeat {
                    _vctrInstr      :: Instrument
                    ,_vctrKey       :: KeySignature
                    ,_vctrScale     :: Scale
                    ,_vctrTime      :: TimeSignature
                    ,_vctrMPitOctss :: NE.NonEmpty (NE.NonEmpty (Maybe Pitch,Int))
                    ,_vctrDurss     :: NE.NonEmpty (NE.NonEmpty Duration)
                    ,_vctrAcctss    :: NE.NonEmpty (NE.NonEmpty Accent)
                    ,_vctrRegister  :: (Pitch,Octave)
                    ,_vctrDurVal    :: Int
                 } 
  | VoiceConfigCanon {
                    _vctcInstr      :: Instrument
                    ,_vctcKey       :: KeySignature
                    ,_vctcScale     :: Scale
                    ,_vctcTime      :: TimeSignature
                    ,_vctcMPitOctss :: NE.NonEmpty (NE.NonEmpty (Maybe Pitch,Int))
                    ,_vctcDurss     :: NE.NonEmpty (NE.NonEmpty Duration)
                    ,_vctcAcctss    :: NE.NonEmpty (NE.NonEmpty Accent)
                    ,_vctcRegister  :: (Pitch,Octave)
                    ,_vctcDurVal    :: Int
                    ,_vctcRotVal    :: Int
                 }

tup2VoiceConfigXPose :: String -> Driver VoiceConfig
tup2VoiceConfigXPose pre =
      VoiceConfigXPose 
        <$> searchConfigParam  (pre <> ".instr")
        <*> searchConfigParam  (pre <> ".key")
        <*> searchConfigParam  (pre <> ".scale")
        <*> searchConfigParam  (pre <> ".time")
        <*> searchConfigParam  (pre <> ".mpitOctss")
        <*> searchConfigParam  (pre <> ".durss")
        <*> searchConfigParam  (pre <> ".accentss")
        <*> searchConfigParam  (pre <> ".range")

tup2VoiceConfigRepeat :: String -> Driver VoiceConfig
tup2VoiceConfigRepeat pre =
      VoiceConfigRepeat 
        <$> searchConfigParam  (pre <> ".instr")
        <*> searchConfigParam  (pre <> ".key")
        <*> searchConfigParam  (pre <> ".scale")
        <*> searchConfigParam  (pre <> ".time")
        <*> searchConfigParam  (pre <> ".mpitOctss")
        <*> searchConfigParam  (pre <> ".durss")
        <*> searchConfigParam  (pre <> ".accentss")
        <*> searchConfigParam  (pre <> ".register")
        <*> searchConfigParam  (pre <> ".durval")

tup2VoiceConfigCanon :: String -> Driver VoiceConfig
tup2VoiceConfigCanon pre =
      VoiceConfigCanon 
        <$> searchConfigParam  (pre <> ".instr")
        <*> searchConfigParam  (pre <> ".key")
        <*> searchConfigParam  (pre <> ".scale")
        <*> searchConfigParam  (pre <> ".time")
        <*> searchConfigParam  (pre <> ".mpitOctss")
        <*> searchConfigParam  (pre <> ".durss")
        <*> searchConfigParam  (pre <> ".accentss")
        <*> searchConfigParam  (pre <> ".register")
        <*> searchConfigParam  (pre <> ".durval")
        <*> searchConfigParam  (pre <> ".rotval")

tup2SectionConfigNeutral :: String -> NE.NonEmpty String -> Driver SectionConfig
tup2SectionConfigNeutral section voices =
      SectionConfigNeutral section
      <$> searchConfigParam (section <> ".reps")
      <*> searchMConfigParam (section <> ".sctname")
      <*> searchMConfigParam (section <> ".cfgmods")
      <*> searchMConfigParam (section <> ".norrsmods")
      <*> cfg2Tups cfg2VoiceConfig section voices
tup2SectionConfigFadeIn :: String -> NE.NonEmpty String -> Driver SectionConfig
tup2SectionConfigFadeIn section voices =
      SectionConfigFadeIn section
      <$> searchConfigParam  (section <> ".delays")
      <*> searchMConfigParam (section <> ".sctname")
      <*> searchMConfigParam (section <> ".cfgmods")
      <*> searchMConfigParam (section <> ".norrsmods")
      <*> cfg2Tups cfg2VoiceConfig section voices
tup2SectionConfigFadeOut :: String -> NE.NonEmpty String -> Driver SectionConfig
tup2SectionConfigFadeOut section voices =
      SectionConfigFadeOut section
      <$> searchConfigParam  (section <> ".drops")
      <*> searchMConfigParam (section <> ".sctname")
      <*> searchMConfigParam (section <> ".cfgmods")
      <*> searchMConfigParam (section <> ".norrsmods")
      <*> cfg2Tups cfg2VoiceConfig section voices

name2SectionConfig :: M.Map String (String -> NE.NonEmpty String -> Driver SectionConfig)
name2SectionConfig = M.fromList [("neutral",tup2SectionConfigNeutral)
                                   ,("fadein" ,tup2SectionConfigFadeIn)
                                   ,("fadeout",tup2SectionConfigFadeOut)]

cfg2SectionConfig :: String -> NE.NonEmpty String -> Driver SectionConfig
cfg2SectionConfig section voices =
  searchConfigParam (section <> ".stype") >>= runConfigType
  where
    runConfigType cfgType =
      case M.lookup cfgType name2SectionConfig of
        Nothing -> error $ "cfg2SectionConfig no converter for \"stype:\" " <> cfgType
        Just tup2SectionConfig -> tup2SectionConfig section voices

name2VoiceConfig :: M.Map String (String -> Driver VoiceConfig)
name2VoiceConfig = M.fromList [("xpose" ,tup2VoiceConfigXPose)
                               ,("repeat",tup2VoiceConfigRepeat)
                               ,("canon" ,tup2VoiceConfigCanon)]

cfg2VoiceConfig :: String -> Driver VoiceConfig
cfg2VoiceConfig section =
  searchConfigParam (section <> ".vtype") >>= runConfigType
  where
    runConfigType cfgType =
      case M.lookup cfgType name2VoiceConfig of
        Nothing -> error $ "cfg2VoiceConfig no converter for \"vtype:\" " <> cfgType
        Just tup2VoiceConfig -> tup2VoiceConfig section
        
data VoiceRuntimeTup = VoiceRuntimeTup {
                                  _vrtSctnPath :: String
                                 ,_vrtMNumVoc  :: Maybe Int
                                 ,_vrtCntVocs  :: Int
                                 ,_vrtNumVoc   :: Int
                                 ,_vrtCntSegs  :: Int
                                 ,_vrtNumSeg   :: Int
                                 } deriving Show

type ConfigMod = VoiceRuntimeTup -> VoiceConfig -> Driver VoiceConfig

name2VoiceConfigMods :: M.Map String ConfigMod
name2VoiceConfigMods = M.fromList [("incrRandOcts",incrRandomizeMPitOctssOctaves)
                                 ,("decrRandOcts",decrNormalizeMPitOctssOctaves)]
                      
type NOrRsMod = VoiceRuntimeTup -> [NoteOrRest] -> Driver [NoteOrRest]

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
incrRandomizeMPitOctssOctaves = incrMPitOctssOctaves mkIdWeightsIncr

-- complement incrNormalizeMPitOctssOctaves should start with maximal randomness and reduce incrementally
decrNormalizeMPitOctssOctaves :: ConfigMod
decrNormalizeMPitOctssOctaves = incrMPitOctssOctaves mkIdWeightsDecr 

incrMPitOctssOctaves :: (Int -> Int -> Int ) -> ConfigMod
incrMPitOctssOctaves mkIdWeight VoiceRuntimeTup{..} vct@VoiceConfigCanon{..} = do
  vctcMPitOctss' <- traverse randomizeMPitOctsOctaves _vctcMPitOctss
  pure vct { _vctcMPitOctss = vctcMPitOctss' }
  where
    idWeight  = mkIdWeight _vrtCntSegs _vrtNumSeg
    modWeight = (100 - idWeight) `div` 2
    weights   = [(modWeight,pred),(idWeight,id),(modWeight,succ)]
    randomizeMPitOctsOctaves mPitInts = traverse randomizeMPitOctsOctave mPitInts
    randomizeMPitOctsOctave (Just pit,oct) = randomWeightedElement weights <&> (\weight -> (Just pit,weight oct))
    randomizeMPitOctsOctave pr = pure pr
incrMPitOctssOctaves _ _ vct = pure vct

mkIdWeightsIncr :: Int -> Int -> Int
mkIdWeightsIncr       1      _ = 50
mkIdWeightsIncr cntSegs numSeg = 100 - (numSeg * (50 `div` (cntSegs - 1))) -- TBD: magic constant 50% of results to be modified at end

mkIdWeightsDecr :: Int -> Int -> Int
mkIdWeightsDecr       1      _ = 0
mkIdWeightsDecr cntSegs numSeg = 100 - ((cntSegs - (1 + numSeg)) * (50 `div` (cntSegs - 1))) -- TBD: magic constant 50% of results to be modified at end)

fadeInAccents :: NOrRsMod
fadeInAccents VoiceRuntimeTup{..} nOrRs = do
  acc1 <- searchMConfigParam (_vrtSctnPath <> ".fadeInAcc1") <&> fromMaybe Staccato
  acc2 <- searchMConfigParam (_vrtSctnPath <> ".fadeInAcc2") <&> fromMaybe Staccatissimo
  traverse (fadeInAccent _vrtMNumVoc acc1 acc2) nOrRs
  where
    fadeInAccent :: Maybe Int -> Accent -> Accent -> NoteOrRest -> Driver NoteOrRest
    fadeInAccent (Just _) acc1 _   (Left note@Note{..}) = pure $ Left (note { _noteAccs = acc1 NE.<| _noteAccs })
    fadeInAccent Nothing  _   acc2 (Left note@Note{..}) = pure $ Left (note { _noteAccs = acc2 NE.<| _noteAccs })
    fadeInAccent _        _   _    (Right rest)         = pure $ Right rest

fadeInDynamics :: NOrRsMod
fadeInDynamics VoiceRuntimeTup{..} nOrRs = do
  dyn1 <- searchMConfigParam (_vrtSctnPath <> ".fadeInDyn1") <&> fromMaybe Forte
  dyn2 <- searchMConfigParam (_vrtSctnPath <> ".fadeInDyn2") <&> fromMaybe PPP
  pure $ maybe (tagFirstNoteDynamic dyn2 nOrRs) (const $ tagFirstNoteDynamic dyn1 nOrRs) _vrtMNumVoc

uniformDynamics :: NOrRsMod
uniformDynamics VoiceRuntimeTup{..} nOrRs = 
  searchMConfigParam (_vrtSctnPath <> ".uniformDyn") <&> fromMaybe PPP <&> flip tagFirstNoteDynamic nOrRs

tagFirstNoteDynamic :: Dynamic -> [NoteOrRest] -> [NoteOrRest]
tagFirstNoteDynamic dyn nOrRs = maybe nOrRs (`tagDynForIdx` nOrRs) (findIndex isLeft nOrRs)
  where
    tagDynForIdx idx = toList . adjust (tagDyn dyn) idx . fromList
    tagDyn dyn' (Left note) = Left $ note { _noteDyn = dyn' }
    tagDyn _    (Right oops) = error $ "tagDyn unexpected rest: " <> show oops
    
sectionDynamics :: NOrRsMod
sectionDynamics VoiceRuntimeTup{..} nOrRs = 
  searchConfigParam (_vrtSctnPath <> ".sectionDyns") <&> flip tagFirstNoteDynamic nOrRs . (NE.!! _vrtNumSeg)

fadeOutDynamics :: NOrRsMod
fadeOutDynamics _ = pure 

uniformAccents :: NOrRsMod
uniformAccents VoiceRuntimeTup{..} nOrRs = do
  acc <- searchMConfigParam (_vrtSctnPath <> ".uniformAccent") <&> fromMaybe Staccatissimo
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

-- static means each [Maybe Int] is interpreted with respect to (Pitch,Octave)
-- instead of continuing to transpose from the end of one [Maybe Int] to the next
genStatic :: [[Duration]] -> [[Accent]] -> [[(Maybe Pitch,Int)]] -> Scale -> (Pitch,Octave) -> Int -> Driver [NoteOrRest]
genStatic durss acctss mPitOctss scale register maxDurVal = 
  genCanon durss acctss mPitOctss scale register maxDurVal 0 <&> replaceAnnFirstNote "static"

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
  genXPose (nes2arrs _vctxDurss) (nes2arrs _vctxAcctss) (nes2arrs _vctxMPitOctss) _vctxScale (Range _vctxRange)
voiceConfig2NoteOrRests VoiceConfigRepeat{..} =
  genStatic (nes2arrs _vctrDurss) (nes2arrs _vctrAcctss) (nes2arrs _vctrMPitOctss) _vctrScale _vctrRegister _vctrDurVal
voiceConfig2NoteOrRests VoiceConfigCanon{..} =
  genCanon (nes2arrs _vctcDurss) (nes2arrs _vctcAcctss) (nes2arrs _vctcMPitOctss)_vctcScale _vctcRegister _vctcDurVal _vctcRotVal

voiceConfig2Rests :: VoiceConfig -> Driver [NoteOrRest]
voiceConfig2Rests voiceConfig = voiceConfig2NoteOrRests voiceConfig <&> notes2Rests

notes2Rests :: [NoteOrRest] -> [NoteOrRest]
notes2Rests = fmap (either note2Rest Right)
  where
    note2Rest Note{..} = Right (Rest _noteDur NoDynamic)

applyMConfigMods :: Maybe (NE.NonEmpty String) -> (VoiceRuntimeTup, VoiceConfig) -> Driver (VoiceRuntimeTup,VoiceConfig)
applyMConfigMods mNames pr = applyMMods mNames pr (applyMod name2VoiceConfigMods) <&> (fst pr,)

applyMNOrRsMods :: Maybe (NE.NonEmpty String) -> (VoiceRuntimeTup,[NoteOrRest]) -> Driver [NoteOrRest]
applyMNOrRsMods mNames pr = applyMMods mNames pr (applyMod name2VoiceNOrRsMods)

applyMMods :: forall a. Maybe (NE.NonEmpty String) -> (VoiceRuntimeTup,a) -> (String -> (VoiceRuntimeTup,a) -> Driver a) -> Driver a
applyMMods Nothing (_,as) _ = pure as
applyMMods (Just modNames) pr applyAsMods = foldM foldMf pr (NE.toList modNames) <&> snd
  where
    foldMf :: (VoiceRuntimeTup,a) -> String -> Driver (VoiceRuntimeTup,a)
    foldMf pr' modName = applyAsMods modName pr' <&> (fst pr',)

applyMod :: M.Map String (VoiceRuntimeTup -> a -> Driver a) -> String -> (VoiceRuntimeTup,a) -> Driver a
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
    segRuntimeTupss = chunksOf cntSegs [VoiceRuntimeTup scnPath Nothing cntVocs numVoc cntSegs numSeg | numVoc <- [0..cntVocs - 1], numSeg <- [0..cntSegs - 1]]
    prs = zipWith (\rtups cfgtup -> (,cfgtup) <$> rtups) segRuntimeTupss (NE.toList voiceConfigs)
sectionConfig2NoteOrRests (SectionConfigFadeIn scnPath fadeIxs mSctnName mConfigMods mNOrRsMods voiceConfigs) =
  foldM foldMf ([],replicate cntVocs []) fadeIxs <&> addSecnName scnName . snd
  where
    scnName = fromMaybe "fade in" mSctnName
    cntSegs = length fadeIxs
    cntVocs = length voiceConfigs
    mkVocRTT nSeg nVoc =  VoiceRuntimeTup scnPath Nothing cntVocs nVoc cntSegs nSeg
    foldMf (seenNumVocs,prevNOrRss) numVoc = do
      let numSeg = length seenNumVocs
          vocCfgTup = voiceConfigs NE.!! numVoc
          vocRTTup  = VoiceRuntimeTup scnPath (Just numVoc) cntVocs numVoc cntSegs numSeg
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
    timeSigs = NE.toList $ _vctcTime <$> voiceConfigs
    mkVocRTT nSeg nVoc =  VoiceRuntimeTup scnPath Nothing cntVocs nVoc cntSegs nSeg
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

applyMods :: Maybe (NE.NonEmpty String) -> Maybe (NE.NonEmpty String) -> (VoiceRuntimeTup,VoiceConfig) -> Driver (Int,[NoteOrRest])
applyMods mConfigMods mNOrRsMods pr =
  applyMConfigMods mConfigMods pr >>= voiceConfig2NoteOrRests . snd >>= applyMNOrRsMods mNOrRsMods . (vrt,) <&> (numVoc,)
  where
    vrt = fst pr
    numVoc = _vrtNumVoc vrt

addSecnName :: String -> [[NoteOrRest]] -> [[NoteOrRest]]
addSecnName scnName voices = prependAnnFirstNote scnName <$> voices

genPolyVocs :: Instrument -> KeySignature -> TimeSignature -> (NE.NonEmpty VoiceEvent,NE.NonEmpty VoiceEvent) -> Voice
genPolyVocs instr keySig timeSig (treble,bass) = PolyVoice instr vess
  where
    vess = NE.fromList [veKeySig NE.<| veTimeSig NE.<| treble, veKeySig NE.<| veTimeSig NE.<| bass]
    veKeySig = VeKeySignature keySig
    veTimeSig = VeTimeSignature timeSig

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
mkVesPrTotDur :: Int -> Int -> TimeSignature -> ([VoiceEvent],[VoiceEvent]) -> ([VoiceEvent],[VoiceEvent])
mkVesPrTotDur maxLen vesLen timeSig =
  bimap addLenToVes addLenToVes
  where
    beatLen = dur2DurVal (timeSig2Denom timeSig)
    barLen  = timeSig2Num timeSig * beatLen
    remBar  = if maxLen `rem` barLen == 0 then 0 else barLen - (maxLen `rem` barLen)
    addLen  = if maxLen > vesLen then (maxLen - vesLen) + remBar else remBar
    addLenToVes ves = ves <> (spacerOrRest <$> addEndDurs timeSig vesLen addLen)
      where
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

tagTempo :: Tempo -> NE.NonEmpty Voice -> NE.NonEmpty Voice
tagTempo tempo (v1 NE.:| rest) = tagVoice v1 NE.:| rest
  where
    tagVoice ::  Voice -> Voice
    tagVoice PitchedVoice{..} = PitchedVoice _ptvInstrument (VeTempo tempo NE.<| _ptvVoiceEvents)
    tagVoice PercussionVoice{..} = PercussionVoice _pcvInstrument (VeTempo tempo NE.<| _pcvVoiceEvents)
    tagVoice (PolyVoice instr (ves NE.:| vess)) = PolyVoice instr ((VeTempo tempo NE.<| ves) NE.:| vess)
    tagVoice (VoiceGroup (v1' NE.:| r)) = VoiceGroup (tagVoice v1' NE.:| r)

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
alignNoteOrRestsDurations :: TimeSignature -> [NoteOrRest] -> [NoteOrRest]
alignNoteOrRestsDurations timeSig =
  snd . foldl' foldlf (0,[]) . groupBy ((==) `on` isRight)
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

-- for given debounce length, split [NoteOrRest] into ([VoiceEvent],[VoiceEvent]) to be rendered as piano staff
-- *where* debounce length tells how many contiguous Treble or Bass notes determine a change in staff,
-- *allowing for* [Right Rest] instances to ride along carrying current clef
-- note: for inactive clef, need to generate spacer events to maintain synchronization between Treble and Bass
-- 5 for winLen to wait for row of 5 Note with same clef to determine treble vs. bass, 1 for winLen for squashed notes.
splitNoteOrRests :: Int -> [NoteOrRest] -> ([VoiceEvent],[VoiceEvent])
splitNoteOrRests winLen =
  -- 1) chunk [NoteOrRest] into [[NoteOrRest]] where each inner [] is [Left Note] or [Right Rest]
  -- 2) fold over [[NoteOrRest]] pending [Right Rest] until next [Left Note] determines Clef
  -- 3) flush any pending [Right Rest] at end
  flush . foldl' foldlf ((Nothing,[]),([VeClef Treble],[VeClef Bass])) . groupBy equalClefs
  where
    flush :: ((Maybe Clef,[NoteOrRest]),([VoiceEvent],[VoiceEvent])) -> ([VoiceEvent],[VoiceEvent])
    flush ((Just _,[]),vesPr) = vesPr
    flush ((Nothing,pending),vesPr) -- never hit winLen Left Note in a row, try again decrementing winLen by one
      | winLen == 0 = error $ "splitNoteOrRests flush but no clef for pending " <> show pending
      | otherwise   = vesPr <> splitNoteOrRests (winLen - 1) pending
    flush ((Just clef,pending),vesPr) = vesPr <> genVesPr clef pending
    foldlf :: ((Maybe Clef,[NoteOrRest]),([VoiceEvent],[VoiceEvent])) -> [NoteOrRest] -> ((Maybe Clef,[NoteOrRest]),([VoiceEvent],[VoiceEvent]))
    foldlf ((mCl,pending),vesPr) nOrRs
      | allRests || cntNotes nOrRs < winLen = ((mCl,pending <> nOrRs),vesPr)
      | otherwise = case mCl of
                      Nothing -> ((Just nextCl,[]),vesPr <> genVesPr nextCl (pending <> nOrRs))
                      Just cl -> ((Just nextCl,[]),vesPr <> genVesPr cl pending <> genVesPr nextCl nOrRs)
      where
        allRests = all isRest nOrRs
        isRest (Right _) = True
        isRest (Left _)  = False
        cntNotes = length . filter (not . isTiedNote)
        isTiedNote (Left Note {..}) = _noteTie
        isTiedNote (Right r)        = error $ "isTiedNote unexpected rest: " <> show r
        nextCl  = case headMay (filter (not . isRest) nOrRs) of
                    Just (Left note) -> pickNoteClef note
                    Just (Right _)   -> error $ "splitNoteOrRests unexpected Rest in nOrRs " <> show nOrRs
                    Nothing          -> error "splitNoteOrRests unexpected empty list for nOrRs"
    genVesPr :: Clef -> [NoteOrRest] -> ([VoiceEvent],[VoiceEvent])
    genVesPr cl pending
      | cl == Treble = (either VeNote VeRest <$> pending,either note2Spacer rest2Spacer <$> pending)
      | otherwise    = (either note2Spacer rest2Spacer <$> pending,either VeNote VeRest <$> pending)
      where
        note2Spacer Note{..} = VeSpacer (Spacer _noteDur NoDynamic)
        rest2Spacer Rest{..} = VeSpacer (Spacer _restDur NoDynamic)
    equalClefs (Left n1) (Left n2) = pickNoteClef n1 == pickNoteClef n2
    equalClefs (Right _) (Right _) = True
    equalClefs _          _        = False
    pickNoteClef :: Note -> Clef
    pickNoteClef Note{..}
      | (_noteOct,_notePit) <= (COct,E) = Bass
      | otherwise = Treble


{-- Graveyard I:

-- Hard to predict because of indeterminite length of repetitions of voice config tups.
-- Given voice index sequence 0,3,1,2 need:
-- a) voice 0 to generate [NoteOrRest] from its VoiceTup,
--    voices 1,2,3 to convert same to [Right Rest],
--    voices 0,1,2,3 all end at same point
-- b) voice 3 to generate [NoteOrRest] from its VoiceTup,
--    voices 1,2 to convert to [RightRest]
--    voice 0 to generate new [NoteOrRest] from its VoiceTup
-- c) voice 1 to generate [NoteOrTest] from its VoiceTup
--    voice 2 to convert to [RightRest]
--    voices 0,3 to generate new [VoiceOrRest] from VoiceTups
-- d) voice 2 to generate [NoteOrRest] from its VoiceTup
--    voices 0,1,3 to generate new [VoiceOrRest] from VoiceTup
--

sectionConfig2NoteOrRests (SectionConfigFadeIn fadeIdxs voiceConfigs) =
  traverse (concatMapM (either voiceConfig2Rests voiceConfig2NoteOrRests)) eVoiceConfigss
  where
    eVoiceConfigss = genInOrder (NE.toList fadeIdxs) (NE.toList voiceConfigs)
sectionConfig2NoteOrRests (SectionConfigFadeOut fadeIdxs voiceConfigs) =
  traverse (concatMapM (either voiceConfig2Rests voiceConfig2NoteOrRests)) eVoiceConfigss
  where
    eVoiceConfigss = genOutOrder (NE.toList fadeIdxs) (NE.toList voiceConfigs)

genInOrder :: Ord a => [a] -> [b] -> [[Either b b]]
genInOrder = genOrder Left Right

genOutOrder :: Ord a => [a] -> [b] -> [[Either b b]]
genOutOrder = genOrder Right Left

-- TBD: this is ugly!
genOrder :: Ord a1 => (t -> a2) -> (t -> a2) -> [a1] -> [t] -> [[a2]]
genOrder mkA mkB is as = snd $ foldl' foldlf (initMap,initRet) is
  where
    initMap = M.fromList $ (,mkA) <$> is
    initRet = replicate (length is) []
    foldlf (m,rets) i = (m',zipWith app rets r)
      where
        m' = M.insert i mkB m
        r  = zipWith (\(_,b) a -> b a) (M.toAscList m') as
        app xs x = xs <> [x]
--}


{--
Graveyard II:

From Compose.hs:

      -- ghost voices
      -- manyIntPrss = cycle <$> nes2arrs (_stGhosts <$> tups)
      -- gWinLens    = replicate cntVoices 1 -- tbd: magic constant
      -- gVoices     = zipWith3 squashNoteOrRests manyIntPrss timeSigs noteOrRestss
      --               &  pipeline tempo s1tups gWinLens

From Utils.hs:

genLNotes :: Int -> Int -> TimeSignature -> Note -> [Either Note Rest]
genLNotes addLen curLen timeSig note =
  (dur2LeftNote True <$> init durs) <> [dur2LeftNote False (last durs)]
  where
    durs = addEndDurs timeSig curLen addLen
    dur2LeftNote tie dur = Left (note { _noteDur = dur, _noteTie = tie })

genRRests :: Int -> Int -> TimeSignature -> [Either Note Rest]
genRRests addLen curLen timeSig =
  dur2RightRest <$> durs
  where
    durs = addEndDurs timeSig curLen addLen
    dur2RightRest = Right . flip Rest NoDynamic
-- using counts in pair (#rests, #notes) combine #rests of NoteOrRest together into one rest
-- followed by #notes of notes of NoteOrRest together into one note *if* the first NoteOrRest
-- is a (Left Note), else accumulate duration of #notes and duration of #rests with duration
-- of previously accumulated rests and continue
squashNoteOrRests :: [(Int,Int)] -> TimeSignature -> [NoteOrRest] -> [NoteOrRest]
squashNoteOrRests prs timeSig =
  flush . foldl foldlf ((0,0),[]) . chunkByPairCounts prs . fmap (stripNoteOrRest False)
  where
    flush ((_,0),nOrRs)                       = nOrRs
    flush ((curDurVal,prevRestsDurVal),nOrRs) = nOrRs <> genRRests prevRestsDurVal curDurVal timeSig
    foldlf ((curDurVal,prevRestsDurVal),ret) (rests,notes) =
      maybe restsRet notesRet $ mFirstNote notes
      where
        restsRet      = ((curDurVal
                         ,prevRestsDurVal
                          + restsDurVal
                          + notesDurVal)
                        ,ret)
        notesRet note = ((curDurVal
                          + prevRestsDurVal
                          + restsDurVal
                          + notesDurVal
                         ,0),
                         ret
                         <> genRRests (prevRestsDurVal + restsDurVal) curDurVal timeSig
                         <> genLNotes notesDurVal (curDurVal + prevRestsDurVal + restsDurVal) timeSig note)
        restsDurVal   = nOrRs2DurVal rests
        notesDurVal   = nOrRs2DurVal notes
    mFirstNote (Left note:_) = Just note
    mFirstNote _             = Nothing
--}

