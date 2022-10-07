{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}

module ConfigData where

import Driver (Driver, searchConfigParam, searchMConfigParam, cfgPath2Keys)
import Types
import Utils 

import Control.Lens hiding (pre)
import Data.List (isPrefixOf)
import Data.List.Extra (allSame)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

data GroupConfig =
  GroupConfigNeutral {
  _gcnPath     :: String
  ,_gcnMName   :: Maybe String
  ,_gcnConfigs :: NE.NonEmpty SectionConfig
  }
  | GroupConfigEvenEnds {
      _gcePath    :: String
      ,_gceMName   :: Maybe String
      ,_gceConfigs :: NE.NonEmpty SectionConfig
      }
  | GroupConfigOrdered {
      _gcoPath    :: String
      ,_gcoMName   :: Maybe String
      ,_gcoSNames  :: NE.NonEmpty String -- order of sections by name
      ,_gcoConfigs :: NE.NonEmpty SectionConfig
      }
  deriving Show

data SectionConfigCore =
  SectionConfigCore {
    _sccPath         :: String
    ,_sccMConfigMods :: Maybe (NE.NonEmpty String)
    ,_sccMVesMods     :: Maybe (NE.NonEmpty String)
    } deriving Show

data SectionConfig =
  SectionConfigNeutral {
  _scnCore    :: SectionConfigCore
  ,_scnReps   :: Int
  ,_scnVoices :: [VoiceConfig]
  }
  | SectionConfigFadeIn {
      _scfiCore    :: SectionConfigCore
      ,_scfiOrder  :: NE.NonEmpty Int
      ,_scfiVoices :: [VoiceConfig]
      }
  | SectionConfigFadeOut {
      _scfoCore    :: SectionConfigCore
      ,_scfoOrder  :: NE.NonEmpty Int
      ,_scfoVoices :: [VoiceConfig]
      }
  | SectionConfigFadeAcross {
      _scfcCore      :: SectionConfigCore
      ,_scfcReps     :: Int
      -- (a,b) pairs for two equal-length consorts
      -- both must be sliceable, though they can be
      -- different lengths
      ,_scfcVoicesAB :: [(VoiceConfig,VoiceConfig)]
      }
  | SectionConfigExp {
      _sceCore       :: SectionConfigCore
      ,_sceKeySig    :: KeySignature
      ,_sceMScale    :: Maybe Scale
      ,_sceInit      :: PitOct
      ,_sceNumcycles :: Int
      ,_sceMotifs    :: NE.NonEmpty (NE.NonEmpty NoteDurOrNoteDurNETup)
      }
    deriving Show

data VoiceConfigCore =
  VoiceConfigCore {
   _vcmPOOrPOss :: NE.NonEmpty (NE.NonEmpty (Maybe PitOctOrNEPitOcts))
  ,_vcDurss     :: NE.NonEmpty (NE.NonEmpty DurValOrDurTuplet)
  ,_vcAcctss    :: NE.NonEmpty (NE.NonEmpty Accent)
  } deriving Show

-- For each list of list of (Maybe PitOctOrNEPitOcts), the outer Maybe is
-- Nothing for a rest or Just PitOctOrNEPitOcts for 1) a pitch, octave pair
-- e.g. for a note, or 2) a non-empty list of pitch, octave pairs,
-- e.g. for a chord.
--
-- A DurValOrDurTuplet is either a 1) DurationVal, which tells the length in 128th
-- notes of a rest, pitch, or chord, or 2) a DurTuplet, which tells the numerator,
-- denominator, unit duration, and list of DurationVals to pair with a list
-- of rest, pitch, or chord.  The total duration of the list of DurationVals
-- must be a multiple of the total duration for the tuplet.
data VoiceConfig =
  -- maximum regularity: nothing changes from segment to segment, collect pitch,
  -- duration, and accent in same order as config until reaching max duration,
  -- repeating entire list of pitches, durations, or accents as necessary, so that,
  -- if lengths of lists are equal, each segment repeats same note sequence
  -- a) concatenate pitch, duration, and accent sublists into a list of lists with
  --    only one inner list
  --      e.g.: [[p0,p1,p2],[p3,p4,p5],[p6,p7,p8]] -> [[p0,p1,p2,p3,p4,p5,p6,p7,p8]]
  -- b) cycle and concat pitch, duration, and accent list of list to make infinite
  --    lists.
  -- c) combine pitch, duration, and accent sublists into list of notes until
  --    total duration matches configuration value in 128th notes
  VoiceConfigVerbatim {
  _vcvCore   :: VoiceConfigCore
  , _vcvDurVal :: Int
  }
  --
  -- Intermediate version here or option to verbatim config to extend components to
  -- be equal lengths?  Does it really matter?  Could also just edit configuration
  -- to do the same.  So maybe not.
  --
  -- introduction of some irregularity vs. verbatim:  extend sublists of pitches,
  -- durations, and accents so they're all equal lengths, then randomly pick the
  -- same sublist from the list of lists to create the next batch of pitches until
  -- reaching the max duration (all lists must contain the same number of sublists)
  -- a) tuple up inner lists of pitches, durations, and accents,
  --    all outer lists must be same length (e.g. two lists below):
  --      e.g.: ((p1,p2),(p3)), ((d1),(d2,d3)), ((a1,a2),(a3,a4)) ->
  --            (((p1,p2),(d1),(a1,a2)), ((p3),(d2,d3),(a3,a4)))
  -- b) cycle all inner lists to be the same length, e.g
  --      e.g.: (((p1,p2),(d1),(a1,a2)), ((p3),(d2,d3),(a3,a4))) ->
  --            (((p1,p2),(d1,d1),(a1,a2)), ((p3,p3),(d2,d3),(a3,a4)))
  -- c) create an infinite list of random indices 0..N-1 where N is 
  --    length of list of tuples (2 above), e.g.: 0,1,1,1,0,0,0..
  -- d) generate list of notes from pitches, durations, and accents in lists
  --      e.g. for indices 0,1: ((p1,d1,a1),(p2,d1,a2),(p3,d2,a3),(p3,d3,a4))
  | VoiceConfigSlice {
      _vccCore    :: VoiceConfigCore
      ,_vcclDurVal :: Int
      }
  -- a) randomly permute list of list of pitches, durations, accents, so
  --    order in inner lists is preserved, order of inner lists themselves 
  --    is randomized, e.g.:
  --      [[p0,p1,p2],[p3,p4],[p5,p6,p7,p8]] -> [[p5,p6,p7,p8],[p0,p1,p2],[p3,p4]], 
  -- b) flatten lists of lists to lists of list of pitches, durations, accents, cycling
  --    that list forever, e.g. from above, e.g:
  --      [p5,p6,p7,p8,p0,p1,p2,p3,p4,p5,p6,p7,p8,p0,p1,p2,p3,p4..]
  -- c) create a list of notes selecting pitch, duration, accent in order from lists in 
  --    step b (each cycled infinitely) until total duration is equal to _vcrDurVal 
  --    where _vcDurVal is in smallest units of 128th notes
  -- different from blend because one randomized reordering of inner lists is repeated,
  -- whereas blend randomly picks from sublist for entire duration
  -- same as blend because (possibly) different lengths of pitches, durations, and
  -- accents mean irregular pairings of pitch, duration, and accent to generate notes
  | VoiceConfigRepeat {
      _vcrCore   :: VoiceConfigCore
      ,_vcrDurVal :: Int
      } 
  -- maximum amount of irregularity:
  -- a) for each of list of list of pitches, durations, accents:
  --    - create an infinite list of indices 0..N-1 where N is length of outer list
  --    - use the list indices to select an infinite list of inner lists by index
  --    - rotate each inner list by _vccRotVal and concatenate the result
  -- b) combine pitch, duration, and accent sublists into list of notes until
  --    total duration matches configuration value in 128th notes
  -- irregular because:
  --  - no (regular) repetitions in the sequence of sublists, so sequences of pitch,
  --    duration, accent rarely (if ever) repeat
  --  - with different length sublists and random selection of sublists themselves,
  --    pairings of e.g. pitch and duration rarely repeat so you don't hear motifs
  -- only way to generate recognizable blend is for each outer list to contain one 
  -- sublist only, all voices use same list of pitches, durations, and accents 
  -- (even then, you need to use a config mod to stagger the arrival of the voices)
  | VoiceConfigBlend {
      _vccCore   :: VoiceConfigCore
      ,_vccDurVal :: Int
      ,_vccRotVal :: Int -- default 0 if not in config
      }
  -- same as blend, except concatenated, randomized selection of sublists from
  -- pitches are mapped to interval diffs extending over _vcxRange from first in
  -- pair to second (have to be careful random sequences of pitches always either
  -- rise or fall depending on order of pitch, octave pairs in range)
  -- sublists of pitches, durations, and accents are randomly selected, then
  -- tupled together to make notes, until the pitch exceeds the range
  -- in the bridge piece, all voices share the same configuration, the outcome
  -- is a race among similar but different voices as the randomizations result in
  -- different range and duration results, so the voices gradually become separated
  | VoiceConfigXPose {
      _vcxCore    :: VoiceConfigCore
      ,_vcxScale  :: Scale
      ,_vcxRange  :: (PitOct,PitOct)
      }
  -- accrete to NumBars beginning and end by xpose of _vcmPOOrPOss
  | VoiceConfigAccrete {
      _vcaCore     :: VoiceConfigCore
      ,_vcaNumBars :: Int
      ,_vcaInit    :: (KeySignature,PitOct)
      }
    deriving Show

makeLenses ''VoiceConfigCore
makeLenses ''VoiceConfig

path2VoiceConfigCore :: String -> Driver VoiceConfigCore
path2VoiceConfigCore pre =
  VoiceConfigCore
  <$> searchConfigParam  (pre <> ".mPitOctsss") 
  <*> searchConfigParam  (pre <> ".durss")
  <*> searchConfigParam  (pre <> ".accentss")

-- Add comparison of ordering in (start,stop) in Range
-- to ordering in mPirtOctsss in Core
path2VoiceConfigXPose' :: String -> Driver VoiceConfig
path2VoiceConfigXPose' pre = 
  VoiceConfigXPose 
  <$> path2VoiceConfigCore pre
  <*> searchConfigParam  (pre <> ".scale")
  <*> searchConfigParam  (pre <> ".range")
  
-- Verify direction of range in config matches direction of list of list of maybe pitch or pitches,
-- e.g. if range goes low to high, then sum of diffs between first and last pitch must be > 0.
path2VoiceConfigXPose :: String -> Driver VoiceConfig
path2VoiceConfigXPose pre = path2VoiceConfigXPose' pre <&> verifyRange
  where
    verifyRange vc@VoiceConfigXPose{..}
      | rangeOrd == mPOOrPOsssOrd = vc
      | otherwise = error $ "path2VoiceConfigXPose range ord: " <> show rangeOrd <> " does not match ord of list of list of maybe pitch or pitches: " <> show mPOOrPOsssOrd
      where
        rangeOrd = rangeToOrd _vcxRange
        mPOOrPOsssOrd = mPitOctOrPitOctsssToOrd _vcxScale (neMXss2MArrsXss $ _vcmPOOrPOss _vcxCore)
    verifyRange vc = error $ "path2VoiceConfigXPose unexpected VoiceConfig: " <> show vc

path2VoiceConfigRepeat :: String -> Driver VoiceConfig
path2VoiceConfigRepeat pre =
      VoiceConfigRepeat 
        <$> path2VoiceConfigCore pre
        <*> searchConfigParam  (pre <> ".durval")
  
path2VoiceConfigVerbatim :: String -> Driver VoiceConfig
path2VoiceConfigVerbatim pre =
      VoiceConfigVerbatim 
        <$> path2VoiceConfigCore pre
        <*> searchConfigParam  (pre <> ".durval")
        
path2VoiceConfigSlice' :: String -> Driver VoiceConfig
path2VoiceConfigSlice' pre =
      VoiceConfigSlice
        <$> path2VoiceConfigCore pre
        <*> searchConfigParam  (pre <> ".durval")

verifyVoiceConfigCoreListsLengths :: VoiceConfigCore -> VoiceConfigCore
verifyVoiceConfigCoreListsLengths core@VoiceConfigCore{..}
  | allSame allLengths = core
  | otherwise = error $ "verifyVoiceConfigCoreListsLengths unequal length listss: " <> show allLengths
  where
    allLengths = [NE.length _vcmPOOrPOss,NE.length _vcAcctss,NE.length _vcDurss]

verifyVoiceConfigListsLengths :: VoiceConfig -> VoiceConfig
verifyVoiceConfigListsLengths vc@VoiceConfigVerbatim{..}  = vc { _vcvCore = verifyVoiceConfigCoreListsLengths _vcvCore }
verifyVoiceConfigListsLengths vc@VoiceConfigRepeat{..}    = vc { _vcrCore = verifyVoiceConfigCoreListsLengths _vcrCore }
verifyVoiceConfigListsLengths vc@VoiceConfigBlend{..}     = vc { _vccCore = verifyVoiceConfigCoreListsLengths _vccCore }
verifyVoiceConfigListsLengths vc@VoiceConfigSlice{..}     = vc { _vccCore = verifyVoiceConfigCoreListsLengths _vccCore }
verifyVoiceConfigListsLengths vc@VoiceConfigXPose{..}     = vc { _vcxCore = verifyVoiceConfigCoreListsLengths _vcxCore }
verifyVoiceConfigListsLengths vc@VoiceConfigAccrete{..}   = vc { _vcaCore = verifyVoiceConfigCoreListsLengths _vcaCore }

path2VoiceConfigSlice :: String -> Driver VoiceConfig
path2VoiceConfigSlice pre = path2VoiceConfigSlice' pre <&> verifyVoiceConfigListsLengths

path2VoiceConfigBlend :: String -> Driver VoiceConfig
path2VoiceConfigBlend pre =
      VoiceConfigBlend 
        <$> path2VoiceConfigCore pre
        <*> searchConfigParam (pre <> ".durval")
        <*> (searchMConfigParam (pre <> ".rotval") <&> fromMaybe 0)

path2VoiceConfigAccrete' :: String -> Driver VoiceConfig
path2VoiceConfigAccrete' pre =
      VoiceConfigAccrete
        <$> path2VoiceConfigCore pre
        <*> searchConfigParam (pre <> ".numBars")
        <*> searchConfigParam (pre <> ".init")
        
path2VoiceConfigAccrete :: String -> Driver VoiceConfig
path2VoiceConfigAccrete pre = path2VoiceConfigAccrete' pre <&> verifyVoiceConfigListsLengths

type Path2VoiceConfig = String -> Driver VoiceConfig

name2VoiceConfigMap :: M.Map String Path2VoiceConfig
name2VoiceConfigMap = M.fromList [("verbatim",path2VoiceConfigVerbatim)
                                 ,("slice"   ,path2VoiceConfigSlice)
                                 ,("repeat"  ,path2VoiceConfigRepeat)
                                 ,("blend"   ,path2VoiceConfigBlend)
                                 ,("xpose"   ,path2VoiceConfigXPose)
                                 ,("accrete" ,path2VoiceConfigAccrete)]

path2VoiceConfig :: Path2VoiceConfig
path2VoiceConfig path =
  searchConfigParam (path <> ".vtype") >>= runConfigType
  where
    runConfigType cfgType =
      case M.lookup cfgType name2VoiceConfigMap of
        Nothing  -> error $ "path2VoiceConfig no converter for \"vtype:\" " <> cfgType
        Just fun -> fun path

path2VoiceConfigs :: String -> [String] -> Driver [VoiceConfig]
path2VoiceConfigs path = traverse (path2VoiceConfig . ((path <> ".") <>))

path2VoiceConfigss :: String -> [String] -> Driver [(VoiceConfig,VoiceConfig)]
path2VoiceConfigss path voices = path2VoiceConfigs path voices <&> uncurry zip . list2PairLists
  where
    list2PairLists l = splitAt (length l `div` 2) l

path2SectionConfigCore :: String -> Driver SectionConfigCore
path2SectionConfigCore pre =
  SectionConfigCore pre
  <$> searchMConfigParam  (pre <> ".cfgmods")
  <*> searchMConfigParam  (pre <> ".vesmods")

type SectionAndVoices2SectionConfig = String -> [String] -> Driver SectionConfig

sectionAndVoices2SectionConfigNeutral :: SectionAndVoices2SectionConfig
sectionAndVoices2SectionConfigNeutral pre voices =
      SectionConfigNeutral
      <$> path2SectionConfigCore pre
      <*> searchConfigParam  (pre <> ".reps")
      <*> path2VoiceConfigs pre voices
            
sectionAndVoices2SectionConfigFadeIn :: SectionAndVoices2SectionConfig
sectionAndVoices2SectionConfigFadeIn pre voices =
      SectionConfigFadeIn 
      <$> path2SectionConfigCore pre
      <*> searchConfigParam  (pre <> ".delays")
      <*> path2VoiceConfigs pre voices
      
sectionAndVoices2SectionConfigFadeOut :: SectionAndVoices2SectionConfig
sectionAndVoices2SectionConfigFadeOut pre voices =
      SectionConfigFadeOut 
      <$> path2SectionConfigCore pre
      <*> searchConfigParam  (pre <> ".drops")
      <*> path2VoiceConfigs pre voices

sectionAndVoices2SectionConfigFadeAcross :: SectionAndVoices2SectionConfig
sectionAndVoices2SectionConfigFadeAcross pre voices = 
      SectionConfigFadeAcross
      <$> path2SectionConfigCore pre
      <*> searchConfigParam  (pre <> ".reps")
      <*> path2VoiceConfigss pre voices

sectionAndVoices2SectionConfigExp :: SectionAndVoices2SectionConfig
sectionAndVoices2SectionConfigExp pre voices = sectionAndVoices2SectionConfigExp' pre voices <&> verifyMotifs
  where
    verifyMotifs se@SectionConfigExp{..} 
      | all nDurOrDurTupOk (concat $ nes2arrs _sceMotifs) = se
      | otherwise = error $ "sectionAndVoices2SectionConfigExp: invalid motifs " <> show _sceMotifs
      where
        nDurOrDurTupOk = either (const True) nDurTupOk 
        nDurTupOk (pos,tup,accs) = allSame [length pos,length $ _durtupDurations tup,length accs]
    verifyMotifs sc = error $ "sectionAndVoices2SectionConfigExp unexpected SectionConfig: " <> show sc

sectionAndVoices2SectionConfigExp' :: SectionAndVoices2SectionConfig
sectionAndVoices2SectionConfigExp' pre _ = 
  SectionConfigExp
      <$> path2SectionConfigCore pre
      <*> searchConfigParam  (pre <> ".key")
      <*> searchMConfigParam (pre <> ".scale")
      <*> searchConfigParam  (pre <> ".init")
      <*> searchConfigParam  (pre <> ".numCycles")
      <*> searchConfigParam  (pre <> ".motifs")
      
name2SectionConfigMap :: M.Map String SectionAndVoices2SectionConfig
name2SectionConfigMap = M.fromList [("neutral"   ,sectionAndVoices2SectionConfigNeutral)
                                   ,("fadein"    ,sectionAndVoices2SectionConfigFadeIn)
                                   ,("fadeout"   ,sectionAndVoices2SectionConfigFadeOut)
                                   ,("fadeacross",sectionAndVoices2SectionConfigFadeAcross)
                                   ,("exp"       ,sectionAndVoices2SectionConfigExp)]

path2SectionConfig :: String -> Driver SectionConfig
path2SectionConfig section = do
  voices <- cfgPath2Keys ("voice" `isPrefixOf`) section
  searchConfigParam (section <> ".stype") >>= flip runConfigType voices
  where
    runConfigType cfgType voices =
      case M.lookup cfgType name2SectionConfigMap of
        Nothing  -> error $ "path2SectionConfig no converter for \"stype:\" " <> cfgType
        Just fun -> fun section voices

path2SectionConfigs :: String -> NE.NonEmpty String -> Driver (NE.NonEmpty SectionConfig)
path2SectionConfigs group = traverse (path2SectionConfig . ((group <> ".") <>))

type GroupAndSections2GroupConfig = String -> NE.NonEmpty String -> Driver GroupConfig

groupAndSections2GroupConfigNeutral :: GroupAndSections2GroupConfig
groupAndSections2GroupConfigNeutral group sections =
      GroupConfigNeutral group
      <$> searchMConfigParam (group <> ".grname")
      <*> path2SectionConfigs group sections
      
groupAndSections2GroupConfigEvenEnds :: GroupAndSections2GroupConfig
groupAndSections2GroupConfigEvenEnds group sections =
      GroupConfigEvenEnds group
      <$> searchMConfigParam (group <> ".grname")
      <*> path2SectionConfigs group sections
      
groupAndSections2GroupConfigOrdered :: GroupAndSections2GroupConfig
groupAndSections2GroupConfigOrdered group sections =
      GroupConfigOrdered group
      <$> searchMConfigParam (group <> ".grname")
      <*> searchConfigParam (group <> ".grsnames")
      <*> path2SectionConfigs group sections

name2GroupConfigMap :: M.Map String GroupAndSections2GroupConfig
name2GroupConfigMap = M.fromList [("neutral" ,groupAndSections2GroupConfigNeutral)
                                 ,("evenends",groupAndSections2GroupConfigEvenEnds)
                                 ,("ordered" ,groupAndSections2GroupConfigOrdered)]
                         
cfg2GroupConfig :: String -> NE.NonEmpty String -> Driver GroupConfig
cfg2GroupConfig group sections =
  searchConfigParam (group <> ".grtype") >>= runConfigType
  where
    runConfigType cfgType =
      case M.lookup cfgType name2GroupConfigMap of
        Nothing  -> error $ "cfg2GroupConfig no converter for \"grtype:\" " <> cfgType
        Just fun -> fun group sections 

-- cfg2SectionConfig :: String -> NE.NonEmpty String -> Driver SectionConfig
-- cfg2SectionConfig section voices =
--   searchConfigParam (section <> ".stype") >>= runConfigType
--   where
--     runConfigType cfgType =
--       case M.lookup cfgType name2SectionConfigMap of
--         Nothing  -> error $ "cfg2SectionConfig no converter for \"stype:\" " <> cfgType
--         Just fun -> fun section voices
