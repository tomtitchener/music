{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}

module ComposeData where

import Driver (Driver, searchConfigParam, searchMConfigParam, cfgPath2Keys)
import Types

import Control.Lens hiding (pre)
import Data.List (isPrefixOf)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M

data GroupConfig =
  GroupConfigNeutral {
                     _gcnPath    :: String
                    ,_gcnMName   :: Maybe String
                    ,_gcnConfigs :: NE.NonEmpty SectionConfig
                    }
  | GroupConfigEvenEnds {
                     _gcePath    :: String
                    ,_gceMName   :: Maybe String
                    ,_gceConfigs :: NE.NonEmpty SectionConfig
                    }
  deriving Show

data SectionConfig =
  SectionConfigNeutral {
                       _scnPath        :: String
                      ,_scnReps        :: Int
                      ,_scnMName       :: Maybe String
                      ,_scnMConfigMods :: Maybe (NE.NonEmpty String)
                      ,_scnMVesMods    :: Maybe (NE.NonEmpty String)
                      ,_scnVoices      :: NE.NonEmpty VoiceConfig
                 }
  | SectionConfigFadeIn {
                       _scfiPath        :: String
                      ,_scfiOrder       :: NE.NonEmpty Int
                      ,_scfiMName       :: Maybe String
                      ,_scfiMConfigMods :: Maybe (NE.NonEmpty String)
                      ,_scfiMVesMods    :: Maybe (NE.NonEmpty String)
                      ,_scfiVoices      :: NE.NonEmpty VoiceConfig
                      }
  | SectionConfigFadeOut {
                       _scfoPath        :: String
                      ,_scfoOrder       :: NE.NonEmpty Int
                      ,_scfoMName       :: Maybe String
                      ,_scfoMConfigMods :: Maybe (NE.NonEmpty String)
                      ,_scfoMVesMods    :: Maybe (NE.NonEmpty String)
                      ,_scfoVoices      :: NE.NonEmpty VoiceConfig
                      }
  | SectionConfigFadeAcross {
                       _scfcPath        :: String
                      ,_scfcReps         :: Int
                      ,_scfcMName       :: Maybe String
                      ,_scfcMConfigMods :: Maybe (NE.NonEmpty String)
                      ,_scfcMVesMods    :: Maybe (NE.NonEmpty String)
                      -- (a,b) pairs for two equal-length consorts
                      -- both must be sliceable, though they can be
                      -- different lengths
                      ,_scfcVoicesAB    :: NE.NonEmpty (VoiceConfig,VoiceConfig)
                      }
    deriving Show

data VoiceConfigCore =
  VoiceConfigCore {
   _vcmPOOrPOss :: NE.NonEmpty (NE.NonEmpty (Maybe PitOctOrNEPitOcts))
  ,_vcDurss    :: NE.NonEmpty (NE.NonEmpty DurOrDurTuplet)
  ,_vcAcctss   :: NE.NonEmpty (NE.NonEmpty Accent)
  } deriving Show

-- For each list of list of (Maybe PitOctOrNEPitOcts), the outer Maybe is
-- Nothing for a rest or Just PitOctOrNEPitOcts for 1) a pitch, octave pair
-- e.g. for a note, or 2) a non-empty list of pitch, octave pairs,
-- e.g. for a chord.
--
-- A DurOrDurTuplet is either a 1) DurationVal, which tells the length in 128th
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
   ,_vcvDurVal :: Int
   }
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
  | VoiceConfigCell {
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
  -- different from canon because one randomized reordering of inner lists is repeated,
  -- whereas canon randomly picks from sublist for entire duration
  -- same as canon because (possibly) different lengths of pitches, durations, and
  -- accents mean irregular pairings of pitch, duration, and accent to generate notes
  | VoiceConfigRepeat {
      _vcrCore    :: VoiceConfigCore
     ,_vcrDurVal    :: Int
     } 
  -- a) for each of list of list of pitches, durations, accents:
  --    - create an infinite list of indices 0..N-1 where N is length of outer list
  --    - use the list indices to select an infinite list of inner lists by index
  --    - rotate each inner list by _vccRotVal and concatenate the result
  -- b) combine pitch, duration, and accent sublists into list of notes until
  --    total duration matches configuration value in 128th notes
  -- maximum amount of irregularity:
  --  - no (regular) repetitions in the sequence of sublists, so sequences of pitch,
  --    duration, accent rarely (if ever) repeat
  --  - with different length sublists and random selection of sublists themselves,
  --    pairings of e.g. pitch and duration rarely repeat so you don't hear motifs
  -- only way to generate recognizable canon is for each outer list to contain one 
  -- sublist only, all voices use same list of pitches, durations, and accents 
  -- (even then, you need to use a config mod to stagger the arrival of the voices)
  -- poor name choice, maybe VoiceConfigRandomizedSublists would be better?
  | VoiceConfigCanon {
       _vccCore    :: VoiceConfigCore
      ,_vccDurVal    :: Int
      ,_vccRotVal    :: Int
      }
  -- same as canon, except concatenated, randomized selection of sublists from
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
      ,_vcxScale     :: Scale
      ,_vcxRange     :: ((Pitch,Octave),(Pitch,Octave))
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
  
path2VoiceConfigXPose :: String -> Driver VoiceConfig
path2VoiceConfigXPose pre =
      VoiceConfigXPose 
        <$> path2VoiceConfigCore pre
        <*> searchConfigParam  (pre <> ".scale")
        <*> searchConfigParam  (pre <> ".range")

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
        
path2VoiceConfigCell' :: String -> Driver VoiceConfig
path2VoiceConfigCell' pre =
      VoiceConfigCell
        <$> path2VoiceConfigCore pre
        <*> searchConfigParam  (pre <> ".durval")

path2VoiceConfigCell :: String -> Driver VoiceConfig
path2VoiceConfigCell pre = path2VoiceConfigCell' pre <&> verifyListsLengths
  where
    verifyListsLengths vc@VoiceConfigCell{..}
      | all (== head allLengths) allLengths = vc
      | otherwise = error $ "path2VoiceConfigCell unequal length listss: " <> show allLengths
      where
        allLengths = [NE.length (_vcmPOOrPOss _vccCore),NE.length (_vcAcctss _vccCore) ,NE.length (_vcDurss _vccCore)]
    verifyListsLengths vc = error $ "pagth2VoiceConfigCell unexpected VoiceConfig: " <> show vc

path2VoiceConfigCanon :: String -> Driver VoiceConfig
path2VoiceConfigCanon pre =
      VoiceConfigCanon 
        <$> path2VoiceConfigCore pre
        <*> searchConfigParam  (pre <> ".durval")
        <*> searchConfigParam  (pre <> ".rotval")
        
type Path2VoiceConfig = String -> Driver VoiceConfig

name2VoiceConfigMap :: M.Map String Path2VoiceConfig
name2VoiceConfigMap = M.fromList [("xpose"   ,path2VoiceConfigXPose)
                                 ,("repeat"  ,path2VoiceConfigRepeat)
                                 ,("verbatim",path2VoiceConfigVerbatim)
                                 ,("cell"    ,path2VoiceConfigCell)
                                 ,("canon"   ,path2VoiceConfigCanon)]

path2VoiceConfig :: Path2VoiceConfig
path2VoiceConfig path =
  searchConfigParam (path <> ".vtype") >>= runConfigType
  where
    runConfigType cfgType =
      case M.lookup cfgType name2VoiceConfigMap of
        Nothing  -> error $ "path2VoiceConfig no converter for \"vtype:\" " <> cfgType
        Just fun -> fun path

path2VoiceConfigs :: String -> NE.NonEmpty String -> Driver (NE.NonEmpty VoiceConfig)
path2VoiceConfigs path = traverse (path2VoiceConfig . ((path <> ".") <>))

path2VoiceConfigss :: String -> NE.NonEmpty String -> Driver (NE.NonEmpty (VoiceConfig,VoiceConfig))
path2VoiceConfigss path voices = path2VoiceConfigs path voices <&> NE.fromList . uncurry zip . list2PairLists . NE.toList
  where
    list2PairLists l = splitAt (length l `div` 2) l

type SectionAndVoices2SectionConfig = String -> NE.NonEmpty String -> Driver SectionConfig

sectionAndVoices2SectionConfigNeutral :: SectionAndVoices2SectionConfig
sectionAndVoices2SectionConfigNeutral section voices =
      SectionConfigNeutral section
      <$> searchConfigParam  (section <> ".reps")
      <*> searchMConfigParam (section <> ".sctname")
      <*> searchMConfigParam (section <> ".cfgmods")
      <*> searchMConfigParam (section <> ".vesmods")
      <*> path2VoiceConfigs section voices
            
sectionAndVoices2SectionConfigFadeIn :: SectionAndVoices2SectionConfig
sectionAndVoices2SectionConfigFadeIn section voices =
      SectionConfigFadeIn section
      <$> searchConfigParam  (section <> ".delays")
      <*> searchMConfigParam (section <> ".sctname")
      <*> searchMConfigParam (section <> ".cfgmods")
      <*> searchMConfigParam (section <> ".vesmods")
      <*> path2VoiceConfigs section voices
      
sectionAndVoices2SectionConfigFadeOut :: SectionAndVoices2SectionConfig
sectionAndVoices2SectionConfigFadeOut section voices =
      SectionConfigFadeOut section
      <$> searchConfigParam  (section <> ".drops")
      <*> searchMConfigParam (section <> ".sctname")
      <*> searchMConfigParam (section <> ".cfgmods")
      <*> searchMConfigParam (section <> ".vesmods")
      <*> path2VoiceConfigs section voices

sectionAndVoices2SectionConfigFadeAcross :: SectionAndVoices2SectionConfig
sectionAndVoices2SectionConfigFadeAcross section voices = 
      SectionConfigFadeAcross section
      <$> searchConfigParam  (section <> ".reps")
      <*> searchMConfigParam (section <> ".sctname")
      <*> searchMConfigParam (section <> ".cfgmods")
      <*> searchMConfigParam (section <> ".vesmods")
      <*> path2VoiceConfigss section voices

name2SectionConfigMap :: M.Map String SectionAndVoices2SectionConfig
name2SectionConfigMap = M.fromList [("neutral"   ,sectionAndVoices2SectionConfigNeutral)
                                   ,("fadein"    ,sectionAndVoices2SectionConfigFadeIn)
                                   ,("fadeout"   ,sectionAndVoices2SectionConfigFadeOut)
                                   ,("fadeacross",sectionAndVoices2SectionConfigFadeAcross)]

path2SectionConfig :: String -> Driver SectionConfig
path2SectionConfig section = do
  voices <- cfgPath2Keys ("voice" `isPrefixOf`) section
  searchConfigParam (section <> ".stype") >>= flip runConfigType (NE.fromList voices)
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

name2GroupConfigMap :: M.Map String GroupAndSections2GroupConfig
name2GroupConfigMap = M.fromList [("neutral"  ,groupAndSections2GroupConfigNeutral)
                                 ,("evenends" ,groupAndSections2GroupConfigEvenEnds)]
                         
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
