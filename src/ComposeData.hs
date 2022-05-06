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
    -- TBD:  config with list of Bool to say flatten per voice per segment per param
  | SectionConfigHomophony {
                       _schPath        :: String
                      ,_schReps        :: Int
                      ,_schMName       :: Maybe String
                      ,_schMConfigMods :: Maybe (NE.NonEmpty String)
                      ,_schMVesMods    :: Maybe (NE.NonEmpty String)
                      ,_schVoices      :: NE.NonEmpty VoiceConfig
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
                      ,_scfcVoicesAB    :: NE.NonEmpty (VoiceConfig,VoiceConfig) -- must be VoiceConfigCell
                      }
    deriving Show

data VoiceConfig =
  VoiceConfigXPose {
                 _vcxInstr       :: Instrument
                 ,_vcxKey        :: KeySignature
                 ,_vcxScale      :: Scale
                 ,_vcxTime       :: TimeSignature
                 ,_vcxmPOOrPOss  :: NE.NonEmpty (NE.NonEmpty (Maybe PitOctOrNEPitOcts))
                 ,_vcxDurss      :: NE.NonEmpty (NE.NonEmpty DurOrDurTuplet)
                 ,_vcxAcctss     :: NE.NonEmpty (NE.NonEmpty Accent)
                 ,_vcxRange      :: ((Pitch,Octave),(Pitch,Octave))
                 } 
  | VoiceConfigRepeat {
                    _vcrInstr       :: Instrument
                    ,_vcrKey        :: KeySignature
                    ,_vcrTime       :: TimeSignature
                    ,_vcrmPOOrPOss  :: NE.NonEmpty (NE.NonEmpty (Maybe PitOctOrNEPitOcts))
                    ,_vcrDurss      :: NE.NonEmpty (NE.NonEmpty DurOrDurTuplet)
                    ,_vcrAcctss     :: NE.NonEmpty (NE.NonEmpty Accent)
                    ,_vcrDurVal     :: Int
                 } 
  | VoiceConfigVerbatim {
                    _vcvInstr       :: Instrument
                    ,_vcvKey        :: KeySignature
                    ,_vcvTime       :: TimeSignature
                    ,_vcvmPOOrPOss  :: NE.NonEmpty (NE.NonEmpty (Maybe PitOctOrNEPitOcts))
                    ,_vcvDurss      :: NE.NonEmpty (NE.NonEmpty DurOrDurTuplet)
                    ,_vcvAcctss     :: NE.NonEmpty (NE.NonEmpty Accent)
                    ,_vcvDurVal     :: Int
                 } 
  | VoiceConfigCell {
                    _vcclInstr       :: Instrument
                    ,_vcclKey        :: KeySignature
                    ,_vcclTime       :: TimeSignature
                    ,_vcclmPOOrPOss  :: NE.NonEmpty (NE.NonEmpty (Maybe PitOctOrNEPitOcts))
                    ,_vcclDurss      :: NE.NonEmpty (NE.NonEmpty DurOrDurTuplet)
                    ,_vcclAcctss     :: NE.NonEmpty (NE.NonEmpty Accent)
                    ,_vcclDurVal     :: Int
                 } 
  | VoiceConfigCanon {
                    _vccInstr       :: Instrument
                    ,_vccKey        :: KeySignature
                    ,_vccTime       :: TimeSignature
                    ,_vccmPOOrPOss  :: NE.NonEmpty (NE.NonEmpty (Maybe PitOctOrNEPitOcts))
                    ,_vccDurss      :: NE.NonEmpty (NE.NonEmpty DurOrDurTuplet)
                    ,_vccAcctss     :: NE.NonEmpty (NE.NonEmpty Accent)
                    ,_vccDurVal     :: Int
                    ,_vccRotVal     :: Int
                 }
  -- Tricky bits:  for a DurTuplet, need to have corresponding counts of pitches and accents.
  -- But it's more complicated than that:  self-similar expansion expects integral units, but
  -- DurTuplet is a collection.  Reduce to NE.NonEmpty DurationVal.
  -- Going to require exceptional handling for some common VoiceConfig handlers.
  -- PitOctOrNEPitOcts is still integral, either a single note or a chord.
  -- Won't it be likely to pair Durs and Accts and treat them integrally with respect to
  -- self-similar expansion?  Seems unlikely I'll want different self-similar expansions
  -- for durations vs. accents.  That's reminiscent of uniform randomization in other
  -- voice config types.
  -- Note will really want equal-length lists of pitches and dur/accent pairs and equal
  -- count expansions of self-similar integer lists.  Or rather it's more than that,
  -- because focus of two expansions should be similar in the sense from generation
  -- to generation there's a progression and then within a sub-generation there's
  -- a finer-grain progression.  So e.g. if there are two progressions, one for pitches
  -- and the other for durations and accents, how do the generations relate?
    --
{--    
  | VoiceConfigSelfSim {
                    _vcsInstr       :: Instrument
                    ,_vcsKey        :: KeySignature
                    ,_vcsTime       :: TimeSignature
                    ,_vcsPitSSs     :: NE.NonEmpty Int
                    ,_vcsmPOOrPOs   :: NE.NonEmpty (Maybe PitOctOrNEPitOcts)
                    ,_vcsDurAcctSSs :: NE.NonEmpty Int
                    ,_vcsDurAcctPrs :: NE.NonEmpty (DurationVal,Accent)
                    ,_vcsDurVal     :: Int
                    ,_vcsSSDepth    :: Int
                 }
--}
    deriving Show

makeLenses ''VoiceConfig

path2VoiceConfigXPose :: String -> Driver VoiceConfig
path2VoiceConfigXPose pre =
      VoiceConfigXPose 
        <$> searchConfigParam  (pre <> ".instr")
        <*> searchConfigParam  (pre <> ".key")
        <*> searchConfigParam  (pre <> ".scale")
        <*> searchConfigParam  (pre <> ".time")
        <*> searchConfigParam  (pre <> ".mPitOctsss")
        <*> searchConfigParam  (pre <> ".durss")
        <*> searchConfigParam  (pre <> ".accentss")
        <*> searchConfigParam  (pre <> ".range")

path2VoiceConfigRepeat :: String -> Driver VoiceConfig
path2VoiceConfigRepeat pre =
      VoiceConfigRepeat 
        <$> searchConfigParam  (pre <> ".instr")
        <*> searchConfigParam  (pre <> ".key")
        <*> searchConfigParam  (pre <> ".time")
        <*> searchConfigParam  (pre <> ".mPitOctsss")
        <*> searchConfigParam  (pre <> ".durss")
        <*> searchConfigParam  (pre <> ".accentss")
        <*> searchConfigParam  (pre <> ".durval")

path2VoiceConfigVerbatim :: String -> Driver VoiceConfig
path2VoiceConfigVerbatim pre =
      VoiceConfigVerbatim 
        <$> searchConfigParam  (pre <> ".instr")
        <*> searchConfigParam  (pre <> ".key")
        <*> searchConfigParam  (pre <> ".time")
        <*> searchConfigParam  (pre <> ".mPitOctsss")
        <*> searchConfigParam  (pre <> ".durss")
        <*> searchConfigParam  (pre <> ".accentss")
        <*> searchConfigParam  (pre <> ".durval")
        
path2VoiceConfigCell' :: String -> Driver VoiceConfig
path2VoiceConfigCell' pre =
      VoiceConfigCell
        <$> searchConfigParam  (pre <> ".instr")
        <*> searchConfigParam  (pre <> ".key")
        <*> searchConfigParam  (pre <> ".time")
        <*> searchConfigParam  (pre <> ".mPitOctsss")
        <*> searchConfigParam  (pre <> ".durss")
        <*> searchConfigParam  (pre <> ".accentss")
        <*> searchConfigParam  (pre <> ".durval")

path2VoiceConfigCell :: String -> Driver VoiceConfig
path2VoiceConfigCell pre = path2VoiceConfigCell' pre <&> verifyListsLengths
  where
    verifyListsLengths vc@VoiceConfigCell{..}
      | all (== head allLengths) allLengths = vc
      | otherwise = error $ "path2VoiceConfigCell unequal length listss: " <> show allLengths
      where
        allLengths = [NE.length _vcclmPOOrPOss,NE.length _vcclAcctss,NE.length _vcclDurss]
    verifyListsLengths vc = error $ "pagth2VoiceConfigCell unexpected VoiceConfig: " <> show vc

path2VoiceConfigCanon :: String -> Driver VoiceConfig
path2VoiceConfigCanon pre =
      VoiceConfigCanon 
        <$> searchConfigParam  (pre <> ".instr")
        <*> searchConfigParam  (pre <> ".key")
        <*> searchConfigParam  (pre <> ".time")
        <*> searchConfigParam  (pre <> ".mPitOctsss")
        <*> searchConfigParam  (pre <> ".durss")
        <*> searchConfigParam  (pre <> ".accentss")
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
      
sectionAndVoices2SectionConfigHomophony :: SectionAndVoices2SectionConfig
sectionAndVoices2SectionConfigHomophony section voices =
      SectionConfigHomophony section
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
                                   ,("homophony" ,sectionAndVoices2SectionConfigHomophony)
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
