{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}

module ComposeData where

import Driver (Driver, searchConfigParam, searchMConfigParam, cfgPath2Keys)
import Types

import Control.Lens hiding (pre)
import Data.List (isPrefixOf)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M

-- A piece is a NE.NonEmpty GroupConfig
-- where the last Section of each is extended
-- to the same length which is extended to the
-- end of the last bar and the beginning is
-- annotated with a rehearsal letter.
-- Except that probably should be optional
-- on a per sections basis, maybe with two
-- flags, one to say to extend to the same
-- length and another to say to extend to the
-- end of the last bar.
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
  | VoiceConfigCell {
                    _vcclInstr       :: Instrument
                    ,_vcclKey        :: KeySignature
                    ,_vcclScale      :: Scale
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
    deriving Show

makeLenses ''VoiceConfig

data VoiceRuntimeConfig =
  VoiceRuntimeConfig {
                   _vrcSctnPath :: String
                   ,_vrcMNumVoc  :: Maybe Int
                   ,_vrcCntVocs  :: Int
                   ,_vrcNumVoc   :: Int
                   ,_vrcCntSegs  :: Int
                   ,_vrcNumSeg   :: Int
                   } deriving Show

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
        
path2VoiceConfigCell' :: String -> Driver VoiceConfig
path2VoiceConfigCell' pre =
      VoiceConfigCell
        <$> searchConfigParam  (pre <> ".instr")
        <*> searchConfigParam  (pre <> ".key")
        <*> searchConfigParam  (pre <> ".scale")
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
name2VoiceConfigMap = M.fromList [("xpose" ,path2VoiceConfigXPose)
                                 ,("repeat",path2VoiceConfigRepeat)
                                 ,("cell"  ,path2VoiceConfigCell)
                                 ,("canon" ,path2VoiceConfigCanon)]

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

type SectionAndVoices2SectionConfig = String -> NE.NonEmpty String -> Driver SectionConfig

sectionAndVoices2SectionConfigNeutral :: SectionAndVoices2SectionConfig
sectionAndVoices2SectionConfigNeutral section voices =
      SectionConfigNeutral section
      <$> searchConfigParam (section <> ".reps")
      <*> searchMConfigParam (section <> ".sctname")
      <*> searchMConfigParam (section <> ".cfgmods")
      <*> searchMConfigParam (section <> ".vesmods")
      <*> path2VoiceConfigs section voices
      
sectionAndVoices2SectionConfigHomophony :: SectionAndVoices2SectionConfig
sectionAndVoices2SectionConfigHomophony section voices =
      SectionConfigHomophony section
      <$> searchConfigParam (section <> ".reps")
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

name2SectionConfigMap :: M.Map String SectionAndVoices2SectionConfig
name2SectionConfigMap = M.fromList [("neutral"  ,sectionAndVoices2SectionConfigNeutral)
                                   ,("homophony",sectionAndVoices2SectionConfigHomophony)
                                   ,("fadein"   ,sectionAndVoices2SectionConfigFadeIn)
                                   ,("fadeout"  ,sectionAndVoices2SectionConfigFadeOut)]

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
        Nothing  -> error $ "cfg2GroupConfig no converter for \"sstype:\" " <> cfgType
        Just fun -> fun group sections 

-- cfg2SectionConfig :: String -> NE.NonEmpty String -> Driver SectionConfig
-- cfg2SectionConfig section voices =
--   searchConfigParam (section <> ".stype") >>= runConfigType
--   where
--     runConfigType cfgType =
--       case M.lookup cfgType name2SectionConfigMap of
--         Nothing  -> error $ "cfg2SectionConfig no converter for \"stype:\" " <> cfgType
--         Just fun -> fun section voices
