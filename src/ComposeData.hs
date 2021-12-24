
module ComposeData (VoiceConfig(..), SectionConfig(..), VoiceRuntimeConfig(..), cfg2SectionConfig) where

import Driver (Driver, searchConfigParam, searchMConfigParam)
import Types

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M

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
        <*> searchConfigParam  (pre <> ".mpitOctss")
        <*> searchConfigParam  (pre <> ".durss")
        <*> searchConfigParam  (pre <> ".accentss")
        <*> searchConfigParam  (pre <> ".range")

path2VoiceConfigRepeat :: String -> Driver VoiceConfig
path2VoiceConfigRepeat pre =
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

path2VoiceConfigCanon :: String -> Driver VoiceConfig
path2VoiceConfigCanon pre =
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
        
type Path2VoiceConfig = String -> Driver VoiceConfig

name2VoiceConfigMap :: M.Map String Path2VoiceConfig
name2VoiceConfigMap = M.fromList [("xpose" ,path2VoiceConfigXPose)
                                 ,("repeat",path2VoiceConfigRepeat)
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
      <*> searchMConfigParam (section <> ".norrsmods")
      <*> path2VoiceConfigs section voices
      
sectionAndVoices2SectionConfigFadeIn :: SectionAndVoices2SectionConfig
sectionAndVoices2SectionConfigFadeIn section voices =
      SectionConfigFadeIn section
      <$> searchConfigParam  (section <> ".delays")
      <*> searchMConfigParam (section <> ".sctname")
      <*> searchMConfigParam (section <> ".cfgmods")
      <*> searchMConfigParam (section <> ".norrsmods")
      <*> path2VoiceConfigs section voices
      
sectionAndVoices2SectionConfigFadeOut :: SectionAndVoices2SectionConfig
sectionAndVoices2SectionConfigFadeOut section voices =
      SectionConfigFadeOut section
      <$> searchConfigParam  (section <> ".drops")
      <*> searchMConfigParam (section <> ".sctname")
      <*> searchMConfigParam (section <> ".cfgmods")
      <*> searchMConfigParam (section <> ".norrsmods")
      <*> path2VoiceConfigs section voices

name2SectionConfigMap :: M.Map String SectionAndVoices2SectionConfig
name2SectionConfigMap = M.fromList [("neutral",sectionAndVoices2SectionConfigNeutral)
                                   ,("fadein" ,sectionAndVoices2SectionConfigFadeIn)
                                   ,("fadeout",sectionAndVoices2SectionConfigFadeOut)]

cfg2SectionConfig :: String -> NE.NonEmpty String -> Driver SectionConfig
cfg2SectionConfig section voices =
  searchConfigParam (section <> ".stype") >>= runConfigType
  where
    runConfigType cfgType =
      case M.lookup cfgType name2SectionConfigMap of
        Nothing  -> error $ "cfg2SectionConfig no converter for \"stype:\" " <> cfgType
        Just fun -> fun section voices

