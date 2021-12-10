module Compositions.Swirls.Compose (cfg2SwirlsScore) where


import Control.Lens (Bifunctor(bimap), (&), (<&>))
import Data.List (isPrefixOf, zipWith4, transpose)
import Data.Maybe (fromMaybe)
import qualified Data.List.NonEmpty as NE
import Data.Tuple.Extra (dupe, second, secondM)

import Driver
import Types

import Compositions.Swirls.Utils

------------
-- Swirls --
------------

changeClefs :: Int
changeClefs = 5

cfg2SwirlsScore :: String -> Driver ()
cfg2SwirlsScore title = do
  chgClfInt <- searchMConfigParam (title <> ".common.changeclefs") <&> fromMaybe changeClefs
  tempoInt  <- searchConfigParam  (title <> ".common.tempo")
  timeSig   <- searchConfigParam  (title <> ".common.time")
  keySig    <- searchConfigParam  (title <> ".common.key")
  instr     <- searchConfigParam  (title <> ".common.instr")
  sections  <- cfgPath2Keys ("section" `isPrefixOf`) title <&> fmap ((title <> ".") <>)
  secVcsPrs <- traverse (secondM (cfgPath2Keys ("voice" `isPrefixOf`)) . dupe) sections
  secTups   <- traverse (uncurry cfg2SectionConfigTup) (second NE.fromList <$> secVcsPrs)
  nOrRsss   <- traverse sectionConfigTup2NoteOrRests secTups
  let nOrRss    = concat <$> transpose nOrRsss
      voices    = pipeline chgClfInt tempoInt timeSig keySig instr nOrRss
  writeScore ("./" <> title <> ".ly") $ Score "no comment" voices
  where
    pipeline :: Int -> Int -> TimeSignature -> KeySignature -> Instrument -> [[NoteOrRest]] -> NE.NonEmpty Voice
    pipeline chgClfs tempoInt timeSig keySig instr nOrRss = --
      zipWith alignNoteOrRestsDurations timeSigs nOrRss            -- -> [[NoteOrRest]]
      & zipWith splitNoteOrRests winLens                           -- -> [([VoiceEvent],[VoiceEvent])]
      & zipWith3 (mkVesPrTotDur (maximum veLens)) veLens timeSigs  -- -> [([VoiceEvent],[VoiceEvent])]
      & fmap (bimap NE.fromList NE.fromList)                       -- -> [(NonEmpty VoiceEvent,NonEmpty VoiceEvent)]
      & NE.fromList . zipWith4 genPolyVocs instrs keySigs timeSigs -- -> NonEmpty Voice
      & tagTempo tempo                                             -- -> NonEmpty Voice
      where
        tempo      = TempoDur QDur (fromIntegral tempoInt)
        cntVoices  = length nOrRss
        veLens     = nOrRs2DurVal <$> nOrRss
        timeSigs   = replicate cntVoices timeSig
        keySigs    = replicate cntVoices keySig
        instrs     = replicate cntVoices instr
        winLens    = replicate cntVoices chgClfs
