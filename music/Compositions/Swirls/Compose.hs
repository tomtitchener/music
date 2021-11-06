{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compositions.Swirls.Compose (cfg2SwirlsScore) where


import Control.Applicative
import Control.Lens (Bifunctor(bimap), (&), (<&>))
import Data.List (isPrefixOf, zipWith4)
import qualified Data.List.NonEmpty as NE
import Data.Tuple.Extra (dupe, second, secondM)

import Driver
import Types
import Utils

import Compositions.Swirls.Utils

------------
-- Swirls --
------------

cfg2SwirlsScore :: String -> Driver ()
cfg2SwirlsScore title = do
  tempoInt  <- searchConfigParam (title <> ".globals.tempo")::(Driver Int)
  timeSig   <- searchConfigParam (title <> ".globals.time")::(Driver TimeSignature)
  keySig    <- searchConfigParam (title <> ".globals.key")::(Driver KeySignature)
  instr     <- searchConfigParam (title <> ".globals.instr")::(Driver Instrument)
  sections  <- cfgPath2Keys ("section" `isPrefixOf`) title <&> fmap ((title <> ".") <>)
  secVcsPrs <- traverse (secondM (cfgPath2Keys ("voice" `isPrefixOf`)) . dupe) sections
  vocTups   <- traverse (uncurry cfg2SwirlsTups) (second NE.fromList <$> secVcsPrs)
  nOrRsss   <- traverse (\tups -> traverse swirlsTup2NoteOrRests tups <&> ZipList . NE.toList) vocTups
  let nOrRss    = concat <$> getZipList (sequenceA nOrRsss)
      tempo     = TempoDur QDur (fromIntegral tempoInt)
      voices    = pipeline tempo timeSig keySig instr nOrRss
  writeScore ("./" <> title <> ".ly") $ Score "no comment" voices
  where
    pipeline :: Tempo -> TimeSignature -> KeySignature -> Instrument -> [[NoteOrRest]] -> NE.NonEmpty Voice
    pipeline tempo timeSig keySig instr nOrRss = --
      zipWith alignNoteOrRestsDurations timeSigs nOrRss            -- -> [[NoteOrRest]]
      & zipWith splitNoteOrRests winLens                           -- -> [([VoiceEvent],[VoiceEvent])]
      & zipWith tagFirstNotes firstNotes                           -- -> [([VoiceEvent],[VoiceEvent])]
      & zipWith3 (mkTotDur (maximum veLens)) veLens timeSigs       -- -> [([VoiceEvent],[VoiceEvent])]
      & fmap (bimap NE.fromList NE.fromList)                       -- -> [(NonEmpty VoiceEvent,NonEmpty VoiceEvent)]
      & NE.fromList . zipWith4 genPolyVocs instrs keySigs timeSigs -- -> NonEmpty Voice
      & tagTempo tempo                                             -- -> NonEmpty Voice
      where
        cntVoices  = length nOrRss
        veLens     = nOrRs2DurVal <$> nOrRss
        timeSigs   = replicate cntVoices timeSig
        keySigs    = replicate cntVoices keySig
        instrs     = replicate cntVoices instr
        winLens    = replicate cntVoices 5 -- tbd: magic constant
        firstNotes = replicate cntVoices firstNote
        firstNote  = Note C COct EDur (singleton Staccatissimo) PPP NoSwell False
{-
TBD:
  1) experiment with different config.yml components:
     a) mpitss:
        - long list of two or three note components, no rests
        - list of easily recognizable motifs, c-d-c-d, e-g-e-g, etc.
        - make interval arithmetic work with non-chromatic scale (does it already?)
        - experiment with per-section scales
     b) mpitss/durs/accents:
        - uniform lengths of mpitss sub-lists, lengths of durs, accents
     c) durs/accents:
        - make multiples to also choose random order among parallel to mpitss
-}


{-
Graveyard:

      -- ghost voices
      -- manyIntPrss = cycle <$> nes2arrs (_stGhosts <$> tups)
      -- gWinLens    = replicate cntVoices 1 -- tbd: magic constant
      -- gVoices     = zipWith3 squashNoteOrRests manyIntPrss timeSigs noteOrRestss
      --               &  pipeline tempo s1tups gWinLens
-}
