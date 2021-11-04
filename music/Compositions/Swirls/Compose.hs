{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Compositions.Swirls.Compose (cfg2SwirlsScore) where


import Control.Applicative
import Control.Lens (Bifunctor(bimap), (&), (<&>))
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
  tempoInt::Int <- searchConfigParam (title <> ".globals.tempo")
  sections      <- cfgPath2Keys "section[[:digit:]]" title <&> fmap ((title <> ".") <>)
  secVcsPrs     <- traverse (secondM (cfgPath2Keys "voice[[:digit:]]") . dupe) sections
  tupss         <- traverse (uncurry cfg2SwirlsTups) (second NE.fromList <$> secVcsPrs)
  nOrRsss       <- traverse (\tups -> traverse swirlsTup2NoteOrRests tups <&> ZipList . NE.toList) tupss
  let nOrRss    = concat <$> getZipList (sequenceA nOrRsss)
      tempo     = TempoDur QDur (fromIntegral tempoInt)
      voices    = pipeline tempo (head tupss) nOrRss
  writeScore ("./" <> title <> ".ly") $ Score "no comment" voices -- (voices <> ghostVoices)
  where
    pipeline :: Tempo -> NE.NonEmpty SwirlsTup -> [[NoteOrRest]] -> NE.NonEmpty Voice
    pipeline tempo tups nOrRss = -- Assumes: TimeSignature, Instrument, Key are the same for all Voice
      zipWith alignNoteOrRestsDurations (NE.toList timeSigs) nOrRss      -- --> [[NoteOrRest]]
      & zipWith splitNoteOrRests winLens                                 -- --> [([VoiceEvent],[VoiceEvent])]
      & zipWith tagFirstNotes firstNotes                                 -- --> [([VoiceEvent],[VoiceEvent])]
      & zipWith3 (mkTotDur (maximum veLens)) veLens (NE.toList timeSigs) -- --> [([VoiceEvent],[VoiceEvent])]
      & NE.fromList . (<$>) (bimap NE.fromList NE.fromList)              -- --> NonEmpty (NonEmpty VoiceEvent,NonEmpty VoiceEvent)
      & neZipWith4 genPolyVocs instrs keys timeSigs                      -- --> NonEmpty Voice
      & tagTempo tempo                                                   -- --> NonEmpty Voice
      where
        veLens     = nOrRs2DurVal <$> nOrRss
        timeSigs   = _stTime  <$> tups
        instrs     = _stInstr <$> tups
        keys       = _stKey   <$> tups
        cntVoices  = length nOrRss
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
