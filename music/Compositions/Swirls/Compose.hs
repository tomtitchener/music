{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Compositions.Swirls.Compose (cfg2SwirlsScore) where


import Control.Lens hiding (pre)
import qualified Data.List.NonEmpty as NE

import Driver (Driver, searchConfigParam, writeScore)
import Types
import Utils

import Compositions.Swirls.Utils

------------
-- Swirls --
------------

cfg2SwirlsScore :: String -> Driver ()
cfg2SwirlsScore title = do
  let voicesNames = NE.fromList ["voice1","voice2","voice3"]
      cntVoices = NE.length voicesNames
  tempo <- searchConfigParam (title <> ".globals.tempo") <&> (\(i :: Int) -> TempoDur QDur $ fromIntegral i)
  s1tups <- cfg2SwirlsTups (title <> ".section1") voicesNames
  s2tups <- cfg2SwirlsTups (title <> ".section2") voicesNames
  s1NoteOrRestss <- traverse swirlsTup2NoteOrRests s1tups <&> NE.toList
  s2NoteOrRestss <- traverse swirlsTup2NoteOrRests s2tups <&> NE.toList
  let noteOrRestss = zipWith (<>) s1NoteOrRestss s2NoteOrRestss
      timeSigs     = NE.toList (_stTime <$> s1tups)
      noteTags     = replicate cntVoices (Note C COct EDur (singleton Staccatissimo) PPP NoSwell False)
      -- regular voices
      winLens      = replicate cntVoices 5 -- tbd: magic constant
      voices       = zipWith alignNoteOrRestsDurations timeSigs noteOrRestss
                     & pipeline tempo noteTags s1tups winLens
      -- ghost voices
      -- manyIntPrss = cycle <$> nes2arrs (_stGhosts <$> tups)
      -- gWinLens    = replicate cntVoices 1 -- tbd: magic constant
      -- gVoices     = zipWith3 squashNoteOrRests manyIntPrss timeSigs noteOrRestss
      --               &  pipeline tempo noteTags tups gWinLens
  writeScore ("./" <> title <> ".ly") $ Score "no comment" voices -- (voices <> ghostVoices)
  where
    pipeline :: Tempo -> [Note] -> NE.NonEmpty SwirlsTup -> [Int] -> [[NoteOrRest]] -> NE.NonEmpty Voice
    pipeline tempo noteTags tups winLens nOrRss =
      zipWith splitNoteOrRests winLens nOrRss
      & zipWith tagFirstNotes noteTags
      & zipWith3 (mkTotDur (maximum veLens)) veLens (NE.toList timeSigs)
      & NE.fromList . (<$>) (bimap NE.fromList NE.fromList)
      & neZipWith4 genPolyVocs instrs keys timeSigs
      & tagTempo tempo
      where
        veLens   = nOrRs2DurVal <$> nOrRss
        timeSigs = _stTime <$> tups
        instrs   = _stInstr <$> tups
        keys     = _stKey <$> tups


