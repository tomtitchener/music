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
  tups <- cfg2SwirlsTups (title <> ".section1") voicesNames
  noteOrRestss <- traverse swirlsTup2NoteOrRests tups <&> NE.toList
  let veLens        = nOrRs2DurVal <$> noteOrRestss
      noteTags      = replicate cntVoices (Note C COct EDur (singleton Staccatissimo) PPP NoSwell False)
      -- regular voices, first apportion durations by position in beat and bar
      winLens       = replicate cntVoices 5 -- tbd: magic constant
      voices        = zipWith alignNoteOrRestsDurations (NE.toList (_stTime <$> tups)) noteOrRestss
                      & pipeline tempo noteTags veLens tups winLens
      -- ghost voices
      -- timeSigs      = NE.toList (_stTime <$> tups)
      -- manyIntPrss   = cycle <$> nes2arrs (_stGhosts <$> tups)
      -- gWinLens      = replicate cntVoices 1 -- tbd: magic constant
      -- gVoices       = zipWith3 squashNoteOrRests manyIntPrss timeSigs noteOrRestss
      --                 &  pipeline tempo noteTags veLens tups gWinLens
  writeScore ("./" <> title <> ".ly") $ Score "no comment" voices -- (voices <> ghostVoices)
  where
    pipeline :: Tempo -> [Note] -> [Int] -> NE.NonEmpty SwirlsTup -> [Int] -> [[NoteOrRest]] -> NE.NonEmpty Voice
    pipeline tempo noteTags veLens tups winLens nOrRss =
      zipWith splitNoteOrRests winLens nOrRss
      & zipWith tagFirstNotes noteTags
      & zipWith3 (mkTotDur (maximum veLens)) veLens (NE.toList (_stTime <$> tups))
      & NE.fromList . (<$>) (bimap NE.fromList NE.fromList)
      & neZipWith4 genPolyVocs (_stInstr <$> tups) (_stKey <$> tups) (_stTime <$> tups)
      & tagTempo tempo
