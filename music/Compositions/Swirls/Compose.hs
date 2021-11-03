{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Compositions.Swirls.Compose (cfg2SwirlsScore) where


import Control.Applicative
import Control.Lens (Bifunctor(bimap), (&), (<&>))
import qualified Data.List.NonEmpty as NE
import Data.Tuple.Extra (dupe, second, secondM)

import Driver (Driver, cfgPath2Keys, printIt, searchConfigParam, writeScore)
import Types
import Utils (neZipWith4, singleton)

import Compositions.Swirls.Utils

------------
-- Swirls --
------------

cfg2SwirlsScore :: String -> Driver ()
cfg2SwirlsScore title = do
  tempoInt :: Int <- searchConfigParam (title <> ".globals.tempo")
  let --vocNames  = NE.fromList ["voice1","voice2","voice3"]
      --cntVoices = NE.length vocNames
      tempo     = TempoDur QDur (fromIntegral tempoInt)
  sections <- cfgPath2Keys title <&> fmap ((title <> ".") <>)
  printIt sections
  secVcsPrs <- traverse (secondM cfgPath2Keys . dupe) sections
  printIt secVcsPrs
  tupss <- traverse (uncurry cfg2SwirlsTups) $ second NE.fromList <$> secVcsPrs
  printIt tupss
  nOrRsss <- traverse (\tups -> traverse swirlsTup2NoteOrRests tups <&> ZipList . NE.toList) tupss
--  s1tups <- cfg2SwirlsTups (title <> ".section1") vocNames
--  s2tups <- cfg2SwirlsTups (title <> ".section2") vocNames
--  s1NoteOrRestss <- traverse swirlsTup2NoteOrRests s1tups <&> NE.toList
--  s2NoteOrRestss <- traverse swirlsTup2NoteOrRests s2tups <&> NE.toList
  let
      -- regular voices
      nOrRss       = map concat $ getZipList $ sequenceA nOrRsss
      cntVoices    = length nOrRss
      winLens      = replicate cntVoices 5 -- tbd: magic constant
--      voices       = zipWith (<>) s1NoteOrRestss s2NoteOrRestss
--                     & pipeline tempo s1tups winLens
      voices       = pipeline tempo (head tupss) winLens nOrRss
      -- ghost voices
      -- manyIntPrss = cycle <$> nes2arrs (_stGhosts <$> tups)
      -- gWinLens    = replicate cntVoices 1 -- tbd: magic constant
      -- gVoices     = zipWith3 squashNoteOrRests manyIntPrss timeSigs noteOrRestss
      --               &  pipeline tempo s1tups gWinLens
  writeScore ("./" <> title <> ".ly") $ Score "no comment" voices -- (voices <> ghostVoices)
  where
    pipeline :: Tempo -> NE.NonEmpty SwirlsTup -> [Int] -> [[NoteOrRest]] -> NE.NonEmpty Voice
    pipeline tempo tups winLens nOrRss =
      zipWith alignNoteOrRestsDurations (NE.toList timeSigs) nOrRss
      & zipWith splitNoteOrRests winLens
      & zipWith tagFirstNotes firstNotes
      & zipWith3 (mkTotDur (maximum veLens)) veLens (NE.toList timeSigs)
      & NE.fromList . (<$>) (bimap NE.fromList NE.fromList)
      & neZipWith4 genPolyVocs instrs keys timeSigs
      & tagTempo tempo
      where
        veLens     = nOrRs2DurVal <$> nOrRss
        timeSigs   = _stTime  <$> tups
        instrs     = _stInstr <$> tups
        keys       = _stKey   <$> tups
        firstNotes = replicate (length nOrRss) firstNote
        firstNote  = Note C COct EDur (singleton Staccatissimo) PPP NoSwell False
{-
TBD:
  1) pattern search on YAML file so I can extract ["voice1","voice2",..], ["section1","section2",..] instead
     of hard-coding list of names in source
  2) initial pipeline to assemble [[NoteOrRest]] from [[SwirlsTup]] by zipWith (<>) trick but for all of [[a]]
  3) experiment with different config.yml components:
     a) mpitss:
        - long list of two or three note components, no rests
        - list of easily recognizable motifs, c-d-c-d, e-g-e-g, etc.
        - make interval arithmetic work with non-chromatic scale (does it already?)
        - experiment with per-section scales
     b) mpitss/durs/accents:
        - uniform lengths of mpitss sub-lists, lengths of durs, accents
     c) durs/accents:
        - make multiples to also choose random order among parallel to mpitss

For search, may be enough to track down all they keys for a given path, e.g.
swirls.* which would give ["globals","section1","section2"] which you could filter
out "globals".  Similarly "swirls.section1.*" for ["globals","voice1","voice2","voice3"]
for which you'd again filter out "globals".
These should just be lenses on the top-level JSON that is the config file, asking for the
key for the JSON Object at that level.
Then you'd take the list of tuples for "swirls.section1", "swirls.section2" and etc.
and combine them breadth first with (<>) with zipList.  This works:
$ (getZipList $ sequenceA ([ZipList [[1..3],[7..10]],ZipList[[4..6],[11..13]],ZipList[[6..10],[14..16]]]))
  [[[1,2,3],[4,5,6],[6,7,8,9,10]],[[7,8,9,10],[11,12,13],[14,15,16]]]
and then if you just (map concat) the result, you get concatenated sub-lists.
-}
