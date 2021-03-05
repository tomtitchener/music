{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (
    main
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Aeson
import qualified Data.List.NonEmpty as NE
import GHC.Generics
import System.Environment
import System.Exit
import System.Process
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Driver
import Lily
import Types
import Utils

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Tests"
  [testProperty "parseLily . toLily Octave == id" (propParseLilytoLilyVal @Octave)
  ,testProperty "parseLily . toLily Duration == id" (propParseLilytoLilyVal @Duration)
  ,testProperty "durSum preserves input" propDurSum2Durs
  ,testProperty "parseLily . toLily Pitch == id" (propParseLilytoLilyVal @Pitch)
  ,testProperty "parseLily . toLily Accent == id" (propParseLilytoLilyVal @Accent)
  ,testProperty "parseLily . toLily Dynamic == id" (propParseLilytoLilyVal @Dynamic)
  ,testProperty "parseLily . toLily Swell == id" (propParseLilytoLilyVal @Swell)
  ,testProperty "parseLily . toLily Note == id" (propParseLilytoLilyVal @Note)
  ,testProperty "parseLily . toLily Rest == id" (propParseLilytoLilyVal @Rest)
  ,testProperty "parseLily . toLily Mode == id" (propParseLilytoLilyVal @Mode)
  ,testProperty "parseLily . toLily KeySignature == id" (propParseLilytoLilyVal @KeySignature)
  ,testProperty "parseLily . toLily Tempo == id" (propParseLilytoLilyVal @Tempo)
  ,testProperty "parseLily . toLily TimeSignature == id" (propParseLilytoLilyVal @TimeSignature)
  ,testProperty "parseLily . toLily Chord == id" (propParseLilytoLilyVal @Chord)
  ,testProperty "parseLily . toLily Tuplet == id" (propParseLilytoLilyVal @Tuplet)
  ,testProperty "parseLily . toLily Tremolo == id" (propParseLilytoLilyVal @Tremolo)
  --
  ,testCase     "parseLily . toLily PitchedVoice" (assertParseLilytoLilyVal pitchedVoice)
  ,testCase     "parseLily . toLily PolyVoice"   (assertParseLilytoLilyVal polyVoice)
  ,testCase     "parseLily . toLily VoiceGroup"  (assertParseLilytoLilyVal voiceGroup)
  --
  ,testCase     "parseLily . toLily min score"   (assertParseLilytoLilyVal minScore)
  ,testCase     "parseLily . toLily multi score" (assertParseLilytoLilyVal multiScore)
  ,testCase     "parseLily . toLily poly score"  (assertParseLilytoLilyVal polyScore)
  ,testCase     "parseLily . toLily group score" (assertParseLilytoLilyVal groupScore)
  --
  ,testCase     "single-voice score" (testLilypond "single-voice.ly" minScore)
  ,testCase     "multi-voice score"  (testLilypond "multi-voice.ly" multiScore)
  ,testCase     "poly-voice score"   (testLilypond "poly-voice.ly" polyScore)
  ,testCase     "group-voice score"  (testLilypond "group-voice.ly" groupScore)
  ]


deriving instance Generic Accent
deriving instance Generic Chord
deriving instance Generic Tuplet
deriving instance Generic Tremolo
deriving instance Generic Duration
deriving instance Generic Dynamic
deriving instance Generic Swell
deriving instance Generic KeySignature
deriving instance Generic Mode
deriving instance Generic Note
deriving instance Generic Octave
deriving instance Generic Pitch
deriving instance Generic Rest
deriving instance Generic TimeSignature

--
-- Select one from enum
--
instance Arbitrary Duration where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Octave where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Accent where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Dynamic where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Swell where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Mode where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Pitch where
  arbitrary = genericArbitrary
  shrink = genericShrink

--
-- Select one from a collection of arbitrary enums
--
instance Arbitrary KeySignature where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Note where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Rest where
  arbitrary = genericArbitrary
  shrink = genericShrink

--
-- Special-purpose arbitrary
--
fixedList :: Int -> Gen a -> Gen [a]
fixedList n = resize n . listOf1

-- make lists of maximum length 3
listOfThree :: Gen a -> Gen [a]
listOfThree = fixedList 3

-- required for genericShrink in Arbitrary Chord
instance Arbitrary (NE.NonEmpty (Pitch,Octave)) where
  arbitrary = genericArbitrary

instance Arbitrary Chord where
  arbitrary = Chord . NE.fromList <$> listOfThree arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary Tuplet where
  arbitrary = do
    arbGtr :: Bool <- arbitrary --  > tuple num / denum, e.g. 1 vs. < 1, e.g. True => N + 1 / N, False => N / N + 1
    arbNum :: Int <- elements [3..7] -- tuples from [4 in the time of 3 | 3 in the time of 4 ..  7 in the time of 6 | 6 in the time of 7]
    note <- Note <$> arbitrary <*> arbitrary <*> pure dur <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    let notes = NE.fromList $ replicate arbNum note
    if arbGtr
    then pure $ Tuplet arbNum (arbNum - 1) dur notes
    else pure $ Tuplet (arbNum -1) arbNum dur notes
    where
      dur = QDur

instance Arbitrary Tempo where
  arbitrary = oneof [TempoText <$> elements ["Presto","Vivace","Meno Mosso"]
                    ,TempoDur <$> arbitrary <*> arbitrarySizedNatural
                    ,TempoLong <$> elements ["Presto","Vivace","Meno Mosso", ""] <*> arbitrary <*> arbitrarySizedNatural
                    ,TempoRange <$> arbitrary <*> arbitrarySizedNatural <*> arbitrarySizedNatural
                    ]

-- required for genericShrink in Arbitrary TimeSignature
instance Arbitrary (NE.NonEmpty Int) where
  arbitrary = genericArbitrary

instance Arbitrary TimeSignature where
  arbitrary = TimeSignature <$> elements [1..10] <*> elements [WDur, HDur, QDur, EDur, SDur, SFDur, HTEDur]
  shrink = genericShrink

-- Duration can't be shorter than quarter note.  Two Durations in ChordTremolo should be the same.
instance Arbitrary Tremolo where
  arbitrary = oneof [NoteTremolo <$> arbNote, arbChordTremolo]
    where
      arbNote = Note <$> arbitrary <*> arbitrary <*> elements [DWDur, WDur, DHDur, HDur, DQDur, QDur] <*>  arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      arbChordTremolo = do
        dur <- elements [DWDur, WDur, DHDur, HDur, DQDur, QDur]
        arbChord1 <- (`Chord` dur) . NE.fromList <$> listOfThree arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        arbChord2 <- (`Chord` dur) . NE.fromList <$> listOfThree arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        pure $ ChordTremolo arbChord1 arbChord2
  shrink = genericShrink

minVEvents :: NE.NonEmpty VoiceEvent
minVEvents = VeClef Treble NE.:|
             [VeTempo (TempoDur QDur 120)
             ,VeTimeSignature (TimeSignature 4 QDur)
             ,VeNote (Note C COct QDur Staccato Forte NoSwell False)
             ,VeNote (Note G COct QDur NoAccent NoDynamic NoSwell False)
             ,VeNote (Note C COct QDur NoAccent NoDynamic NoSwell False)
             ,VeTremolo (NoteTremolo (Note C COct QDur NoAccent NoDynamic NoSwell False))]

pitchedVoice :: Voice
pitchedVoice = PitchedVoice AcousticGrand minVEvents

polyVoice :: Voice
polyVoice = PolyVoice AcousticGrand (minVEvents NE.:| [minVEvents])

voiceGroup :: Voice
voiceGroup = VoiceGroup (pitchedVoice NE.:| [pitchedVoice, polyVoice])

minScore :: Score
minScore = Score "comment" (pitchedVoice NE.:| [])

multiScore :: Score
multiScore = Score "comment" (pitchedVoice NE.:| [pitchedVoice])

polyScore :: Score
polyScore = Score "comment" (polyVoice NE.:| [])

groupScore :: Score
groupScore = Score "comment" (voiceGroup NE.:| [voiceGroup])

--
-- test routines
--
propParseLilytoLilyVal :: (Eq a, ToLily a, FromLily a) => a -> Bool
propParseLilytoLilyVal v = v == (parseLily . toLily) v

propDurSum2Durs :: [Duration] -> Bool
propDurSum2Durs durs = sumDurs durs == (sumDurs . durSum2Durs . sumDurs) durs

assertParseLilytoLilyVal :: (Show a, Eq a, ToLily a, FromLily a) => a -> Assertion
assertParseLilytoLilyVal a = assertEqual (show a) a (parseLily (toLily a))

runTestDriver :: MonadIO m => Driver a -> m a
runTestDriver action = liftIO $ getStdGen >>= runReaderT (runDriver action) . initEnv Null . show

testLilypond :: FilePath -> Score -> Assertion
testLilypond path score = do
  home <- liftIO $ getEnv "HOME"
  void $ runTestDriver (writeScore ("test/"<>path) score)
  (code, _, stderr) <- readProcessWithExitCode (home <> "/bin/lilypond") ["-s","-o","test", "test/"<>path] ""
  unless (ExitSuccess == code) (putStr $ "\n" <> stderr)
  assertEqual "lilypond exit code" ExitSuccess code


