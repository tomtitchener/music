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
import Control.Monad.Trans.State
import Data.Aeson
import GHC.Generics
import System.Exit
import System.Process
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
  ,testProperty "parseLily . toLily Note == id" (propParseLilytoLilyVal @Note)
  ,testProperty "parseLily . toLily Rest == id" (propParseLilytoLilyVal @Rest)
  ,testProperty "parseLily . toLily Mode == id" (propParseLilytoLilyVal @Mode)
  ,testProperty "parseLily . toLily KeySignature == id" (propParseLilytoLilyVal @KeySignature)
  ,testProperty "parseLily . toLily Tempo == id" (propParseLilytoLilyVal @Tempo)
  ,testProperty "parseLily . toLily TimeSignature == id" (propParseLilytoLilyVal @TimeSignature)
  ,testProperty "parseLily . toLily Chord == id" (propParseLilytoLilyVal @Chord)
  --
  ,testCase     "parseLily . toLily SingleVoice" (assertParseLilytoLilyVal singleVoice)
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
deriving instance Generic Duration
deriving instance Generic Dynamic
deriving instance Generic KeySignature
deriving instance Generic Mode
deriving instance Generic Note
deriving instance Generic Octave
deriving instance Generic Pitch
deriving instance Generic Rest
deriving instance Generic TimeSignature

instance Arbitrary Duration where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Note where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Rest where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Chord where
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

instance Arbitrary Pitch where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Mode where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary KeySignature where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Tempo where
  arbitrary = oneof [TempoText <$> elements ["Presto","Vivace","Meno Mosso"]
                    ,TempoDur <$> arbitrary <*> arbitrarySizedNatural
                    ,TempoLong <$> elements ["Presto","Vivace","Meno Mosso", ""] <*> arbitrary <*> arbitrarySizedNatural
                    ,TempoRange <$> arbitrary <*> arbitrarySizedNatural <*> arbitrarySizedNatural
                    ]

instance Arbitrary TimeSignature where
  arbitrary = TimeSignature <$> elements [1..10] <*> elements [WDur, HDur, QDur, EDur, SDur, SFDur, HTEDur]
  shrink = genericShrink

propParseLilytoLilyVal :: (Eq a, ToLily a, FromLily a) => a -> Bool
propParseLilytoLilyVal v = v == (parseLily . toLily) v

propDurSum2Durs :: [Duration] -> Bool
propDurSum2Durs durs = sumDurs durs == (sumDurs . durSum2Durs . sumDurs) durs

runTestDriver :: MonadIO m => Driver a -> m DriverState
runTestDriver action = liftIO $ execStateT (runReaderT (runDriver action) (initEnv Null)) initState

minVEvents :: [VoiceEvent]
minVEvents = [VeClef Treble
             ,VeTempo (TempoDur QDur 120)
             ,VeTimeSignature (TimeSignature 4 QDur)
             ,VeNote (Note C COct QDur Staccato Forte False)
             ,VeNote (Note G COct QDur NoAccent NoDynamic False)
             ,VeNote (Note C COct QDur NoAccent NoDynamic False)]

assertParseLilytoLilyVal :: (Show a, Eq a, ToLily a, FromLily a) => a -> Assertion
assertParseLilytoLilyVal a = assertEqual "" a (parseLily (toLily a))

singleVoice :: Voice
singleVoice = SingleVoice AcousticGrand minVEvents

polyVoice :: Voice
polyVoice = PolyVoice AcousticGrand [minVEvents,minVEvents]

voiceGroup :: Voice
voiceGroup = VoiceGroup [singleVoice, singleVoice, polyVoice]

minScore :: Score
minScore = Score "comment" [singleVoice]

multiScore :: Score
multiScore = Score "comment" [singleVoice,singleVoice]

polyScore :: Score
polyScore = Score "comment" [polyVoice]

groupScore :: Score
groupScore = Score "comment" [voiceGroup, voiceGroup]

testLilypond :: FilePath -> Score -> Assertion
testLilypond path score = do
  void $ runTestDriver (writeLily ("test/"<>path) score)
  (code, _, stderr) <- readProcessWithExitCode "lilypond" ["-s","-o","test", "test/"<>path] ""
  unless (ExitSuccess == code) (putStr $ "\n" <> stderr)
  assertEqual "lilypond exit code" ExitSuccess code

