{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (
    main
  ) where

import Control.Monad (unless, void)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (ReaderT(runReaderT))
import Data.Aeson (Value(Null))
import qualified Data.List.NonEmpty as NE
import GHC.Generics (Generic)
import System.Environment (getEnv)
import System.Exit (ExitCode(ExitSuccess))
import System.Process (readProcessWithExitCode)
import System.Random (getStdGen)
import Test.QuickCheck
       ( Arbitrary(..)
       , Gen
       , arbitrarySizedNatural
       , elements
       , genericShrink
       , listOf1
       , oneof
       , resize
       )
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Test.Tasty.QuickCheck (testProperty)

import Driver (Driver, initEnv, runDriver, writeScore)
import Lily (FromLily(..), ToLily(..))
import Types
import Utils (durSum2Durs, sumDurs, duration2DurationVal)

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
  ,testCase     "parseLily . toLily PitchedVoice"    (assertParseLilytoLilyVal pitchedVoice)
  ,testCase     "parseLily . toLily KeyboardVoice"   (assertParseLilytoLilyVal keyboardVoice)
  ,testCase     "parseLily . toLily PolyVoice"       (assertParseLilytoLilyVal polyVoice)
  ,testCase     "parseLily . toLily SplitStaffVoice" (assertParseLilytoLilyVal splitStaffVoice)
  ,testCase     "parseLily . toLily VoiceGroup"      (assertParseLilytoLilyVal voiceGroup)
  --
  ,testCase     "parseLily . toLily min score"         (assertParseLilytoLilyVal minScore)
  ,testCase     "parseLily . toLily multi score"       (assertParseLilytoLilyVal multiScore)
  ,testCase     "parseLily . toLily poly score"        (assertParseLilytoLilyVal polyScore)
  ,testCase     "parseLily . toLily keybord score"     (assertParseLilytoLilyVal keyboardScore)
  ,testCase     "parseLily . toLily split staff score" (assertParseLilytoLilyVal splitStaffScore)
  ,testCase     "parseLily . toLily group score"       (assertParseLilytoLilyVal groupScore)
  
  ,testCase     "single-voice score"   (testLilypond "single-voice.ly" minScore)
  ,testCase     "multi-voice score"    (testLilypond "multi-voice.ly" multiScore)
  ,testCase     "poly-voice score"     (testLilypond "poly-voice.ly" polyScore)
  ,testCase     "keyboard-voice score" (testLilypond "keyboard-voice.ly" keyboardScore)
  ,testCase     "split-staff score"    (testLilypond "split-staff-voice.ly" splitStaffScore)
  ,testCase     "group-voice score"    (testLilypond "group-voice.ly" groupScore)
  ]


deriving instance Generic Accent
deriving instance Generic Chord
deriving instance Generic Tuplet
deriving instance Generic Tremolo
deriving instance Generic Duration
deriving instance Generic Dynamic
deriving instance Generic Swell
deriving instance Generic Sustain
deriving instance Generic Sostenuto
deriving instance Generic Slur
deriving instance Generic KeySignature
deriving instance Generic Mode
deriving instance Generic Note
deriving instance Generic Octave
deriving instance Generic Pitch
deriving instance Generic Rest
deriving instance Generic TimeSignature
deriving instance Generic PitOct

--
-- Select from enum
--
instance Arbitrary Duration where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary DurationVal where
  arbitrary = duration2DurationVal <$> elements [WDur, HDur, QDur, EDur, SDur, SFDur, HTEDur]

instance Arbitrary Octave where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Accent where
  arbitrary = elements [Marcato .. Portato]
  shrink = genericShrink

instance Arbitrary Dynamic where
  arbitrary = elements [PP .. RFZ]
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

instance Arbitrary Sustain where
  arbitrary = genericArbitrary
  shrink = genericShrink
  
instance Arbitrary Sostenuto where
  arbitrary = genericArbitrary
  shrink = genericShrink
  
instance Arbitrary Slur where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Control where
  arbitrary = oneof [CtrlAccent     <$> arbitrary 
                    ,CtrlDynamic    <$> arbitrary
                    ,CtrlSwell      <$> arbitrary
                    ,CtrlSustain    <$> arbitrary
                    ,CtrlSostenuto  <$> arbitrary
                    ,CtrlSlur       <$> arbitrary
                    ,CtrlAnnotation <$> elements ["a","b","c"]]

instance Arbitrary MidiControl where
  arbitrary = oneof [MidiCtrlAccent <$> arbitrary 
                    ,MidiCtrlDynamic <$> arbitrary]

--
-- Select one from a collection of arbitrary enums
--
instance Arbitrary KeySignature where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Note where
  arbitrary = Note <$> arbitrary <*> arbitrary <*> arbitrary <*> listOfThree arbitrary <*> listOfThree arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary Rest where
  arbitrary = Rest <$> arbitrary <*> listOfThree arbitrary
  shrink = genericShrink

--
-- Special-purpose arbitrary
--
fixedList :: Int -> Gen a -> Gen [a]
fixedList n = resize n . listOf1

-- make lists of maximum length 3
listOfThree :: Gen a -> Gen [a]
listOfThree = fixedList 3

-- required for genericShrink in Arbitrary Note, Rest, and Chord
instance Arbitrary (NE.NonEmpty Accent) where
  arbitrary = genericArbitrary

-- required for genericShrink in Arbitrary Chord
instance Arbitrary PitOct where
  arbitrary = PitOct <$> arbitrary <*> arbitrary
  shrink = genericShrink

-- chord has non-empty list of (pitch,octave) pairs plus duration, accent, dynamic, swell, and slur
instance Arbitrary Chord where
  arbitrary = Chord . NE.fromList <$> listOfThree arbitrary <*> arbitrary <*> pure [] <*> pure [] <*> arbitrary

instance Arbitrary Tuplet where
  arbitrary = do
    arbGtr :: Bool <- arbitrary --  > tuple num / denum, e.g. 1 vs. < 1, e.g. True => N + 1 / N, False => N / N + 1
    arbNum :: Int <- elements [3..7] -- tuples from [4 in the time of 3 | 3 in the time of 4 ..  7 in the time of 6 | 6 in the time of 7]
    note <- Note <$> arbitrary <*> arbitrary <*>  pure (duration2DurationVal dur) <*> pure [] <*>  pure [] <*> arbitrary
    let num = if arbGtr then arbNum else arbNum - 1
        den = if arbGtr then arbNum - 1 else arbNum
        notes = NE.fromList $ replicate num note
    pure $ Tuplet num den dur (NE.map VeNote notes)
    where
      dur = QDur

instance Arbitrary Tempo where
  arbitrary = oneof [TempoText <$> elements ["Presto","Vivace","Meno Mosso"]
                    ,TempoDur <$> arbitrary <*> arbitrarySizedNatural
                    ,TempoLong <$> elements ["Presto","Vivace","Meno Mosso", ""] <*> arbitrary <*> arbitrarySizedNatural
                    ,TempoRange <$> arbitrary <*> arbitrarySizedNatural <*> arbitrarySizedNatural
                    ]

instance Arbitrary (NE.NonEmpty Int) where
  arbitrary = NE.fromList <$> listOfThree arbitrarySizedNatural
  shrink = genericShrink

instance Arbitrary TimeSignature where
  arbitrary = oneof [TimeSignatureSimple <$> elements [1..10] <*> elements [WDur, HDur, QDur, EDur, SDur, SFDur, HTEDur]
                    ,TimeSignatureGrouping <$> arbitrary <*> elements [1..10] <*> elements [WDur, HDur, QDur, EDur, SDur, SFDur, HTEDur]
                    ]

-- Duration can't be shorter than quarter note.  Two Durations in ChordTremolo should be the same.
instance Arbitrary Tremolo where
  arbitrary = oneof [NoteTremolo <$> arbNote, arbChordTremolo]
    where
      arbNote = Note <$> arbitrary <*> arbitrary <*> (duration2DurationVal <$> elements [DWDur, WDur, DHDur, HDur, DQDur, QDur]) <*>  pure [] <*> pure [] <*> arbitrary
      arbChordTremolo = do
        dur <- elements [DWDur, WDur, DHDur, HDur, DQDur, QDur]
        arbChord1 <- (`Chord` duration2DurationVal dur) . NE.fromList <$> listOfThree arbitrary <*> pure [] <*> pure [] <*> arbitrary
        arbChord2 <- (`Chord` duration2DurationVal dur) . NE.fromList <$> listOfThree arbitrary <*> pure [] <*> pure [] <*> arbitrary
        pure $ ChordTremolo arbChord1 arbChord2
  shrink = genericShrink

minVEvents :: NE.NonEmpty VoiceEvent
minVEvents = VeClef Treble NE.:|
             [VeTempo (TempoDur QDur 120)
             ,VeTimeSignature (TimeSignatureSimple 4 QDur)
             ,VeNote (Note C COct (duration2DurationVal QDur) [] [CtrlAccent Marcato,CtrlAnnotation "a",CtrlSlur SlurOn] False)
             ,VeNote (Note G COct (duration2DurationVal QDur) [] [CtrlDynamic Forte,CtrlAnnotation "b.",CtrlSlur SlurOff] False)
             ,VeNote (Note C COct (duration2DurationVal QDur) [] [CtrlAnnotation "C!"] False)
             ,VeTremolo (NoteTremolo (Note C COct (duration2DurationVal QDur) [] [] False))]

pitchedVoice :: Voice
pitchedVoice = PitchedVoice AcousticGrand minVEvents

keyboardVoice :: Voice
keyboardVoice = KeyboardVoice AcousticGrand (minVEvents,minVEvents)

polyVoice :: Voice
polyVoice = PolyVoice AcousticGrand (minVEvents NE.:| [minVEvents])

splitStaffVoice :: Voice
splitStaffVoice = SplitStaffVoice AcousticGrand minVEvents

voiceGroup :: Voice
voiceGroup = VoiceGroup (pitchedVoice NE.:| [pitchedVoice, polyVoice])

minScore :: Score
minScore = Score "min" "comment" (pitchedVoice NE.:| [])

multiScore :: Score
multiScore = Score "multi" "comment" (pitchedVoice NE.:| [pitchedVoice])

polyScore :: Score
polyScore = Score "poly" "comment" (polyVoice NE.:| [])

keyboardScore :: Score
keyboardScore = Score "keyboard" "comment" (keyboardVoice NE.:| [])

splitStaffScore :: Score
splitStaffScore = Score "split staff" "comment" (splitStaffVoice NE.:| [])

groupScore :: Score
groupScore = Score "group" "comment" (voiceGroup NE.:| [voiceGroup])

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
