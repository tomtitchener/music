{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (
    main
  ) where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Accent as Acc
import qualified Chord
import qualified Duration as Dur
import qualified Dynamic as Dyn
import GHC.Generics
import qualified KeySignature as KeySig
import qualified Mode as Mod
import qualified Note
import qualified Octave as Oct
import qualified Pitch as Pit
import qualified Rest
import qualified Tempo as Temp
import qualified TimeSignature as TSig

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Tests"
  [testProperty "toLily parseLily Octave == id" propParseLilytoLilyOct
  ,testProperty "parseLily . toLily Octave == id" propToLilyparseLilyOct
  ,testProperty "toLily parseLily Duration == id" propParseLilytoLilyDur
  ,testProperty "parseLily . toLily Duration == id" propToLilyparseLilyDur
  ,testProperty "durSum preserves input" propDurSum2Durs
  ,testProperty "parseLily . toLily Pitch == id" propToLilyparseLilyPit
  ,testProperty "toLily parseLily Pitch == id" propParseLilytoLilyPit
  ,testProperty "parseLily . toLily Accent == id" propToLilyparseLilyAcc
  ,testProperty "toLily parseLily Accent == id" propParseLilytoLilyAcc
  ,testProperty "parseLily . toLily Dynamic == id" propToLilyparseLilyDyn
  ,testProperty "toLily parseLily Dynamic == id" propParseLilytoLilyDyn
  ,testProperty "parseLily . toLily Note == id" propParseLilytoLilyNote
  ,testProperty "toLily parseLily Note == id" propToLilyparseLilyNote
  ,testProperty "parseLily . toLily Rest == id" propParseLilytoLilyRest
  ,testProperty "toLily parseLily Rest == id" propToLilyparseLilyRest
  ,testProperty "parseLily . toLily Mode == id" propParseLilytoLilyMode
  ,testProperty "toLily parseLily Mode == id" propToLilyparseLilyMode
  ,testProperty "parseLily . toLily KeySignature == id" propParseLilytoLilyKeySig
  ,testProperty "toLily parseLily KeySignature == id" propToLilyparseLilyKeySig
  ,testProperty "parseLily . toLily Tempo == id" propParseLilytoLilyTemp
  ,testProperty "toLily parseLily Tempo == id" propToLilyparseLilyTemp
  ,testProperty "parseLily . toLily TimeSignature == id" propParseLilytoLilyTSig
  ,testProperty "toLily parseLily TimeSignature == id" propToLilyparseLilyTSig
  ,testProperty "parseLily . toLily Chord == id" propParseLilytoLilyChord
  ,testProperty "toLily parseLily Chord == id" propToLilyparseLilyChord
  ]

deriving instance Generic Acc.Accent

deriving instance Generic Chord.Chord

deriving instance Generic Dur.Duration

deriving instance Generic Dyn.Dynamic

deriving instance Generic KeySig.KeySignature

deriving instance Generic Mod.Mode

deriving instance Generic Note.Note

deriving instance Generic Oct.Octave

deriving instance Generic Pit.Pitch

deriving instance Generic Rest.Rest

deriving instance Generic TSig.TimeSignature

instance Arbitrary Dur.Duration where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Note.Note where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Rest.Rest where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Chord.Chord where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Oct.Octave where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Acc.Accent where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Dyn.Dynamic where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Pit.Pitch where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Mod.Mode where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary KeySig.KeySignature where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Temp.Tempo where
  arbitrary = oneof [Temp.TempoText <$> elements ["Presto","Vivace","Meno Mosso"]
                    ,Temp.Tempo <$> arbitrary <*> arbitrarySizedNatural
                    ,Temp.TempoLong <$> elements ["Presto","Vivace","Meno Mosso", ""] <*> arbitrary <*> arbitrarySizedNatural
                    ,Temp.TempoRange <$> arbitrary <*> arbitrarySizedNatural <*> arbitrarySizedNatural
                    ]

instance Arbitrary TSig.TimeSignature where
  arbitrary = liftM2 TSig.TimeSignature (elements [1..10]) (elements Dur.integralDurations)
  shrink = genericShrink

propParseLilytoLilyOct :: Oct.Octave -> Property
propParseLilytoLilyOct o = True ==> o == (Oct.parseLily . Oct.toLily) o

propToLilyparseLilyOct :: Property
propToLilyparseLilyOct = forAll (elements Oct.lilySyms) (\s -> s == Oct.toLily (Oct.parseLily s::Oct.Octave))

propParseLilytoLilyDur :: Dur.Duration -> Property
propParseLilytoLilyDur d = True ==> d == (Dur.parseLily . Dur.toLily) d

propToLilyparseLilyDur :: Property
propToLilyparseLilyDur = forAll (elements Dur.lilySyms) (\s -> s == Dur.toLily (Dur.parseLily s::Dur.Duration))

propDurSum2Durs :: [Dur.Duration] -> Bool
propDurSum2Durs durs = Dur.sumDurs durs == (Dur.sumDurs . Dur.durSum2Durs . Dur.sumDurs) durs

propParseLilytoLilyPit :: Pit.Pitch -> Property
propParseLilytoLilyPit p = True ==> p == (Pit.parseLily . Pit.toLily) p

propToLilyparseLilyPit :: Property
propToLilyparseLilyPit = forAll (elements Pit.lilySyms) (\s -> s == Pit.toLily (Pit.parseLily s::Pit.Pitch))

propParseLilytoLilyAcc :: Acc.Accent -> Property
propParseLilytoLilyAcc a = True ==> a == (Acc.parseLily . Acc.toLily) a

propToLilyparseLilyAcc :: Property
propToLilyparseLilyAcc = forAll (elements Acc.lilySyms) (\s -> s == Acc.toLily (Acc.parseLily s::Acc.Accent))

propParseLilytoLilyDyn :: Dyn.Dynamic -> Property
propParseLilytoLilyDyn p = True ==> p == (Dyn.parseLily . Dyn.toLily) p

propToLilyparseLilyDyn :: Property
propToLilyparseLilyDyn = forAll (elements Dyn.lilySyms) (\s -> s == Dyn.toLily (Dyn.parseLily s::Dyn.Dynamic))

propParseLilytoLilyNote :: Note.Note -> Property
propParseLilytoLilyNote n = True ==> n == (Note.parseLily . Note.toLily) n

propToLilyparseLilyNote :: Property
propToLilyparseLilyNote = forAll
                          (elements [ pit <> oct <> dur <> dyn <> acc <> slr |
                                      pit <- Pit.lilySyms, oct <- Oct.lilySyms, dur <-  Dur.lilySyms, dyn <- Dyn.lilySyms, acc <- Acc.lilySyms, slr <- ["", "~"]])
                          (\s -> s == Note.toLily (Note.parseLily s::Note.Note))

propParseLilytoLilyRest :: Rest.Rest -> Property
propParseLilytoLilyRest n = True ==> n == (Rest.parseLily . Rest.toLily) n

propToLilyparseLilyRest :: Property
propToLilyparseLilyRest = forAll
                          (elements [ "r" <> dur | dur <-  Dur.lilySyms])
                          (\s -> s == Rest.toLily (Rest.parseLily s::Rest.Rest))

propParseLilytoLilyChord :: Chord.Chord -> Property
propParseLilytoLilyChord c = True ==> c == (Chord.parseLily . Chord.toLily) c

propToLilyparseLilyChord :: Property
propToLilyparseLilyChord = forAll
                           (elements [ "<" <> pit <> oct <> " " <> pit' <> oct' <> ">" <> dur <> dyn <> acc <> slr |
                                       pit <- take 4 Pit.lilySyms, oct <- take 4 Oct.lilySyms, -- prevent comb. explosion
                                       pit' <- take 4 Pit.lilySyms, oct' <- take 4 Oct.lilySyms, -- prevent comb. explosion
                                       dur <-  Dur.lilySyms, dyn <- Dyn.lilySyms, acc <- Acc.lilySyms, slr <- ["", "~"]])
                           (\c -> c == Chord.toLily (Chord.parseLily c::Chord.Chord))

propParseLilytoLilyMode :: Mod.Mode -> Property
propParseLilytoLilyMode m = True ==> m == (Mod.parseLily . Mod.toLily) m

propToLilyparseLilyMode :: Property
propToLilyparseLilyMode = forAll (elements Mod.lilySyms) (\m -> m == Mod.toLily (Mod.parseLily m::Mod.Mode))

propParseLilytoLilyKeySig :: KeySig.KeySignature -> Property
propParseLilytoLilyKeySig ks = True ==> ks == (KeySig.parseLily . KeySig.toLily) ks

propToLilyparseLilyKeySig :: Property
propToLilyparseLilyKeySig = forAll
                          (elements [ "\\key " <> pit <> " " <> mode | pit <- Pit.lilySyms, mode <- Mod.lilySyms])
                          (\s -> s == KeySig.toLily (KeySig.parseLily s::KeySig.KeySignature))

propParseLilytoLilyTemp :: Temp.Tempo -> Property
propParseLilytoLilyTemp t = True ==> t == (Temp.parseLily . Temp.toLily) t

propToLilyparseLilyTemp :: Property
propToLilyparseLilyTemp = forAll
                          (elements [ "\\tempo " <> i <> " = " <> dur | i <- ["1","20","5","7","128"], dur <- Dur.integralDurationSyms])
                          (\s -> s == Temp.toLily (Temp.parseLily s::Temp.Tempo))

propParseLilytoLilyTSig :: TSig.TimeSignature -> Property
propParseLilytoLilyTSig t = True ==> t == (TSig.parseLily . TSig.toLily) t

propToLilyparseLilyTSig :: Property
propToLilyparseLilyTSig = forAll
                          (elements [ "\\time " <> i <> "/" <> dur | i <- ["1","2","3","4","5","6","7","8","9"], dur <- Dur.integralDurationSyms])
                          (\s -> s == TSig.toLily (TSig.parseLily s::TSig.TimeSignature))

{--
--import Test.QuickCheck.Classes
--import Test.QuickCheck.Property

-- From https://hackage.haskell.org/package/quickcheck-classes-0.6.4.0/docs/src/Test.QuickCheck.Classes.Json.html#jsonLaws :: jsonEncodingPartialIsomorphism
lilyEncodingPartialIsomorphism :: forall a. (ToLily a, FromLily a, Show a, Eq a, Arbitrary a) => Proxy a -> Property
lilyEncodingPartialIsomorphism _ =
  MkProperty $
    arbitrary >>= \(x :: a) ->
      unProperty $
      shrinking shrink x $ \x' ->
        let desc1 = "Just"
            desc2 = "Lily.parseLily . Lily.toLily"
            name1 = "Lily.toLily a"
            name2 = "Lily.parseLily (Lily.toLily a)"
            b1  = toLily x'
            b2  = parseLily (toLily x')::a
            sb1 = show b1
            sb2 = show b2
            description = "  Description: " ++ desc1 ++ " == " ++ desc2
            err = description ++ "\n" ++ unlines (map ("  " ++) ["a = " ++ show x']) ++ "  " ++ name1 ++ " = " ++ sb1 ++ "\n  " ++ name2 ++ " = " ++ sb2
        in counterexample err (x' == b2)

lilyLaws :: (ToLily a, FromLily a, Show a, Arbitrary a, Eq a) => Proxy a -> Laws
lilyLaws p = Laws "ToLily/FromLily"
  [ ("Partial Isomorphism", lilyEncodingPartialIsomorphism p)
  ]
--}
