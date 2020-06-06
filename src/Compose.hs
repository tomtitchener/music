{-# LANGUAGE RecordWildCards #-}

module Compose (cfg2MaxRandScore
               ,cfg2MinRandScore) where

import Data.Foldable (fold)
import Data.List (zipWith4)
import Data.Semigroup (stimes)
import Driver
import Types
import Utils

data VoiceTup = VoiceTup {_vtInstr :: Instrument
                         ,_vtKey   :: KeySignature
                         ,_vtClef  :: Clef
                         ,_vtScale :: [Pitch]
                         ,_vtStart :: (Pitch,Octave)
                         } deriving (Show)

data VoiceMottos = VoiceMottos {_vmMIntss :: [[Maybe Int]]
                               ,_vmDurss  :: [[Duration]]
                               ,_vmAcctss :: [[Accent]]
                               ,_vmDynss  :: [[Dynamic]]
                               } deriving (Show)

instance Semigroup VoiceMottos where
  vm1 <> vm2 = VoiceMottos
               (_vmMIntss vm1 <> _vmMIntss vm2)
               (_vmDurss  vm1 <> _vmDurss  vm2)
               (_vmAcctss vm1 <> _vmAcctss vm2)
               (_vmDynss  vm1 <> _vmDynss  vm2)

instance Monoid VoiceMottos where
  mempty = VoiceMottos [] [] [] []

cfg2VocTup :: String -> Driver VoiceTup
cfg2VocTup pre =
  VoiceTup
    <$> getConfigParam (pre <> ".instr")
    <*> getConfigParam (pre <> ".key")
    <*> getConfigParam (pre <> ".clef")
    <*> getConfigParam (pre <> ".scale")
    <*> getConfigParam (pre <> ".start")

cfg2VocTups :: String -> [String] -> Driver [VoiceTup]
cfg2VocTups root = mapM (\v -> cfg2VocTup (root <> "." <> v))

cfg2VocMottos :: String -> Driver VoiceMottos
cfg2VocMottos title =
  VoiceMottos
    <$> getConfigParam (title <> ".intss")
    <*> getConfigParam (title <> ".durss")
    <*> getConfigParam (title <> ".accss")
    <*> getConfigParam (title <> ".dynss")

normalizeList :: a -> Int -> [a] -> [a]
normalizeList val n vals
  | diff < 0 = take n vals
  | diff > 0 = vals ++ replicate diff val
  | otherwise = vals
  where
    diff = n - length vals

normalizeVoiceMotto :: [Maybe Int] -> [Duration] -> [Accent] -> [Dynamic] ->  VoiceMottos
normalizeVoiceMotto mInts durs accs dyns =
  VoiceMottos [mInts'] [durs] [accs'] [dyns']
  where
    len = length durs
    mInts' = normalizeList (Just 1) len mInts
    accs'  = normalizeList NoAccent len accs
    dyns'  = normalizeList NoDynamic len dyns

normalizeVoiceMottos :: VoiceMottos -> VoiceMottos
normalizeVoiceMottos VoiceMottos{..} =
  fold $ zipWith4 normalizeVoiceMotto _vmMIntss _vmDurss _vmAcctss _vmDynss

genMaxRandVoc :: Int -> VoiceMottos -> VoiceTup -> Driver Voice
genMaxRandVoc reps VoiceMottos{..} VoiceTup{..} = do
  mots <- concatMap (mtranspose _vtScale _vtStart) . take reps <$> randomElements _vmMIntss
  durs <- concat . take reps <$> randomElements _vmDurss
  accs <- concat . take reps <$> randomElements _vmAcctss
  dyns <- concat . take reps <$> randomElements _vmDynss
  pure $ SingleVoice _vtInstr (VeKeySignature _vtKey:VeClef _vtClef:genNotes mots durs accs dyns)

-- reps repetitions of the mottos all in the same order, no randomization, just repeats
-- Monadic to fit genRandVocs harness, otherwise pure.
genMinRandVoc :: Int -> VoiceMottos -> VoiceTup -> Driver Voice
genMinRandVoc reps vocmots VoiceTup{..} =
  pure $ SingleVoice _vtInstr (VeKeySignature _vtKey:VeClef _vtClef:genNotes mots durs accs dyns)
  where
    mots = concatMap (mtranspose _vtScale _vtStart) _vmMIntss
    durs = concat _vmDurss
    accs = concat _vmAcctss
    dyns = concat _vmDynss
    VoiceMottos{..} = stimes reps $ normalizeVoiceMottos vocmots

newtype GenRandVoice = GenRandVoice { _genRandVoc :: Int -> VoiceMottos -> VoiceTup -> Driver Voice }

genRandVocs :: GenRandVoice -> Int -> VoiceMottos -> [VoiceTup] -> Driver [Voice]
genRandVocs GenRandVoice{..} reps vocmots = mapM (_genRandVoc reps vocmots)

cfg2ConfigTup :: String -> Driver (Int, [VoiceTup], VoiceMottos)
cfg2ConfigTup title =
  (,,)
  <$> getConfigParam (title <> ".reps")
  <*> cfg2VocTups title ["voice1","voice2","voice3","voice4"]
  <*> cfg2VocMottos title

-- Select randomly from lists of mottos, durations, accents, and dynamics
-- so pairings vary continuously.  Write score with input title, also
-- head config parameter with subheads voice1 .. voice4, intss, durss,
-- accss, dynss, and reps.
cfg2MaxRandScore :: String -> Driver ()
cfg2MaxRandScore title = do
  (reps, voctups, vocmots) <- cfg2ConfigTup title
  voices  <- genRandVocs (GenRandVoice genMaxRandVoc) reps vocmots voctups
  writeScore title $ Score title voices

cfg2MinRandScore :: String -> Driver ()
cfg2MinRandScore title = do
  (reps, voctups, vocmots) <- cfg2ConfigTup title
  voices  <- genRandVocs (GenRandVoice genMinRandVoc) reps vocmots voctups
  writeScore title $ Score title voices

{--
matchLen :: Int -> [a] -> [a]
matchLen n arr
  | lenArr < n = take n arr
  | lenArr > n = concat $ replicate n arr
  | otherwise = arr
  where
    lenArr = length arr
--}

