{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Compose (cfg2MaxRandScore
               ,cfg2HomoPhonScore
               ,cfg2CanonScore
               ,cfg2RandMotScore) where

import Control.Monad (zipWithM)
import Data.Foldable (fold)
import Data.List (zipWith4)
import Data.Semigroup (stimesMonoid)

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

lift2VocMots :: (forall a . ([[a]] -> [[a]])) -> VoiceMottos -> VoiceMottos
lift2VocMots f VoiceMottos{..} = VoiceMottos (f _vmMIntss) (f _vmDurss) (f _vmAcctss) (f _vmDynss)

--lift2VocMotsM :: (forall a . ([[a]] -> Driver [[a]])) -> VoiceMottos -> Driver VoiceMottos
--lift2VocMotsM f VoiceMottos{..} = VoiceMottos <$> f _vmMIntss <*> f _vmDurss <*> f _vmAcctss <*> f _vmDynss

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
    mInts' = normalizeList Nothing len mInts
    accs'  = normalizeList NoAccent len accs
    dyns'  = normalizeList NoDynamic len dyns

normalizeVoiceMottos :: VoiceMottos -> VoiceMottos
normalizeVoiceMottos VoiceMottos{..} =
  fold $ zipWith4 normalizeVoiceMotto _vmMIntss _vmDurss _vmAcctss _vmDynss

genMaxRandVoc :: Int -> VoiceMottos -> VoiceTup -> GenVoiceMottos -> Driver Voice
genMaxRandVoc reps VoiceMottos{..} VoiceTup{..} _ = do
  mots <- concatMap (mtranspose _vtScale _vtStart) . take reps <$> randomElements _vmMIntss
  durs <- concat . take reps <$> randomElements _vmDurss
  accs <- concat . take reps <$> randomElements _vmAcctss
  dyns <- concat . take reps <$> randomElements _vmDynss
  pure $ SingleVoice _vtInstr (VeKeySignature _vtKey:VeClef _vtClef:genNotes mots durs accs dyns)

genVoc :: Int -> VoiceMottos -> VoiceTup -> GenVoiceMottos -> Driver Voice
genVoc reps vocmots VoiceTup{..} GenVoiceMottos{..} =
  pure $ SingleVoice _vtInstr (VeKeySignature _vtKey:VeClef _vtClef:genNotes mots durs accs dyns)
  where
    mots = concatMap (mtranspose _vtScale _vtStart) _vmMIntss
    durs = concat _vmDurss
    accs = concat _vmAcctss
    dyns = concat _vmDynss
    VoiceMottos{..} = stimesMonoid reps $ normalizeVoiceMottos (_genVoiceMottos vocmots)

genVocM :: Int -> VoiceMottos -> VoiceTup -> GenVoiceMottosM -> Driver Voice
genVocM _ vocmots VoiceTup{..} GenVoiceMottosM{..} = do
  VoiceMottos{..} <- _genVoiceMottosM vocmots
  let mots = concatMap (mtranspose _vtScale _vtStart) _vmMIntss
      durs = concat _vmDurss
      accs = concat _vmAcctss
      dyns = concat _vmDynss
  pure $ SingleVoice _vtInstr (VeKeySignature _vtKey:VeClef _vtClef:genNotes mots durs accs dyns)

newtype GenVoice = GenVoice { _genRandVoc :: Int -> VoiceMottos -> VoiceTup -> GenVoiceMottos -> Driver Voice }

newtype GenVoiceMottos = GenVoiceMottos { _genVoiceMottos :: VoiceMottos -> VoiceMottos }

newtype GenVoiceM = GenVoiceM { _genRandVocM :: Int -> VoiceMottos -> VoiceTup -> GenVoiceMottosM -> Driver Voice }

newtype GenVoiceMottosM = GenVoiceMottosM { _genVoiceMottosM :: VoiceMottos -> Driver VoiceMottos }

genVoices :: GenVoice -> Int -> VoiceMottos -> [VoiceTup] -> [GenVoiceMottos] -> Driver [Voice]
genVoices GenVoice{..} reps vocmots = zipWithM (_genRandVoc reps vocmots)

genVoicesM :: GenVoiceM -> Int -> VoiceMottos -> [VoiceTup] -> [GenVoiceMottosM] -> Driver [Voice]
genVoicesM GenVoiceM{..} reps vocmots = zipWithM (_genRandVocM reps vocmots)

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
  voices  <- genVoices (GenVoice genMaxRandVoc) reps vocmots voctups []
  writeScore title $ Score title voices

-- reps repetitions of the mottos all in the same order, no randomization, just repeats
cfg2HomoPhonScore :: String -> Driver ()
cfg2HomoPhonScore title = do
  (reps, voctups, vocmots) <- cfg2ConfigTup title
  let genVocMots = replicate (length voctups) $ GenVoiceMottos id
  voices  <- genVoices (GenVoice genVoc) reps vocmots voctups genVocMots
  writeScore title $ Score title voices

-- effectively canon after lead-in with all voices running at same imitative distance repeatedly
cfg2CanonScore :: String -> Driver ()
cfg2CanonScore title = do
  (reps, voctups, vocmots) <- cfg2ConfigTup title
  let genVocMot n = GenVoiceMottos $ lift2VocMots (rotNFor n)
      genVocMots = map genVocMot [0..(length voctups) - 1]
  voices <- genVoices (GenVoice genVoc) reps vocmots voctups genVocMots
  writeScore title $ Score title voices

genRandMots :: [Int] -> [[a]] -> [[a]]
genRandMots idxs src = map (src !!)  idxs

genRandVoiceMottos :: Int -> VoiceMottos -> Driver VoiceMottos
genRandVoiceMottos reps vocmots = do
  ridxs <- randomizeList idxs
  pure $ lift2VocMots (genRandMots ridxs) (normalizeVoiceMottos vocmots)
  where
    idxs = concat $ replicate reps [0..(length (_vmDurss vocmots) - 1)]

cfg2RandMotScore :: String -> Driver ()
cfg2RandMotScore title = do
  (reps, voctups, vocmots) <- cfg2ConfigTup title
  let genVocMotM = GenVoiceMottosM $ genRandVoiceMottos reps
      genVocMotMs = replicate (length voctups) genVocMotM
  voices  <- genVoicesM (GenVoiceM genVocM) reps vocmots voctups genVocMotMs
  writeScore title $ Score title voices

{--
matchLen :: Int -> [a] -> [a]
matchLen n arr
  | lenArr < n = take n arr
  | lenArr > n = concat $ replicate n arr
  | otherwise = arr
  where
    lenArr = length arr

genHomoPhonVoc :: Int -> VoiceMottos -> VoiceTup -> GenVoiceMottos -> Driver Voice
genHomoPhonVoc reps vocmots VoiceTup{..} _ =
  pure $ SingleVoice _vtInstr (VeKeySignature _vtKey:VeClef _vtClef:genNotes mots durs accs dyns)
  where
    mots = concatMap (mtranspose _vtScale _vtStart) _vmMIntss
    durs = concat _vmDurss
    accs = concat _vmAcctss
    dyns = concat _vmDynss
    VoiceMottos{..} = stimesMonoid reps $ normalizeVoiceMottos vocmots

genRot1RandVoc :: Int -> VoiceMottos -> VoiceTup -> GenVoiceMottos -> Driver Voice
genRot1RandVoc reps vocmots VoiceTup{..} GenVoiceMottos{..} =
  pure $ SingleVoice _vtInstr (VeKeySignature _vtKey:VeClef _vtClef:genNotes mots durs accs dyns)
  where
    mots = concatMap (mtranspose _vtScale _vtStart) _vmMIntss
    durs = concat _vmDurss
    accs = concat _vmAcctss
    dyns = concat _vmDynss
    VoiceMottos{..} = stimesMonoid reps $ normalizeVoiceMottos (_genVoiceMottos vocmots)
--}


