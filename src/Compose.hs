{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Compose (cfg2MaxRandScore
               ,cfg2HomoPhonScore
               ,cfg2CanonScore
               ,cfg2RandMotScore
               ,cfg2ArpeggiosScore) where

import Control.Monad (zipWithM)
import Data.Foldable (fold)
import Data.List (zipWith4)
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (stimesMonoid)

import Driver
import Types
import Utils

data MottoVoiceTup = MottoVoiceTup {_vtInstr :: Instrument
                                   ,_vtKey   :: KeySignature
                                   ,_vtClef  :: Clef
                                   ,_vtScale :: Scale
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

cfg2MottoVocTup :: String -> Driver MottoVoiceTup
cfg2MottoVocTup pre =
  MottoVoiceTup
    <$> getConfigParam (pre <> ".instr")
    <*> getConfigParam (pre <> ".key")
    <*> getConfigParam (pre <> ".clef")
    <*> getConfigParam (pre <> ".scale")
    <*> getConfigParam (pre <> ".start")

cfg2MottoVocTups :: String -> [String] -> Driver [MottoVoiceTup]
cfg2MottoVocTups root = mapM (\v -> cfg2MottoVocTup (root <> "." <> v))

nes2arrs :: NE.NonEmpty (NE.NonEmpty a) -> [[a]]
nes2arrs = map NE.toList . NE.toList

cfg2VocMottos :: String -> Driver VoiceMottos
cfg2VocMottos title =
  VoiceMottos
    <$> (nes2arrs <$> getConfigParam (title <> ".intss"))
    <*> (nes2arrs <$> getConfigParam (title <> ".durss"))
    <*> (nes2arrs <$> getConfigParam (title <> ".accss"))
    <*> (nes2arrs <$> getConfigParam (title <> ".dynss"))

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

genMaxRandVoc :: Int -> VoiceMottos -> MottoVoiceTup -> GenVoiceMottos -> Driver Voice
genMaxRandVoc reps VoiceMottos{..} MottoVoiceTup{..} _ = do
  mots <- concatMap (mtranspose _vtScale _vtStart) . take reps <$> randomElements _vmMIntss
  durs <- concat . take reps <$> randomElements _vmDurss
  accs <- concat . take reps <$> randomElements _vmAcctss
  dyns <- concat . take reps <$> randomElements _vmDynss
  pure $ PitchedVoice _vtInstr (NE.fromList (VeKeySignature _vtKey:VeClef _vtClef:genNotes mots durs accs dyns))

genVoc :: Int -> VoiceMottos -> MottoVoiceTup -> GenVoiceMottos -> Driver Voice
genVoc reps vocmots MottoVoiceTup{..} GenVoiceMottos{..} =
  pure $ PitchedVoice _vtInstr (NE.fromList (VeKeySignature _vtKey:VeClef _vtClef:genNotes mots durs accs dyns))
  where
    mots = concatMap (mtranspose _vtScale _vtStart) _vmMIntss
    durs = concat _vmDurss
    accs = concat _vmAcctss
    dyns = concat _vmDynss
    VoiceMottos{..} = stimesMonoid reps $ normalizeVoiceMottos (_genVoiceMottos vocmots)

genVocM :: Int -> VoiceMottos -> MottoVoiceTup -> GenVoiceMottosM -> Driver Voice
genVocM _ vocmots MottoVoiceTup{..} GenVoiceMottosM{..} = do
  VoiceMottos{..} <- _genVoiceMottosM vocmots
  let mots = concatMap (mtranspose _vtScale _vtStart) _vmMIntss
      durs = concat _vmDurss
      accs = concat _vmAcctss
      dyns = concat _vmDynss
  pure $ PitchedVoice _vtInstr (NE.fromList (VeKeySignature _vtKey:VeClef _vtClef:genNotes mots durs accs dyns))

newtype GenVoice = GenVoice { _genRandVoc :: Int -> VoiceMottos -> MottoVoiceTup -> GenVoiceMottos -> Driver Voice }

newtype GenVoiceMottos = GenVoiceMottos { _genVoiceMottos :: VoiceMottos -> VoiceMottos }

newtype GenVoiceM = GenVoiceM { _genRandVocM :: Int -> VoiceMottos -> MottoVoiceTup -> GenVoiceMottosM -> Driver Voice }

newtype GenVoiceMottosM = GenVoiceMottosM { _genVoiceMottosM :: VoiceMottos -> Driver VoiceMottos }

genVoices :: GenVoice -> Int -> VoiceMottos -> [MottoVoiceTup] -> [GenVoiceMottos] -> Driver (NE.NonEmpty Voice)
genVoices GenVoice{..} reps vocmots tups mottos = NE.fromList <$> zipWithM (_genRandVoc reps vocmots) tups mottos

genVoicesM :: GenVoiceM -> Int -> VoiceMottos -> [MottoVoiceTup] -> [GenVoiceMottosM] -> Driver (NE.NonEmpty Voice)
genVoicesM GenVoiceM{..} reps vocmots tups mots = NE.fromList <$> zipWithM (_genRandVocM reps vocmots) tups mots

cfg2MottoConfigTup :: String -> Driver (Int, [MottoVoiceTup], VoiceMottos)
cfg2MottoConfigTup title =
  (,,)
  <$> getConfigParam (title <> ".reps")
  <*> cfg2MottoVocTups title ["voice1","voice2","voice3","voice4"]
  <*> cfg2VocMottos title

-- Select randomly from lists of mottos, durations, accents, and dynamics
-- so pairings vary continuously.  Write score with input title, also
-- head config parameter with subheads voice1 .. voice4, intss, durss,
-- accss, dynss, and reps.
cfg2MaxRandScore :: String -> Driver ()
cfg2MaxRandScore title = do
  (reps, voctups, vocmots) <- cfg2MottoConfigTup title
  voices  <- genVoices (GenVoice genMaxRandVoc) reps vocmots voctups []
  writeScore title $ Score title voices

-- reps repetitions of the mottos all in the same order, no randomization, just repeats
cfg2HomoPhonScore :: String -> Driver ()
cfg2HomoPhonScore title = do
  (reps, voctups, vocmots) <- cfg2MottoConfigTup title
  let genVocMots = replicate (length voctups) $ GenVoiceMottos id
  voices  <- genVoices (GenVoice genVoc) reps vocmots voctups genVocMots
  writeScore title $ Score title voices

-- effectively canon after lead-in with all voices running at same imitative distance repeatedly
cfg2CanonScore :: String -> Driver ()
cfg2CanonScore title = do
  (reps, voctups, vocmots) <- cfg2MottoConfigTup title
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
  (reps, voctups, vocmots) <- cfg2MottoConfigTup title
  let genVocMotM = GenVoiceMottosM $ genRandVoiceMottos reps
      genVocMotMs = replicate (length voctups) genVocMotM
  voices  <- genVoicesM (GenVoiceM genVocM) reps vocmots voctups genVocMotMs
  writeScore title $ Score title voices

{--
Arpeggios
--}

data ArpeggiosVoiceTup = ArpeggiosVoiceTup {_atInstr :: Instrument
                                           ,_atKey    :: KeySignature
                                           ,_atScale  :: Scale
                                           ,_atStarts :: [(Pitch,Octave)]
                                           ,_atStops  :: [(Pitch,Octave)]
                                           } deriving (Show)


data ArpeggiosMottos = ArpeggiosMottos {_amMIntss :: [[Maybe Int]]
                                       ,_amDurss  :: [[Duration]]
                                       ,_amAcctss :: [[Accent]]
                                       ,_amDynss  :: [[Dynamic]]
                                       ,_amSlurss :: [[Bool]]
                                       } deriving (Show)

cfg2ArpeggioVocTup :: String -> Driver ArpeggiosVoiceTup
cfg2ArpeggioVocTup pre =
  ArpeggiosVoiceTup
    <$> getConfigParam (pre <> ".instr")
    <*> getConfigParam (pre <> ".key")
    <*> getConfigParam (pre <> ".scale")
    <*> (NE.toList <$> getConfigParam (pre <> ".starts"))
    <*> (NE.toList <$> getConfigParam (pre <> ".stops"))

cfg2ArpeggiosVocTups :: String -> [String] -> Driver [ArpeggiosVoiceTup]
cfg2ArpeggiosVocTups root = mapM (\v -> cfg2ArpeggioVocTup (root <> "." <> v))

cfg2ArpeggiosMottos :: String -> Driver ArpeggiosMottos
cfg2ArpeggiosMottos title =
  ArpeggiosMottos
    <$> (nes2arrs <$> getConfigParam (title <> ".intss"))
    <*> (nes2arrs <$> getConfigParam (title <> ".durss"))
    <*> (nes2arrs <$> getConfigParam (title <> ".accss"))
    <*> (nes2arrs <$> getConfigParam (title <> ".dynss"))
    <*> (nes2arrs <$> getConfigParam (title <> ".slurss"))

cfg2ArpeggiosConfigTup :: String -> Driver ([ArpeggiosVoiceTup], ArpeggiosMottos)
cfg2ArpeggiosConfigTup title =
  (,)
  <$> cfg2ArpeggiosVocTups title ["voice"]
  <*> cfg2ArpeggiosMottos title

cfg2ArpeggiosScore :: String -> Driver ()
cfg2ArpeggiosScore title = do
  (arpTups, arpMottos) <- cfg2ArpeggiosConfigTup title
  -- wrangle seqMTranspose :: Scale -> (Pitch,Octave) -> (Pitch,Octave) -> [Maybe Int] -> [Maybe (Pitch,Octave)]
  -- multiples are _atStarts, _atStops, _amMIntss gives [[Maybe (Pitch,Octave)]], each per voice
  let arpMPOss = map (\ArpeggiosVoiceTup{..} -> concat $ zipWith3 (seqMTranspose _atScale) _atStarts _atStops (_amMIntss arpMottos)) arpTups
  -- next need to take [[Just (C,FifteenVBOct),Nothing,..]] into [[Note]] using _amDurss, _amAcctss, _amDynss, _amSlurss
  -- keeping in mind length of each [Maybe(Pitch,Oct)] will be longer than each [Note].
  -- That means filling-out the lengths of the shorter lists to match the length of the longer
  -- list with repetitions of the shorter list, could be just take n from (concat . repeat $ _foo),
  -- e.g.: (take 4 (concat (repeat [1])))
  -- need engine to compose note:
  -- data Note = Note { _notePit :: Pitch, _noteOct :: Octave, _noteDur :: Duration, _noteAcc :: Accent, _noteDyn :: Dynamic, _noteSwell :: Swell, _noteSlur :: Bool }
  -- except don't forget Nothing, which turns into a Rest.  Which means what I actually want are VoiceEvents, with instances VeNote and VeRest
  -- so type is [Maybe (Pitch,Octave)] -> [Duration] -> [Accent] -> [Dynamic] -> [Swell] -> [Bool] -> [VoiceEvent]
  -- which at one level lower means Maybe (Pitch,Octave) -> Duration -> Accent -> Dynamic -> Swell -> Slur -> VoiceEvent
  Driver.print arpMPOss
  Driver.print arpMottos

{--
genVE :: Maybe (Pitch,Octave) -> Duration -> Accent -> Dynamic -> Swell -> Bool -> VoiceEvent
genVE Nothing dur _ dyn _ _ = VeRest (Rest dur dyn)
genVE (Just (pit,oct)) dur acc dyn swl slr = VeNote (Note pit oct dur acc dyn swl slr)

genVEs :: [Maybe (Pitch,Octave)] -> [Duration] -> [Accent] -> [Dynamic] -> [Swell] -> [Bool] -> [VoiceEvent]
genVEs mpos durs accs dyns swls slrs = zipWith6 genVE mpos durs accs dyns swls slrs

matchLen :: [a] -> [b] -> [b]
matchLen src trg = take (length src) (concat . repeat $ trg)
--}

{--
matchLen :: Int -> [a] -> [a]
matchLen n arr
  | lenArr < n = take n arr
  | lenArr > n = concat $ replicate n arr
  | otherwise = arr
  where
    lenArr = length arr

genHomoPhonVoc :: Int -> VoiceMottos -> MottoVoiceTup -> GenVoiceMottos -> Driver Voice
genHomoPhonVoc reps vocmots MottoVoiceTup{..} _ =
  pure $ PitchedVoice _vtInstr (VeKeySignature _vtKey:VeClef _vtClef:genNotes mots durs accs dyns)
  where
    mots = concatMap (mtranspose _vtScale _vtStart) _vmMIntss
    durs = concat _vmDurss
    accs = concat _vmAcctss
    dyns = concat _vmDynss
    VoiceMottos{..} = stimesMonoid reps $ normalizeVoiceMottos vocmots

genRot1RandVoc :: Int -> VoiceMottos -> MottoVoiceTup -> GenVoiceMottos -> Driver Voice
genRot1RandVoc reps vocmots MottoVoiceTup{..} GenVoiceMottos{..} =
  pure $ PitchedVoice _vtInstr (VeKeySignature _vtKey:VeClef _vtClef:genNotes mots durs accs dyns)
  where
    mots = concatMap (mtranspose _vtScale _vtStart) _vmMIntss
    durs = concat _vmDurss
    accs = concat _vmAcctss
    dyns = concat _vmDynss
    VoiceMottos{..} = stimesMonoid reps $ normalizeVoiceMottos (_genVoiceMottos vocmots)
--}


