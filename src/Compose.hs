module Compose (cfg2MaxRandScore) where

import Driver
import Types
import Utils

type VoiceTup = (Instrument, KeySignature, Clef, [Pitch], (Pitch,Octave))

cfg2VocTup :: String -> Driver VoiceTup
cfg2VocTup pre = do
  instr <- getConfigParam (pre <> ".instr")
  key <- getConfigParam (pre <> ".key")
  clef <- getConfigParam (pre <> ".clef")
  pitOctPr <- getConfigParam (pre <> ".start")
  scale <- getConfigParam (pre <> ".scale")
  pure (instr, key, clef, scale, pitOctPr)

cfg2VocTups :: String -> [String] -> Driver [VoiceTup]
cfg2VocTups root = mapM (\v -> cfg2VocTup (root <> "." <> v))

genMaxRandVoc :: Int -> [[Maybe Int]] -> [[Duration]] -> [[Accent]] -> [[Dynamic]] -> VoiceTup -> Driver Voice
genMaxRandVoc reps mottos durss accss dynss (instr, key, clef, scale, (p,o))= do
  mots <- concatMap (mtranspose scale (p,o)) . take reps <$> randomElements mottos
  durs <- concat . take reps <$> randomElements durss
  accs <- concat . take reps <$> randomElements accss
  dyns <- concat . take reps <$> randomElements dynss
  pure $ SingleVoice instr (VeKeySignature key:VeClef clef:genNotes mots durs accs dyns)

genMaxRandVocs :: Int -> [[Maybe Int]] -> [[Duration]] -> [[Accent]] -> [[Dynamic]] -> [VoiceTup] -> Driver [Voice]
genMaxRandVocs reps mottos durss accss dynss = mapM (genMaxRandVoc reps mottos durss accss dynss)

-- Select randomly from lists of mottos, durations, accents, and dynamics
-- so pairings vary continuously.  Write score with input title, also
-- head config parameter with subheads voice1 .. voice4, intss, durss,
-- accss, dynss, and reps.
cfg2MaxRandScore :: String -> Driver ()
cfg2MaxRandScore title = do
  voctups <- cfg2VocTups title ["voice1","voice2","voice3","voice4"]
  mottos <- getConfigParam (title <> ".intss")
  durss <- getConfigParam (title <> ".durss")
  accss <- getConfigParam (title <> ".accss")
  dynss <- getConfigParam (title <> ".dynss")
  reps  <- getConfigParam (title <> ".reps")
  voices <- genMaxRandVocs reps mottos durss accss dynss voctups
  writeScore title $ Score title voices
