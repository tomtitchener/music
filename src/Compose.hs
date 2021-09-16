{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Compose {--(cfg2Driver, genSwirl)--} where

import Control.Monad (zipWithM)
import Data.Foldable (fold)
import Data.Functor ((<&>))
import Data.List (zipWith4, unfoldr, foldl')
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (stimesMonoid)
import Data.Tuple (swap)
import GHC.Base (sconcat)
import Safe (headMay)

import Driver
    ( getConfigParam,
      randomElements,
      randomizeList,
      writeScore,
      Driver )
import Types
import Utils
    ( genNotes,
      genNotesWithTies,
      mtranspose,
      normPitch,
      neZipWith3,
      neZipWith7,
      rotNFor,
      seqMTranspose,
      xp)

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
      genVocMots = map genVocMot [0..length voctups - 1]
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
Arpeggios:  weave of voices all arpeggios cycling up/down
  - voices can vary by
    * range: so e.g. at the same duration, one may occupy the full range
      while another would cycle at double speed over lower half and another
      over upper half -- you'd get doubling with bottom and outer voice for
      first and last eighths, while half-range voices moved in parallel
    * intervals for harmony and for total duration e.g. if count of notes
      to span same range is greater or less
    * accent for motive/melody against background
  - plan:
    * start with unique voice data of scale, instrument, and range only,
      uniform for everything else
    * move data from mottos to voice or find a way to programmatically
      sequence using more general input in mottos
--}

-- Data that varies voice-by-voice:
data ArpeggiosVoiceTup = ArpeggiosVoiceTup {_atInstr :: Instrument
                                           ,_atKey    :: KeySignature
                                           ,_atScale  :: Scale
                                           ,_atRanges :: NE.NonEmpty ((Pitch,Octave),(Pitch,Octave))
                                           } deriving (Show)

-- Data that stays the same for all voices:
data ArpeggiosMottos = ArpeggiosMottos {_amMIntss :: NE.NonEmpty (NE.NonEmpty (Maybe Int))
                                       ,_amDurss  :: NE.NonEmpty (NE.NonEmpty Duration)
                                       ,_amAcctss :: NE.NonEmpty (NE.NonEmpty Accent)
                                       ,_amDynss  :: NE.NonEmpty (NE.NonEmpty Dynamic)
                                       ,_amSlurss :: NE.NonEmpty (NE.NonEmpty Bool)
                                       } deriving (Show)

cfg2ArpeggioVocTup :: String -> Driver ArpeggiosVoiceTup
cfg2ArpeggioVocTup pre =
  ArpeggiosVoiceTup
    <$> getConfigParam (pre <> ".instr")
    <*> getConfigParam (pre <> ".key")
    <*> getConfigParam (pre <> ".scale")
    <*> getConfigParam (pre <> ".ranges")


cfg2Tups :: (String -> Driver a) -> String -> NE.NonEmpty String -> Driver (NE.NonEmpty a)
cfg2Tups f title = traverse (\v -> f (title <> "." <> v))

cfg2ArpeggiosVocTups :: String -> NE.NonEmpty String -> Driver (NE.NonEmpty ArpeggiosVoiceTup)
cfg2ArpeggiosVocTups = cfg2Tups cfg2ArpeggioVocTup

cfg2ArpeggiosMottos :: String -> Driver ArpeggiosMottos
cfg2ArpeggiosMottos title =
  ArpeggiosMottos
    <$> getConfigParam (title <> ".intss")
    <*> getConfigParam (title <> ".durss")
    <*> getConfigParam (title <> ".accss")
    <*> getConfigParam (title <> ".dynss")
    <*> getConfigParam (title <> ".slurss")

cfg2ArpeggiosConfigTup :: String -> Driver (NE.NonEmpty ArpeggiosVoiceTup, ArpeggiosMottos)
cfg2ArpeggiosConfigTup title =
  (,)
  <$> cfg2ArpeggiosVocTups title ("voice1" NE.:| ["voice2"])
  <*> cfg2ArpeggiosMottos title

-- mIntss and ranges give [Maybe (Pitch,Octave)] via seqMTranspose scale.
-- Expand durs, accts, dyns, slurs to be at least as long as [Maybe (Pitch,Octave)]
-- for input to genNotesWithTiesto answer [VoiceEvent] for a particular voice.
genArpVEs ::
  Scale
  -> NE.NonEmpty ((Pitch,Octave),(Pitch,Octave))
  -> NE.NonEmpty (NE.NonEmpty (Maybe Int))
  -> NE.NonEmpty Duration
  -> NE.NonEmpty Accent
  -> NE.NonEmpty Dynamic
  -> NE.NonEmpty Bool
  -> NE.NonEmpty VoiceEvent
genArpVEs scale ranges mIntss durs accts dyns slurs =
  genNotesWithTies mPOs (NE.cycle durs) (NE.cycle accts) (NE.cycle dyns) (NE.cycle slurs)
  where
    mPOs = sconcat $ NE.zipWith (seqMTranspose scale) mIntss ranges

genVocs :: Instrument -> KeySignature -> NE.NonEmpty VoiceEvent -> Voice
genVocs instr keySig ves = PitchedVoice instr (VeKeySignature keySig NE.<| ves)

cfg2ArpeggiosScore :: String -> Driver ()
cfg2ArpeggiosScore title = do
  (arpVocTups, ArpeggiosMottos{..}) <- cfg2ArpeggiosConfigTup title
  let instrs = _atInstr   <$> arpVocTups
      scales = _atScale   <$> arpVocTups
      keys   = _atKey     <$> arpVocTups
      ranges = _atRanges  <$> arpVocTups
      vess   = neZipWith7 genArpVEs scales ranges (NE.repeat _amMIntss) _amDurss _amAcctss  _amDynss _amSlurss
      voices = neZipWith3 genVocs instrs keys vess
  writeScore ("./" <> title <> ".ly") $ Score "no comment" voices

driverFuns :: [(String,String -> Driver ())]
driverFuns =
  [("example_texture",   cfg2MaxRandScore)
  ,("example_texture",   cfg2HomoPhonScore)
  ,("example_texture",   cfg2CanonScore)
  ,("exmaple_texture",   cfg2RandMotScore)
  ,("example_arpeggios", cfg2ArpeggiosScore)
  ,("example_swirls",    cfg2SwirlsScore')]

cfg2Driver :: String -> Driver ()
cfg2Driver title = maybe (error $ "cfg2Driver: no fun for title " <> title) ($ title) $ lookup title driverFuns

{--
  Consider a generator against a chromatic scale.
  Specify a list of intervals as a motif, and string together a small list of motifs where the overall direction is either ascending or descending.
  Randomly sequence the list of motifs so the overall direction persists.
  Specify start and stop pitch/octave pairs.
  Repeat kernel generation chaining from one to the next from the start to the stop.
--}

newtype Range = Range ((Pitch, Octave),(Pitch, Octave)) deriving (Eq, Show)

-- This is complicated:  here I only use adjacent sharps as though
-- the line always ascends.  To make a score easier to read, I should
-- choose the pitch according to the local shape of the line.  For
-- now that can be a refinement.
chromaticScale :: Scale
chromaticScale = Scale (NE.fromList[C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B])

genIntervalList :: [Pitch] -> [Int]
genIntervalList = reverse . snd . foldl f (0,[]) . (normPitch <$>)
  where
    f (p,l) x = (x, x - p:l)

-- ascending, six note patterns with an internal swirl
ascSwirlPit6a, ascSwirlPit6b, ascSwirlPit6c :: [Pitch]
ascSwirlPit6a = [C,Ds,E,D,Cs,E]
ascSwirlPit6b = [C,G,D,A,Ds,D]
ascSwirlPit6c = [Cs,D,Fs,A,E,F]
ascSwirl6Pits :: [[Pitch]]
ascSwirl6Pits = [ascSwirlPit6a,ascSwirlPit6b,ascSwirlPit6c]

genSwirl :: [Duration] -> [[Pitch]] -> Scale -> Range -> Driver [Note]
genSwirl durs motifs scale (Range (start,stop)) = do
  steps <- randomizeList motifs <&> concatMap genIntervalList
  let stepOrd = sum steps `compare` 0
      rangeOrd = swap stop `compare` swap start
      compareOp = if stepOrd == rangeOrd && stepOrd /= EQ
                  then if stepOrd == LT then (<=) else (>=)
                  else error $ "invalid step order " <> show stepOrd <> " compared with range order " <> show rangeOrd
      manySteps = concat $ repeat steps
      manyDurs = concat $ repeat durs
      pOs :: [(Pitch,Octave)] = unfoldr (f compareOp) (start,manySteps)
  pure $ zipWith mkNote pOs manyDurs
  where
    f cmp (prev,step:steps)
      | swap next `cmp` swap stop = Nothing
      | otherwise = Just (next, (next, steps))
      where
        next = xp scale prev step
    f _ steps = error $ "invalid list of steps " <> show steps
    mkNote (p,o) d = Note p o d Staccatissimo NoDynamic  NoSwell False

genSwirl' :: NE.NonEmpty Duration -> NE.NonEmpty (NE.NonEmpty Pitch) -> Scale -> Range -> Driver (NE.NonEmpty Note)
genSwirl' durs motifs scale (Range (start,stop)) = do
  steps <- randomizeList motifs' <&> concatMap genIntervalList
  let stepOrd = sum steps `compare` 0
      rangeOrd = swap stop `compare` swap start
      compareOp = if stepOrd == rangeOrd && stepOrd /= EQ
                  then if stepOrd == LT then (<=) else (>=)
                  else error $ "invalid step order " <> show stepOrd <> " compared with range order " <> show rangeOrd
      manySteps = concat $ repeat steps
      manyDurs = NE.cycle durs
      pOs :: NE.NonEmpty (Pitch,Octave) = NE.unfoldr (f compareOp) (start,manySteps)
  pure $ NE.zipWith mkNote pOs manyDurs
  where
    motifs' = map NE.toList $ NE.toList motifs
    f cmp (prev,step1:step2:steps)
      | swap nextnext `cmp` swap stop = (next, Nothing)
      | otherwise = (next, Just (next, step2:steps))
      where
        next = xp scale prev step1
        nextnext = xp scale next step2
    f _ steps = error $ "invalid list of steps " <> show steps
    mkNote (p,o) d = Note p o d Staccatissimo NoDynamic  NoSwell False

-- > gen <- getStdGen
-- > liftIO (runReaderT (runDriver (notes >>= print)) (initEnv Y.Null (show gen)))
--notes :: Driver [Note]
--notes = genSwirl [QDur,EDur,EDur,QDur,EDur,EDur,EDur] ascSwirl6Pits chromaticScale (Range ((C,FifteenVBOct),(B,FifteenVAOct)))

data SwirlsTup = SwirlsTup {_stInstr :: Instrument
                           ,_stKey   :: KeySignature
                           ,_stScale :: Scale
                           ,_stPitss :: NE.NonEmpty (NE.NonEmpty Pitch)
                           ,_stDurs  :: NE.NonEmpty Duration
                           ,_stRange :: ((Pitch,Octave),(Pitch,Octave))
                           } deriving Show

cfg2SwirlsTup :: String -> Driver SwirlsTup
cfg2SwirlsTup pre =
  SwirlsTup
    <$> getConfigParam (pre <> ".instr")
    <*> getConfigParam (pre <> ".key")
    <*> getConfigParam (pre <> ".scale")
    <*> getConfigParam (pre <> ".pitss")
    <*> getConfigParam (pre <> ".durs")
    <*> getConfigParam (pre <> ".range")

cfg2SwirlsTups :: String -> NE.NonEmpty String -> Driver (NE.NonEmpty SwirlsTup)
cfg2SwirlsTups = cfg2Tups cfg2SwirlsTup

swirlsTup2VEs :: SwirlsTup -> Driver (NE.NonEmpty VoiceEvent)
swirlsTup2VEs SwirlsTup{..} = do
  genSwirl dursl pitssll _stScale (Range _stRange) <&> NE.fromList . map VeNote
  where
    dursl = NE.toList _stDurs
    pitssll = NE.toList . NE.map  NE.toList $ _stPitss
    
swirlsTup2VEs' :: SwirlsTup -> Driver (NE.NonEmpty VoiceEvent)
swirlsTup2VEs' SwirlsTup{..} = 
  genSwirl' _stDurs _stPitss _stScale (Range _stRange) <&> NE.map VeNote

pickNoteClef :: Note -> Clef
pickNoteClef Note{..}
  | (_noteOct,_notePit) <= (COct,E) = Bass
  | otherwise = Treble

pickVoiceEventClef :: VoiceEvent -> Clef
pickVoiceEventClef (VeNote note) = pickNoteClef note
pickVoiceEventClef evnt = error $ "pickVoiceEventClef unexpected VoiceEvent: " <> show evnt

veIsVeNote :: VoiceEvent -> Bool
veIsVeNote (VeNote _) = True
veIsVeNote _ = False

pickInitialClef :: [VoiceEvent] -> Clef
pickInitialClef ves =  maybe err pickVoiceEventClef . headMay . filter veIsVeNote $ ves
  where err = error $ "pickInitialClef no VeNote in [VoiceEvent]: " <> show ves
  
pickInitialClef' :: NE.NonEmpty VoiceEvent -> Clef
pickInitialClef' ves =  maybe err pickVoiceEventClef . headMay . NE.filter veIsVeNote $ ves
  where err = error $ "pickInitialClef no VeNote in [VoiceEvent]: " <> show (NE.toList ves)

splitStaves :: Clef -> [VoiceEvent] -> ([VoiceEvent],[VoiceEvent])
splitStaves clef = snd . foldl' f (clef,([],[]))
  where
    f :: (Clef,([VoiceEvent],[VoiceEvent])) -> VoiceEvent -> (Clef,([VoiceEvent],[VoiceEvent]))
    f (cl,(trebleVes,bassVes)) ve@(VeNote note@Note{..})
      | cl == Treble && cl' == Treble = (Treble, (trebleVes <> [ve], bassVes <> [sp]))
      | cl == Bass   && cl' == Treble = (Treble, (trebleVes <> [ve], bassVes <> [sp]))
      | cl == Bass   && cl' == Bass   = (Bass,   (trebleVes <> [sp], bassVes <> [ve]))
      | cl == Treble && cl' == Bass   = (Bass,   (trebleVes <> [sp], bassVes <> [ve]))
      | otherwise = error $ "splitStaves unexpected clefs: " <> show cl <> " and " <> show cl'
      where
        cl' = pickNoteClef note
        sp = VeSpacer (Spacer _noteDur NoDynamic)
    f (cl,(trebleVes,bassVes)) ve@(VeRest Rest{..})
      | cl == Treble = (Treble, (trebleVes <> [ve], bassVes <> [sp]))
      | cl == Bass   = (Bass,   (trebleVes <> [sp], bassVes <> [ve]))
      | otherwise = error $ "splitSaves unexpected clef: " <> show cl
      where
        sp = VeSpacer (Spacer _restDur NoDynamic)
    f (cl,(trebleVes,bassVes)) ve@VeTempo {}                 = (cl,(trebleVes <> [ve],bassVes <> [ve]))
    f (cl,(trebleVes,bassVes)) ve@VeKeySignature {}          = (cl,(trebleVes <> [ve],bassVes <> [ve]))
    f (cl,(trebleVes,bassVes)) ve@VeTimeSignature {}         = (cl,(trebleVes <> [ve],bassVes <> [ve]))
    f (_,_) ve = error $ "splitStaves unexpected VoiceEvent: " <> show ve

splitStaves' :: Clef -> NE.NonEmpty VoiceEvent -> (NE.NonEmpty VoiceEvent,NE.NonEmpty VoiceEvent)
splitStaves' clef = snd . foldl f (clef,(VeClef Treble NE.:| [],VeClef Bass NE.:| []))
  where
    f :: (Clef,(NE.NonEmpty VoiceEvent,NE.NonEmpty VoiceEvent)) -> VoiceEvent -> (Clef,(NE.NonEmpty VoiceEvent,NE.NonEmpty VoiceEvent))
    f (cl,(trebleVes,bassVes)) ve@(VeNote note@Note{..})
      | cl == Treble && cl' == Treble = (Treble, (trebleVes <> (ve NE.:| []), bassVes <> (sp NE.:| [])))
      | cl == Bass   && cl' == Treble = (Treble, (trebleVes <> (ve NE.:| []), bassVes <> (sp NE.:| [])))
      | cl == Bass   && cl' == Bass   = (Bass,   (trebleVes <> (sp NE.:| []), bassVes <> (ve NE.:| [])))
      | cl == Treble && cl' == Bass   = (Bass,   (trebleVes <> (sp NE.:| []), bassVes <> (ve NE.:| [])))
      | otherwise = error $ "splitStaves unexpected clefs: " <> show cl <> " and " <> show cl'
      where
        cl' = pickNoteClef note
        sp = VeSpacer (Spacer _noteDur NoDynamic)
    f (cl,(trebleVes,bassVes)) ve@(VeRest Rest{..})
      | cl == Treble = (Treble, (trebleVes <> (ve NE.:| []), bassVes <> (sp NE.:| [])))
      | cl == Bass   = (Bass,   (trebleVes <> (sp NE.:| []), bassVes <> (ve NE.:| [])))
      | otherwise = error $ "splitSaves unexpected clef: " <> show cl
      where
        sp = VeSpacer (Spacer _restDur NoDynamic)
    f (cl,(trebleVes,bassVes)) ve@VeTempo {}                 = (cl,(trebleVes <> (ve NE.:| []),bassVes <> (ve NE.:| [])))
    f (cl,(trebleVes,bassVes)) ve@VeKeySignature {}          = (cl,(trebleVes <> (ve NE.:| []),bassVes <> (ve NE.:| [])))
    f (cl,(trebleVes,bassVes)) ve@VeTimeSignature {}         = (cl,(trebleVes <> (ve NE.:| []),bassVes <> (ve NE.:| [])))
    f (_,_) ve = error $ "splitStaves unexpected VoiceEvent: " <> show ve

genPolyVocs :: Instrument -> KeySignature -> ([VoiceEvent],[VoiceEvent]) -> Voice
genPolyVocs instr keySig (treble,bass) = PolyVoice instr vess
  where
    vess = NE.fromList [NE.fromList (trebleClef:veKeySig:treble), NE.fromList (bassClef:veKeySig:bass)]
    bassClef = VeClef Bass
    trebleClef = VeClef Treble
    veKeySig = VeKeySignature keySig
    
genPolyVocs' :: Instrument -> KeySignature -> (NE.NonEmpty VoiceEvent,NE.NonEmpty VoiceEvent) -> Voice
genPolyVocs' instr keySig (treble,bass) = PolyVoice instr vess
  where
    vess = NE.fromList [veKeySig NE.<| treble, veKeySig NE.<| bass]
    veKeySig = VeKeySignature keySig

cfg2SwirlsScore :: String -> Driver ()
cfg2SwirlsScore title = do
  tups <- cfg2SwirlsTups title (NE.fromList ["voice1"]) <&> NE.toList
  vess <- traverse swirlsTup2VEs tups <&> fmap NE.toList
  let clefs = pickInitialClef <$> vess
      vePrss = zipWith splitStaves clefs vess
      voices = zipWith3 genPolyVocs (_stInstr <$> tups) (_stKey <$> tups) vePrss
  writeScore ("./" <> title <> ".ly") $ Score "no comment" (NE.fromList voices)

cfg2SwirlsScore' :: String -> Driver ()
cfg2SwirlsScore' title = do
  tups <- cfg2SwirlsTups title (NE.fromList ["voice1"])
  vess <- traverse swirlsTup2VEs' tups 
  let clefs = pickInitialClef' <$> vess
      vePrss = NE.zipWith splitStaves' clefs vess
      voices = neZipWith3 genPolyVocs' (_stInstr <$> tups) (_stKey <$> tups) vePrss
  writeScore ("./" <> title <> ".ly") $ Score "no comment" voices

