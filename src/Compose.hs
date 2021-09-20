{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compose {--(cfg2Driver, genSwirl)--} where

import Control.Monad (zipWithM)
import Data.Foldable (fold)
import Data.Bifunctor (bimap)
import Data.Functor ((<&>))
import Data.List (zipWith4, groupBy)
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (stimesMonoid)
import Data.Tuple (swap)
import Data.Tuple.Extra (dupe, first)
import GHC.Base (sconcat)

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
  ,("example_swirls",    cfg2SwirlsScore)]

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

genIntervalList :: [Pitch] -> [Int]
genIntervalList = reverse . snd . foldl f (0,[]) . (normPitch <$>)
  where
    f (p,l) x = (x, x - p:l)

genSwirl :: NE.NonEmpty Duration -> NE.NonEmpty (NE.NonEmpty Pitch) -> Scale -> Range -> Driver (NE.NonEmpty Note)
genSwirl durs motifs scale (Range (start,stop)) = do
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
    
{--
swirlsTup2VEs :: SwirlsTup -> Driver (NE.NonEmpty VoiceEvent)
swirlsTup2VEs SwirlsTup{..} = 
  genSwirl _stDurs _stPitss _stScale (Range _stRange) <&> NE.map VeNote

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

pickInitialClef :: NE.NonEmpty VoiceEvent -> Clef
pickInitialClef ves =  maybe err pickVoiceEventClef . headMay . NE.filter veIsVeNote $ ves
  where err = error $ "pickInitialClef no VeNote in [VoiceEvent]: " <> show (NE.toList ves)

ve2MaybeClef :: VoiceEvent -> Maybe Clef
ve2MaybeClef (VeNote note) = Just (pickNoteClef note)
ve2MaybeClef _ = Nothing

singleton :: a -> NE.NonEmpty a
singleton = flip (NE.:|) []

-- tbd: insert debounce before accumulation of next element in NE.NonEmpty VoiceEvent pair.
-- going to mean replacing NE.NonEmpty wih ordinary list so debouncer can answer []!
-- nb: given ++ as <> for [], ([a],[b]) <> ([a],[b]) works as expected.
-- so, (trebleVes <> singleton ve,bassVes <> singleton sp) will work as
--     (trebleVes,bassVes) <> genPair cache ve
-- currently fold captures state as simply current Clef, this needs to be agumented
-- with list of VoiceEvent to debounce, max length e.g. 5
-- uh oh: more complicated than just using <> to append pairs, as I need to update
-- active clef as output from debouncer as well -- currently hard-wired to change
-- immediately
-- question: is it enough to cache list of VoiceEvent alone, note I can easily compute
-- Maybe clef per VoiceEvent or if that's too awkward, as [(VoiceEvent,Maybe Clef)]
-- nb: debounce list needs at least 5 VeNote, list could be longer if we accumulate some
-- Ve<other> events along the way
-- so that leaves me with state as (Clef,[(VoiceEvent,Clef)]) and type of debounce as
-- debounce :: (Clef,[(VoiceEvent,Clef)],([VoiceEvent],[VoiceEvent]))
--             -> VoiceEvent
--             -> (Clef,[(VoiceEvent,Clef)],([VoiceEvent],[VoiceEvent]))
-- or b -> a -> b for the foldl.
-- don't forget:  thread all VoiceEvent instances through cache to avoid out-of-order
-- don't forget:  purge [VoiceEvent,Clef]
splitStaves :: Clef -> NE.NonEmpty VoiceEvent -> (NE.NonEmpty VoiceEvent,NE.NonEmpty VoiceEvent)
splitStaves clef = snd . foldl f (clef,(singleton (VeClef Treble),singleton (VeClef Bass)))
  where
    f :: (Clef,(NE.NonEmpty VoiceEvent,NE.NonEmpty VoiceEvent)) -> VoiceEvent -> (Clef,(NE.NonEmpty VoiceEvent,NE.NonEmpty VoiceEvent))
    f (cl,(trebleVes,bassVes)) ve@(VeNote note@Note{..})
      | cl == Treble && cl' == Treble = (Treble, (trebleVes <> singleton ve, bassVes <> singleton sp))
      | cl == Bass   && cl' == Treble = (Treble, (trebleVes <> singleton ve, bassVes <> singleton sp))
      | cl == Bass   && cl' == Bass   = (Bass,   (trebleVes <> singleton sp, bassVes <> singleton ve))
      | cl == Treble && cl' == Bass   = (Bass,   (trebleVes <> singleton sp, bassVes <> singleton ve))
      | otherwise = error $ "splitStaves unexpected clefs: " <> show cl <> " and " <> show cl'
      where
        cl' = pickNoteClef note
        sp = VeSpacer (Spacer _noteDur NoDynamic)
    f (cl,(trebleVes,bassVes)) ve@(VeRest Rest{..})
      | cl == Treble = (Treble, (trebleVes <> singleton ve, bassVes <> singleton sp))
      | cl == Bass   = (Bass,   (trebleVes <> singleton sp, bassVes <> singleton ve))
      | otherwise = error $ "splitStaves unexpected clef: " <> show cl
      where
        sp = VeSpacer (Spacer _restDur NoDynamic)
    f (cl,(trebleVes,bassVes)) ve@VeTempo {}                 = (cl,(trebleVes <> singleton ve,bassVes <> singleton ve))
    f (cl,(trebleVes,bassVes)) ve@VeKeySignature {}          = (cl,(trebleVes <> singleton ve,bassVes <> singleton ve))
    f (cl,(trebleVes,bassVes)) ve@VeTimeSignature {}         = (cl,(trebleVes <> singleton ve,bassVes <> singleton ve))
    f (_,_) ve = error $ "splitVoiceEvent unexpected VoiceEvent: " <> show ve
    
splitVoiceEvent :: Clef -> VoiceEvent -> (Clef,(VoiceEvent,VoiceEvent))
splitVoiceEvent cl ve@(VeNote note@Note{..})
      | cl == Treble && cl' == Treble = (Treble, (ve, sp))
      | cl == Bass   && cl' == Treble = (Treble, (ve, sp))
      | cl == Bass   && cl' == Bass   = (Bass,   (sp, ve))
      | cl == Treble && cl' == Bass   = (Bass,   (sp, ve))
      | otherwise = error $ "splitVoiceEvent unexpected clefs: " <> show cl <> " and " <> show cl'
      where
        cl' = pickNoteClef note
        sp = VeSpacer (Spacer _noteDur NoDynamic)
splitVoiceEvent cl ve@(VeRest Rest{..})
      | cl == Treble = (Treble, (ve, sp))
      | cl == Bass   = (Bass,   (sp, ve))
      | otherwise = error $ "splitVoiceEventunexpected clef: " <> show cl
      where
        sp = VeSpacer (Spacer _restDur NoDynamic)
splitVoiceEvent cl ve@VeTempo {}                 = (cl,(ve,ve))
splitVoiceEvent cl ve@VeKeySignature {}          = (cl,(ve,ve))
splitVoiceEvent cl ve@VeTimeSignature {}         = (cl,(ve,ve))
splitVoiceEvent _ ve = error $ "splitVoiceEvent  unexpected VoiceEvent: " <> show ve

forceClef :: Clef -> VoiceEvent -> (VoiceEvent,VoiceEvent)
forceClef cl ve@(VeNote note@Note{..})
      | cl == Treble = (ve, sp)
      | cl == Bass   = (sp, ve)
      | otherwise = error $ "forceClef unexpected clef: " <> show cl
      where
        sp = VeSpacer (Spacer _noteDur NoDynamic)
forceClef cl ve@(VeRest Rest{..})
      | cl == Treble = (ve, sp)
      | cl == Bass   = (sp, ve)
      | otherwise = error $ "forceClef unexpected clef: " <> show cl
      where
        sp = VeSpacer (Spacer _restDur NoDynamic)
forceClef cl ve@VeTempo {}                 = (ve,ve)
forceClef cl ve@VeKeySignature {}          = (ve,ve)
forceClef cl ve@VeTimeSignature {}         = (ve,ve)
forceClef _ ve = error $ "forceClef  unexpected VoiceEvent: " <> show ve

-- algorithm:  first, need to fill window, or can I just grab until first 5 note events to initialize ahead of time?
-- or do I do it opportunistically:
-- a) if window is empty, pass through until first note, then take clef of first note and compare with active clef:
--    a') if clefs are the same, then emit note to output VesPr
--    a'') if clefs are different, then add note to win
-- or I can circumvent this whole startup by figuring out the starting clef ahead of time and passing everything
-- through to VesPr until I encounter the first note with a new clef
-- 
type VeClefPr = (VoiceEvent,Clef)
type VesPr = ([VoiceEvent],[VoiceEvent])
type SplitState = (Clef,[VeClefPr],VesPr)

splitStaves' :: Clef -> NE.NonEmpty VoiceEvent -> (NE.NonEmpty VoiceEvent,NE.NonEmpty VoiceEvent)
splitStaves' clef = purge . foldl f (clef,([VeClef Treble],[VeClef Bass])) . bimap NE.toList NE.toList
  where
    f :: SplitState -> VoiceEvent -> SplitState
    f (cl,window,vesPr) ve@(VeNote note@Note{..})
      | null window && cl == cl' && cl == Treble = (Treble, [], vesPr <> (ve,sp))
      | null window && cl == cl' && cl == Bass   = (Bass,   [], vesPr <> (sp,ve))
      | cntNotes < maxRunCnt = (cl,window <> [(cl',ve)],vesPr)
      | otherwise = (cl',[(cl',ve)],vesPr <> debounce window)
      where
        cl' = pickNoteClef note
        sp = VeSpacer (Spacer _noteDur NoDynamic)
        maxRunCnt :: Int = 5
        cntNotes = length (filter isNote window)
        isNote :: VeClefPr -> Bool
        isNote (VeNote {},_) = True
        isNote _ = False
        debounce win
          | all (== cl) (map snd notes) = forceClef cl (map fst window)
          | otherwise = forceClef (otherClef cl) (map fst window)
          where
            notes = filter isNote window
            cl = snd $ head notes
            forceClef Treble window = map f window
            otherClef Treble = Bass
            otherClef Bass = Treble
            otherClef cl = error $ "otherClef unexpected clef: " <> show cl
    f (cl,(trebleVes,bassVes)) ve@(VeRest Rest{..})
      | ...
    f (cl,(trebleVes,bassVes)) ve@VeTempo {}                 = (cl,(trebleVes <> singleton ve,bassVes <> singleton ve))
    f (cl,(trebleVes,bassVes)) ve@VeKeySignature {}          = (cl,(trebleVes <> singleton ve,bassVes <> singleton ve))
    f (cl,(trebleVes,bassVes)) ve@VeTimeSignature {}         = (cl,(trebleVes <> singleton ve,bassVes <> singleton ve))
    f (_,_) ve = error $ "splitStaves unexpected VoiceEvent: " <> show ve
    purge :: SplitState -> VesPr
    purge (_, win, ves) = ves <> unzip (map win2Ves win)
    win2Ves :: VeClefPr -> VesPr
    win2Ves (Treble,ve) = (ve,ve2Sp ve)
    win2Ves (Bass,  ve) = (ve2Sp ve,ve)
    ve2Sp :: VoiceEvent -> Spacer
    ve2Sp VeNote{..} = Spacer _noteDur NoDynamic
    ve2Sp VeRest{..} = Spacer _restDur NoDynamic
    ve2Sp ve = ve

debounceStaves :: (NE.NonEmpty VoiceEvent,NE.NonEmpty VoiceEvent) -> (NE.NonEmpty VoiceEvent,NE.NonEmpty VoiceEvent)
debounceStaves = bimap NE.fromList NE.fromList . uncurry g . bimap winlist winlist
  where
    winlist = windows 5 . NE.toList
    windows n xs = transpose (take n (tails xs))
    g tves bves = unzip $ zipWith h tves bves
    h tves bves
      | isSpacer t = if any isNoteOrRest (tail tves) then (b,t) else (t,b)
      | isSpacer b = if any isNoteOrRest (tail bves) then (t,b) else (b,t)
      | otherwise = (t,b)
        where
          t = head tves
          b = head bves
          isSpacer VeSpacer {} = True
          isSpacer _ = False
          isNoteOrRest VeNote {} = True
          isNoteOrRest VeRest {} = True
          isNoteOrRest _ = False

genPolyVocs :: Instrument -> KeySignature -> (NE.NonEmpty VoiceEvent,NE.NonEmpty VoiceEvent) -> Voice
genPolyVocs instr keySig (treble,bass) = PolyVoice instr vess
  where
    vess = NE.fromList [veKeySig NE.<| treble, veKeySig NE.<| bass]
    veKeySig = VeKeySignature keySig

cfg2SwirlsScore :: String -> Driver ()
cfg2SwirlsScore title = do
  tups <- cfg2SwirlsTups title (NE.fromList ["voice1"])
  vess <- traverse swirlsTup2VEs tups 
  let clefs = pickInitialClef <$> vess
      vePrss = NE.map debounceStaves $ NE.zipWith splitStaves clefs vess
      voices = neZipWith3 genPolyVocs (_stInstr <$> tups) (_stKey <$> tups) vePrss
  writeScore ("./" <> title <> ".ly") $ Score "no comment" voices
--}

pickNoteClef :: Note -> Clef
pickNoteClef Note{..}
  | (_noteOct,_notePit) <= (COct,E) = Bass
  | otherwise = Treble

swirlsTup2Notes :: SwirlsTup -> Driver [Note]
swirlsTup2Notes SwirlsTup{..} = 
  genSwirl _stDurs _stPitss _stScale (Range _stRange) <&> NE.toList

genPolyVocs :: Instrument -> KeySignature -> (NE.NonEmpty VoiceEvent,NE.NonEmpty VoiceEvent) -> Voice
genPolyVocs instr keySig (treble,bass) = PolyVoice instr vess
  where
    vess = NE.fromList [veKeySig NE.<| treble, veKeySig NE.<| bass]
    veKeySig = VeKeySignature keySig

cfg2SwirlsScore :: String -> Driver ()
cfg2SwirlsScore title = do
  tups <- cfg2SwirlsTups title (NE.fromList ["voice1"])
  notess <- traverse swirlsTup2Notes tups <&> NE.toList
  let bNotePrss :: [[(Bool,Note)]] = map (map (first ((Treble ==) . pickNoteClef) . dupe)) notess
      startClefs :: [Bool] = map (fst . head) bNotePrss
      vePrss = zipWith zipf startClefs bNotePrss
      zipf :: Bool -> [(Bool,Note)] -> ([VoiceEvent],[VoiceEvent])
      zipf b bNotePrs = purgeWin $ foldl foldf ((b,[]),([VeClef Treble],[VeClef Bass])) bNotePrs
      foldf :: ((Bool,[(Bool,Note)]),([VoiceEvent],[VoiceEvent])) -> (Bool,Note) -> ((Bool,[(Bool,Note)]),([VoiceEvent],[VoiceEvent]))
      foldf ((cl,win),(treb,bass)) (cl',not') =
        if cl == cl' && null win
        then ((cl,win),(treb,bass) <> bNote2VePr (cl,not'))
        else
          let win' = win <> [(cl',not')]
          in
            if length win' < 5
            then ((cl,win'),(treb,bass))
            else
              let wins = groupBy (\(a,_) (b,_) -> a == b)  win'
                  cl'' = if length (head wins) == 5 then not cl else cl
              in
                ((cl'',concat $ tail wins),mconcat ((treb,bass):map bNote2VePr (head wins)))
      bNote2VePr :: (Bool,Note) -> ([VoiceEvent],[VoiceEvent])
      bNote2VePr (True,note@Note{..})  = ([VeNote note],[VeSpacer (Spacer _noteDur NoDynamic)])
      bNote2VePr (False,note@Note{..}) = ([VeSpacer (Spacer _noteDur NoDynamic)],[VeNote note])
      purgeWin :: ((Bool,[(Bool,Note)]),([VoiceEvent],[VoiceEvent])) -> ([VoiceEvent],[VoiceEvent])
      purgeWin ((cl,win),(treb,bass)) = mconcat ((treb,bass):(map bNote2VePr (map ((cl,) . snd)  win)))
      
      vePrss' = NE.fromList (map (bimap NE.fromList NE.fromList) vePrss)
      voices = neZipWith3 genPolyVocs (_stInstr <$> tups) (_stKey <$> tups) vePrss'
  writeScore ("./" <> title <> ".ly") $ Score "no comment" voices

-- Essence of transformation.
-- 1) simplify swirlsTup2VEs so instead of being
--   SwirlsTup -> Driver (NE.NonEmpty VoiceEvent)
-- instead, it's:
--   SwirlsTup -> Driver (NE.NonEmpty Note)
-- (what follows is pure and should be pipeline-able)
-- 2) pair up with a Bool to say if it's treble (True) or bass:
--   NE.NonEmpty Note -> NE.NonEmpty (Bool, Note)
-- 3) convert to an ordinary list:
--   NE.NonEmpty (Bool,Note) -> [(Bool,Note)]
-- 4) foldl over into a list of pairs ordered treble, bass
--   [(Bool,Note)] -> [(VoiceEvent,VoiceEvent)]
--   for state/ouput, foldl takes ((Bool,[(Bool,Note)]),[(VoiceEvent,VoiceEvent)]),
--   where (Bool,[(Bool,Note)]) is debounce state, [(VoiceEvent,VoiceEvent)] is
--   output, and input is (Bool,[(Bool,Note)]) and Bool in first part of state indicates
--   current clef initialized from first Bool in [(Bool,Note)] input.
--   if input Bool is the same as debounce Bool
--   then
--     convert input (Bool,Note) to (NoteEvent,NoteEvent) and append to output
--   else
--     append (Bool,Note) to debounce list
--     if length of debounce list is < 5
--     then
--       iterate
--     else
--       group debounce list by Bool
--       if length of first element in [[]] is 5
--       then
--         iterate with debounce state with flipped bool
--           and empty list and all debounce list elements
--           converted and appended to output
--       else
--         iterate with existing debounce state Bool,
--           and concatenation of tail of [[]] for list
--           and convert head of [[]] to output flipping
--           all Bool in (Bool,Note) before converting
