{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compose {--(cfg2Driver, genSwirl)--} where

-- import Debug.Trace (trace)

import Control.Monad (zipWithM)
import Data.Foldable 
import Data.Bifunctor (bimap)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (zipWith4, groupBy, findIndex)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Semigroup (stimesMonoid)
import Data.Sequence (adjust, fromList)
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
import Lily

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

-- tbd: this picks a single permutation of the motifs then repeats it over and over
-- so the voice moves pretty consistently and fairly rapidly in one direction until
-- it reaches the target boundary
-- if I change the rhythm to uniform eighth notes then I hear the repetitions clearly
-- if I keep the odd count--as in 4 8 8 8 4 8 8 8--then it gets more regular, but
-- the mismatch with the count of notes in the motifs keeps things off balance
-- spread things out, maybe stringing together a series of shuffles of the motifs?
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

{--
to test via ghci:
 a) create [Note] test case that traverses treble/bass boundaries
 b) call renderStaves with Bool and [(Pitch,Octave)] where Bool says True to start with treble, else Bass
    e.g.: > renderStaves 5 False [(B,EightVBOct),(D,COct),(E,COct),(F,COct),(G,COct),(A,COct),(B,COct),(F,COct),(G,COct),(A,COct),(B,COct)]
          > (" sp 8 sp 8 sp 8 f' 8 g' 8 a' 8 b' 8 f' 8 g' 8 a' 8 b' 8"," b 8 d' 8 e' 8 sp 8 sp 8 sp 8 sp 8 sp 8 sp 8 sp 8 sp 8")

test cases:
 a) span from bass to treble
    renderStaves 5 False [(B,EightVBOct),(D,COct),(E,COct),(F,COct),(G,COct),(A,COct),(B,COct),(F,COct),(G,COct),(A,COct),(B,COct)]
    (" sp 8 sp 8 sp 8 f' 8 g' 8 a' 8 b' 8 f' 8 g' 8 a' 8 b' 8"," b 8 d' 8 e' 8 sp 8 sp 8 sp 8 sp 8 sp 8 sp 8 sp 8 sp 8")

 b) span from treble to bass:
    renderStaves 5 True [(B,COct),(A,COct),(G,COct),(F,COct),(E,COct),(D,COct),(B,EightVBOct),(B,EightVBOct),(A,EightVBOct),(G,EightVBOct),(F,EightVBOct)]
    (" b' 8 a' 8 g' 8 f' 8 sp 8 sp 8 sp 8 sp 8 sp 8 sp 8 sp 8"," sp 8 sp 8 sp 8 sp 8 e' 8 d' 8 b 8 b 8 a 8 g 8 f 8") 


 c) debounce temporary from treble to bass:
    renderStaves 5 False [(B,EightVBOct),(D,COct),(E,COct),(F,COct),(G,EightVBOct),(A,COct),(B,EightVBOct),(F,COct),(G,COct),(A,COct),(B,COct),(B,COct),(A,COct)]
    (" sp 8 sp 8 sp 8 sp 8 sp 8 sp 8 sp 8 f' 8 g' 8 a' 8 b' 8 b' 8 a' 8"," b 8 d' 8 e' 8 f' 8 g 8 a' 8 b 8 sp 8 sp 8 sp 8 sp 8 sp 8 sp 8")

 d) wander back and forth:
    renderStaves 1 False [(B,EightVBOct),(D,COct),(E,COct),(F,COct),(G,EightVBOct),(A,COct),(B,EightVBOct),(F,COct),(G,COct),(A,COct),(B,COct),(B,COct),(A,COct)]
    (" sp 8 sp 8 sp 8 f' 8 sp 8 a' 8 sp 8 f' 8 g' 8 a' 8 b' 8 b' 8 a' 8"," b 8 d' 8 e' 8 sp 8 g 8 sp 8 b 8 sp 8 sp 8 sp 8 sp 8 sp 8 sp 8")

 e) purge at end:
    renderStaves 4 False [(B,EightVBOct),(D,COct),(E,COct),(F,COct),(G,COct),(A,COct)]
    (" sp 8 sp 8 sp 8 sp 8 sp 8 sp 8"," b 8 d' 8 e' 8 f' 8 g' 8 a' 8")
-}

-- testing only:
prNoteOrSpacer :: VoiceEvent -> String
prNoteOrSpacer (VeSpacer (Spacer d _)) = "sp " <> toLily d
prNoteOrSpacer (VeNote (Note p o d _ _ _ _)) = toLily p <> toLily o <> " " <> toLily d
prNoteOrSpacer _ = ""

-- testing only:
renderStaves :: Int -> Bool -> [(Pitch,Octave)] -> (String,String)
renderStaves n b = bimap f f . zipBAndBNotePrs n  b  . notes2BNotePrs . map mkNote'
  where
    f = unwords . map prNoteOrSpacer

-- testing only:
mkNote' :: (Pitch,Octave) -> Note
mkNote' (p,o) = Note p o EDur Staccatissimo NoDynamic  NoSwell False

notes2BNotePrs :: [Note] -> [(Bool,Note)]
notes2BNotePrs = map (first ((Treble ==) . pickNoteClef) . dupe)

-- combine:
--   a) length of debounce buffer
--   b) starting clef (True -> Treble, False -> Bass)
--   c) list of (Clef,Note) pairs
-- to yield:
--   (Treble,Bass) pair of [VoiceEvent]
-- for rendering in piano staff.
--
-- debounce Treble -> Bass and Bass -> Treble transitions by suppressing
-- transition until count in first argument consecutive notes in the new clef
zipBAndBNotePrs :: Int -> Bool -> [(Bool,Note)] -> ([VoiceEvent],[VoiceEvent])
zipBAndBNotePrs n b = purgeWin . foldl foldf ((b,[]),([VeClef Treble],[VeClef Bass]))
  where
    purgeWin :: ((Bool,[(Bool,Note)]),([VoiceEvent],[VoiceEvent])) -> ([VoiceEvent],[VoiceEvent])
    purgeWin ((cl,win),(treb,bass)) = mconcat ((treb,bass):map (bNote2VePr . ((cl,) . snd)) win)
    bNote2VePr :: (Bool,Note) -> ([VoiceEvent],[VoiceEvent])
    bNote2VePr (True,note@Note{..})  = ([VeNote note],[VeSpacer (Spacer _noteDur NoDynamic)])
    bNote2VePr (False,note@Note{..}) = ([VeSpacer (Spacer _noteDur NoDynamic)],[VeNote note])
    foldf :: ((Bool,[(Bool,Note)]),([VoiceEvent],[VoiceEvent])) -> (Bool,Note) -> ((Bool,[(Bool,Note)]),([VoiceEvent],[VoiceEvent]))
    foldf ((cl,win),(treb,bass)) (cl',not')
      | cl == cl' && null win = ((cl,win),(treb,bass) <> bNote2VePr (cl,not'))
      | length win' < n = ((cl,win'),(treb,bass))
      | otherwise = case groupBy (\(a,_) (b',_) -> a == b') win' of
                      -- as are in new clef, but still followed by bs: flip clef and emit as, emit bs as is, and continue with rest
                      (as:bs:rest) -> ((cl,concat rest),mconcat ((treb,bass):map (bNote2VePr . first not) as <> map bNote2VePr bs))
                      -- all as with new clef in debounce buffer, continue with other new clef, empty debounce buffer, emit as as is
                      [as] -> ((not cl,[]),mconcat ((treb,bass):map bNote2VePr as))
                      -- shouldn't ever get here
                      [] -> error "zipBAndBNotePrs:foldf programming error" -- ((cl,[]),(treb,bass))
       where
         win' = win <> [(cl', not')]

-- to avoid cluttering score with repeats of the same dynamic, accent, 
tagFirstNotes :: Note -> ([VoiceEvent],[VoiceEvent]) -> ([VoiceEvent],[VoiceEvent])
tagFirstNotes (Note _ _ _ acc dyn swell tie) = bimap tagFirstNote tagFirstNote
  where
    tagFirstNote ves = toList $ adjust tagNote idx $ fromList ves
      where
        idx = fromMaybe (error $ "tagFirstNote, no note in [VoiceEvent]: " <> show ves) $ findIndex isNote ves
    tagNote (VeNote (Note p o d _ _ _ _)) = VeNote (Note p o d acc dyn swell tie)
    tagNote ve = error $ "tagNote, VoiceEvent is not VeNote: " <> show ve
    isNote VeNote {} = True
    isNote _ = False

-- targLen and vesLen are in 128th notes
-- targLen is target length, vesLen is actual length
mkTotDur :: Int -> Int -> ([VoiceEvent],[VoiceEvent]) -> ([VoiceEvent],[VoiceEvent])
mkTotDur targLen vesLen
  | targLen == vesLen = id
  | targLen > vesLen = bimap addLen addLen
  | otherwise = error $ "mkTotDur programming error, targLen: " <> show targLen <> " < vesLen: " <> show vesLen
  where
    addend = DurationSum (targLen - vesLen)
    addLen ves = ves ++ map (VeRest . flip Rest NoDynamic) (durSum2Durs addend)

-- tbd: extend last bar by remaining beats, need to specify time signature, count bars 
cfg2SwirlsScore :: String -> Driver ()
cfg2SwirlsScore title = do
  let voicesNames = NE.fromList ["voice1","voice2"]
      cntVoices = NE.length voicesNames
  tups <- cfg2SwirlsTups title voicesNames
  notess <- traverse swirlsTup2Notes tups <&> NE.toList
  let bNotePrss :: [[(Bool,Note)]] = map notes2BNotePrs notess
      startClefs :: [Bool] = map (fst . head) bNotePrss
      winLens :: [Int] = replicate cntVoices 5
      veLens :: [Int] = map (sum . map (dur2DurVal . _noteDur . snd)) bNotePrss
      noteTags = replicate cntVoices (Note C COct EDur NoAccent PPP NoSwell False)
      voices = zipWith3 zipBAndBNotePrs winLens startClefs bNotePrss
               & zipWith tagFirstNotes noteTags
               & zipWith (mkTotDur (maximum veLens)) veLens
               & NE.fromList . map (bimap NE.fromList NE.fromList)
               & neZipWith3 genPolyVocs (_stInstr <$> tups) (_stKey <$> tups)
  writeScore ("./" <> title <> ".ly") $ Score "no comment" voices

