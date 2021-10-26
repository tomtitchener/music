{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compose (cfg2Driver) where

import Control.Monad (zipWithM)
import Data.Foldable
import Data.Bifunctor (bimap)
import Data.Either (isRight)
import Data.Function ((&),on)
import Data.Functor ((<&>))
import Data.List (zipWith4, groupBy, findIndex)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Semigroup (stimesMonoid)
import Data.Sequence (adjust, fromList)
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

------------
-- Swirls --
------------

type NoteOrRest = Either Note Rest

newtype Range = Range ((Pitch, Octave),(Pitch, Octave)) deriving (Eq, Show)

-- Implicitly works against chromatic scale, fmap normPitch gives Maybe 0..11.
-- Just Int maps to a new interval for a new Pitch, Nothing maps to Rest.
-- Pitch in [Maybe Pitch] is from chromatic scale so stays within one octave
-- where C (Bs, Dff) is 0.
genMIntervalList :: [Maybe Pitch] -> [Maybe Int]
genMIntervalList = reverse . snd . foldl f (0,[]) . fmap (fmap normPitch)
  where
    f (prev,ret) (Just normPit) = (normPit, Just (normPit - prev):ret)
    f (prev,ret) Nothing        = (prev,    Nothing:ret)

-- tbd: this picks a single permutation of the motifs then repeats it over and over
-- so the voice moves pretty consistently and fairly rapidly in one direction until
-- it reaches the target boundary
-- if I change the rhythm to uniform eighth notes then I hear the repetitions clearly
-- if I keep the odd count--as in 4 8 8 8 4 8 8 8--then it gets more regular, but
-- the mismatch with the count of notes in the motifs keeps things off balance
-- spread things out, maybe stringing together a series of shuffles of the motifs?
genSwirl :: NE.NonEmpty Duration -> NE.NonEmpty Accent -> NE.NonEmpty (NE.NonEmpty (Maybe Pitch)) -> Scale -> Range -> Driver (NE.NonEmpty NoteOrRest)
genSwirl durs accts motifs scale (Range (start,stop)) = do
  mSteps <- randomizeList motifs' <&> concatMap genMIntervalList
  let stepOrd = sum (map (fromMaybe 0) mSteps) `compare` 0
      rangeOrd = swap stop `compare` swap start
      compareOp = if stepOrd == rangeOrd && stepOrd /= EQ
                  then if stepOrd == LT then (<=) else (>=)
                  else error $ "invalid step order " <> show stepOrd <> " compared with range order " <> show rangeOrd
      manyMSteps = concat $ repeat mSteps
      manyAccts = NE.cycle accts
      manyDurs = NE.cycle durs
      mPOs :: NE.NonEmpty (Maybe (Pitch,Octave)) = NE.unfoldr (unfoldf compareOp) (start,manyMSteps)
  pure $ neZipWith3 mkNoteOrRest mPOs manyDurs manyAccts
  where
    motifs' = nes2arrs motifs
    unfoldf cmp (prev,(Just step1):(Just step2):mSteps)
      | swap nextnext `cmp` swap stop = (Just next, Nothing)
      | otherwise = (Just next, Just (next, Just step2:mSteps))
      where
        next = xp scale prev step1
        nextnext = xp scale next step2
    unfoldf _ (prev,(Just step1):Nothing:mSteps) =
      (Just next, Just (next, Nothing:mSteps))
      where
        next = xp scale prev step1
    unfoldf _ (prev, Nothing:mSteps) =
      (Nothing, Just (prev, mSteps))
    unfoldf _ steps = error $ "invalid list of steps, (" <> show (fst steps) <> "," <> show (take 10 (snd steps)) <> ")"
    mkNoteOrRest (Just (p,o)) d a = Left $ Note p o d (Staccatissimo NE.:| [a]) NoDynamic  NoSwell False -- tbd: magic constant Stacctissimo
    mkNoteOrRest Nothing d _ = Right $ Rest d  NoDynamic

data SwirlsTup = SwirlsTup {_stInstr  :: Instrument
                           ,_stKey    :: KeySignature
                           ,_stScale  :: Scale
                           ,_stTime   :: TimeSignature
                           ,_stMPitss :: NE.NonEmpty (NE.NonEmpty (Maybe Pitch))
                           ,_stDurs   :: NE.NonEmpty Duration
                           ,_stAccts  :: NE.NonEmpty Accent
                           ,_stRange  :: ((Pitch,Octave),(Pitch,Octave))
                           ,_stGhosts :: NE.NonEmpty (Int,Int)
                           } deriving Show

cfg2SwirlsTup :: String -> Driver SwirlsTup
cfg2SwirlsTup pre =
  SwirlsTup
    <$> getConfigParam (pre <> ".instr")
    <*> getConfigParam (pre <> ".key")
    <*> getConfigParam (pre <> ".scale")
    <*> getConfigParam (pre <> ".time")
    <*> getConfigParam (pre <> ".mpitss")
    <*> getConfigParam (pre <> ".durs")
    <*> getConfigParam (pre <> ".accents")
    <*> getConfigParam (pre <> ".range")
    <*> getConfigParam (pre <> ".ghosts")

cfg2SwirlsTups :: String -> NE.NonEmpty String -> Driver (NE.NonEmpty SwirlsTup)
cfg2SwirlsTups = cfg2Tups cfg2SwirlsTup

pickNoteClef :: Note -> Clef
pickNoteClef Note{..}
  | (_noteOct,_notePit) <= (COct,E) = Bass
  | otherwise = Treble

swirlsTup2NoteOrRests :: SwirlsTup -> Driver [NoteOrRest]
swirlsTup2NoteOrRests SwirlsTup{..} =
  genSwirl _stDurs _stAccts _stMPitss _stScale (Range _stRange) <&> NE.toList

genPolyVocs :: Instrument -> KeySignature -> TimeSignature -> (NE.NonEmpty VoiceEvent,NE.NonEmpty VoiceEvent) -> Voice
genPolyVocs instr keySig timeSig (treble,bass) = PolyVoice instr vess
  where
    vess = NE.fromList [veKeySig NE.<| veTimeSig NE.<| treble, veKeySig NE.<| veTimeSig NE.<| bass]
    veKeySig = VeKeySignature keySig
    veTimeSig = VeTimeSignature timeSig

-- to avoid cluttering score with repeats of the same dynamic, accent, 
tagFirstNotes :: Note -> ([VoiceEvent],[VoiceEvent]) -> ([VoiceEvent],[VoiceEvent])
tagFirstNotes (Note _ _ _ acc dyn _ _) = bimap tagFirstNote tagFirstNote
  where
    tagFirstNote ves = maybe ves (`tagNoteForIdx` ves) (findIndex isNote ves)
    tagNoteForIdx idx = toList . adjust tagNote idx .  fromList
    tagNote (VeNote (Note p o d _ _ swell tie)) = VeNote (Note p o d acc dyn swell tie)
    tagNote ve = error $ "tagNote, VoiceEvent is not VeNote: " <> show ve
    isNote VeNote {} = True
    isNote _ = False

-- maxLen and vesLen are in 128th notes
-- maxLen is target length so all voices are equal length
-- vesLen is actual length maybe same as maxLen
mkTotDur :: Int -> Int -> TimeSignature -> ([VoiceEvent],[VoiceEvent]) -> ([VoiceEvent],[VoiceEvent])
mkTotDur maxLen vesLen timeSig =
  bimap addLenToVes addLenToVes
  where
    beatLen = dur2DurVal (timeSig2Denom timeSig)
    barLen  = timeSig2Num timeSig * beatLen
    remBar  = if maxLen `rem` barLen == 0 then 0 else barLen - (maxLen `rem` barLen)
    addLen  = if maxLen > vesLen then (maxLen - vesLen) + remBar else remBar
    addLenToVes ves = ves ++ map spacerOrRest (addEndDurs timeSig vesLen addLen)
      where
        spacerOrRest = if isSpacer (last ves) then VeSpacer . flip Spacer NoDynamic else VeRest . flip Rest NoDynamic
        isSpacer VeSpacer {} = True
        isSpacer _ = False

tagTempo :: Tempo -> NE.NonEmpty Voice -> NE.NonEmpty Voice
tagTempo tempo (v1 NE.:| rest) = tagVoice v1 NE.:| rest
  where
    tagVoice ::  Voice -> Voice
    tagVoice PitchedVoice{..} = PitchedVoice _ptvInstrument (VeTempo tempo NE.<| _ptvVoiceEvents)
    tagVoice PercussionVoice{..} = PercussionVoice _pcvInstrument (VeTempo tempo NE.<| _pcvVoiceEvents)
    tagVoice (PolyVoice instr (ves NE.:| vess)) = PolyVoice instr ((VeTempo tempo NE.<| ves) NE.:| vess)
    tagVoice (VoiceGroup (v1' NE.:| r)) = VoiceGroup (tagVoice v1' NE.:| r)

genLNotes :: Int -> Int -> TimeSignature -> Note -> [Either Note Rest]
genLNotes addLen curLen timeSig note =
  map (dur2LeftNote True) (init durs) <> [dur2LeftNote False (last durs)]
  where
    durs = addEndDurs timeSig curLen addLen
    dur2LeftNote tie dur = Left $ note { _noteDur = dur, _noteTie = tie }

genRRests :: Int -> Int -> TimeSignature -> [Either Note Rest]
genRRests addLen curLen timeSig =
  map dur2RightRest durs
  where
    durs = addEndDurs timeSig curLen addLen
    dur2RightRest = Right . flip Rest NoDynamic

squashNoteOrRests :: [(Int,Int)] -> TimeSignature -> [NoteOrRest] -> [NoteOrRest]
squashNoteOrRests prs timeSig =
  flush . foldl foldlf ((0,0),[]) . chunkByPairCounts prs . map stripNoteOrRest
  where
    flush ((_,0),nOrRs)                   = nOrRs
    flush ((curDurVal,prevRestVal),nOrRs) = nOrRs <> genRRests prevRestVal curDurVal timeSig
    foldlf ((curDurVal,prevRestsDurVal),ret) (rests,notes) =
      maybe restsRet notesRet $ mFirstNote notes
      where
        restsRet      = ((curDurVal
                         ,prevRestsDurVal
                          + restsDurVal
                          + notesDurVal)
                        ,ret)
        notesRet note = ((curDurVal
                          + prevRestsDurVal
                          + restsDurVal
                          + notesDurVal
                         ,0),
                         ret
                         <> genRRests (prevRestsDurVal + restsDurVal) curDurVal timeSig
                         <> genLNotes notesDurVal (curDurVal + prevRestsDurVal + restsDurVal) timeSig note)
        restsDurVal   = nOrRs2DurVal rests
        notesDurVal   = nOrRs2DurVal notes
    mFirstNote (Left note:_) = Just note
    mFirstNote _             = Nothing

nOrRs2DurVal :: [NoteOrRest] -> Int
nOrRs2DurVal = sum . map nOrR2DurVal

nOrR2DurVal :: NoteOrRest -> Int
nOrR2DurVal (Left Note{..}) = dur2DurVal _noteDur
nOrR2DurVal (Right Rest{..}) = dur2DurVal _restDur

-- When generating ghost voices, don't want accents, dynamics, swells, or ties in any Note
stripNoteOrRest :: NoteOrRest -> NoteOrRest
stripNoteOrRest (Left Note{..}) = Left $ Note _notePit _noteOct _noteDur (singleton NoAccent) NoDynamic NoSwell False
stripNoteOrRest (Right rest) = Right rest

alignNoteOrRestsDurations :: TimeSignature -> [NoteOrRest] -> [NoteOrRest]
alignNoteOrRestsDurations timeSig =
  snd . foldl' foldlf (0,[]) . groupBy ((==) `on` isRight)
  where
    foldlf (curLen,ret) allRests@((Right _):_) =
      (curLen + addLen,ret <> newRests)
      where
        addLen = nOrRs2DurVal allRests
        durs = addEndDurs timeSig curLen addLen
        newRests = map (Right . flip Rest NoDynamic) durs
    foldlf (curLen,ret) allNotes@((Left _):_) =
      foldl' foldlf' (curLen,ret) allNotes
      where
        foldlf' (curLen',ret') (Left note@Note{..}) =
          (curLen' + addLen,ret' <> (stripAccents . addTies $ newNotes))
          where
            durs = addEndDurs timeSig curLen' addLen
            newNotes = map (Left . setNoteDur note) durs
            setNoteDur note' dur = note' { _noteDur = dur }
            addLen = dur2DurVal _noteDur
            addTies nOrRs 
              | null nOrRs        = error "alignNoteOrRestsDurations addTies empty list"
              | 1 == length nOrRs = nOrRs
              | otherwise         = map addTie (init nOrRs) <> [last nOrRs] -- XXX want to strip accents out of all but first
            addTie :: NoteOrRest -> NoteOrRest
            addTie (Left note') = Left (note' {_noteTie = True})
            addTie (Right _)   = error "alignNoteOrRestsDurations addTie unexpected rest"
            stripAccents :: [NoteOrRest] -> [NoteOrRest]
            stripAccents [] = []
            stripAccents [nOrR] = [nOrR]
            stripAccents (nOrR:nOrRs) = nOrR:map stripNoteOrRest nOrRs
        foldlf' (_,_) (Right rest) = error $ "alignNoteOrRestsDurations foldlf' unexpected Rest: " <> show rest
    foldlf (_,_) l = error $ "alignNoteOrRestsDurations unexpected list: " <> show l

cfg2SwirlsScore :: String -> Driver ()
cfg2SwirlsScore title = do
  let voicesNames = NE.fromList ["voice1","voice2","voice3"]
      cntVoices = NE.length voicesNames
  tempo <- getConfigParam (title <> ".global.tempo") <&> (\(i :: Int) -> TempoDur QDur $ fromIntegral i)
  tups <- cfg2SwirlsTups title voicesNames
  noteOrRestss <- traverse swirlsTup2NoteOrRests tups <&> NE.toList
  let -- regular voices, first apportion durations by position in beat and bar
      winLens       = replicate cntVoices 5 -- tbd: magic constant
      veLens        = map nOrRs2DurVal noteOrRestss
      rNoteOrRestss = zipWith alignNoteOrRestsDurations (NE.toList (_stTime <$> tups)) noteOrRestss
      vePrss        = zipWith splitNoteOrRests winLens rNoteOrRestss
      noteTags      = replicate cntVoices (Note C COct EDur (singleton Staccatissimo) PPP NoSwell False)
      voices        = pipeline tempo noteTags veLens tups vePrss
      -- ghost voices
      timeSigs      = NE.toList (_stTime <$> tups)
      manyIntPrss   = map cycle (nes2arrs (_stGhosts <$> tups))
      gNoteOrRestss = zipWith3 squashNoteOrRests manyIntPrss timeSigs noteOrRestss
      gWinLens      = replicate cntVoices 1 -- tbd: magic constant
      gVePrss       = zipWith splitNoteOrRests gWinLens gNoteOrRestss
      gVoices       = pipeline tempo noteTags veLens tups gVePrss
  writeScore ("./" <> title <> ".ly") $ Score "no comment" (voices <> gVoices)
  where
    pipeline :: Tempo -> [Note] -> [Int] -> NE.NonEmpty SwirlsTup -> [([VoiceEvent],[VoiceEvent])] -> NE.NonEmpty Voice
    pipeline tempo noteTags veLens tups vePrss =
      zipWith tagFirstNotes noteTags vePrss
      & zipWith3 (mkTotDur (maximum veLens)) veLens (NE.toList (_stTime <$> tups))
      & NE.fromList . map (bimap NE.fromList NE.fromList)
      & neZipWith4 genPolyVocs (_stInstr <$> tups) (_stKey <$> tups) (_stTime <$> tups)
      & tagTempo tempo

-- 5 for winLen to wait for row of 5 Note with same clef to determine treble vs. bass, 1 for winLen for squashed notes.
splitNoteOrRests :: Int -> [NoteOrRest] -> ([VoiceEvent],[VoiceEvent])
splitNoteOrRests winLen = flush . foldl' foldlf ((Nothing,[]),([VeClef Treble],[VeClef Bass])) . groupBy equalClefs
  where
    flush :: ((Maybe Clef,[NoteOrRest]),([VoiceEvent],[VoiceEvent])) -> ([VoiceEvent],[VoiceEvent])
    flush ((Just _,[]),vesPr) = vesPr
    flush ((Nothing,pending),_) = error $ "splitNoteOrRests flush but no clef for pending " <> show pending
    flush ((Just clef,pending),vesPr) = vesPr <> genVesPr clef pending
    foldlf :: ((Maybe Clef,[NoteOrRest]),([VoiceEvent],[VoiceEvent])) -> [NoteOrRest] -> ((Maybe Clef,[NoteOrRest]),([VoiceEvent],[VoiceEvent]))
    foldlf ((mCl,pending),vesPr) nOrRs
      | allRests || cntNotes nOrRs < winLen = ((mCl,pending <> nOrRs),vesPr)
      | otherwise = ((Just cl,[]),vesPr <> genVesPr cl (pending <> nOrRs))
      where
        allRests :: Bool
        allRests = all isRest nOrRs
        isRest :: NoteOrRest -> Bool
        isRest (Right _) = True
        isRest (Left _)  = False
        cntNotes :: [NoteOrRest] -> Int
        cntNotes = length . filter (not . isTiedNote)
        isTiedNote :: NoteOrRest -> Bool
        isTiedNote (Left Note {..}) = _noteTie
        isTiedNote (Right r) = error $ "isTiedNote unexpected rest: " <> show r
        cl = case headMay (filter (not . isRest) nOrRs) of
               Just (Left note) -> pickNoteClef note
               Just (Right _) -> error $ "splitNoteOrRests unexpected Rest in nOrRs " <> show nOrRs
               Nothing -> error "splitNoteOrRests unexpected empty list for nOrRs"
    genVesPr :: Clef -> [NoteOrRest] -> ([VoiceEvent],[VoiceEvent])
    genVesPr cl pending
      | cl == Treble = (map genNoteOrRestVEFromNoteOrRest pending,map genSpacerVEFromNoteOrRest pending)
      | otherwise    = (map genSpacerVEFromNoteOrRest pending,map genNoteOrRestVEFromNoteOrRest pending)
      where
        genNoteOrRestVEFromNoteOrRest = either VeNote VeRest
        genSpacerVEFromNoteOrRest = either note2Spacer rest2Spacer
        note2Spacer Note{..} = VeSpacer (Spacer _noteDur _noteDyn)
        rest2Spacer Rest{..} = VeSpacer (Spacer _restDur _restDyn)
    equalClefs :: NoteOrRest -> NoteOrRest -> Bool
    equalClefs (Left n1) (Left n2) = pickNoteClef n1 == pickNoteClef n2
    equalClefs (Right _) (Right _) = True
    equalClefs _          _        = False

-- Musical things to fix:
-- a) Effect of accumulation of sustained pitches doesn't build to
--    the sort of hazy background I had in mind, it's too sparse
--    with 1-to-1 match of ghost voices with regular voices.

--------------------
-- Hook with main --
--------------------

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
