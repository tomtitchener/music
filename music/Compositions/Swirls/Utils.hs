{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Compositions.Swirls.Utils  where

import Control.Lens hiding (pre)
import Data.Either (isRight)
import Data.Foldable
import Data.Function (on)
import Data.List (findIndex, groupBy)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Sequence (adjust, fromList)
import Data.Tuple (swap)
import Safe (headMay)

import Driver (Driver, cfg2Tups, randomizeList, searchConfigParam)
import Types
import Utils

type NoteOrRest = Either Note Rest

newtype Range = Range ((Pitch, Octave),(Pitch, Octave)) deriving (Eq, Show)

data SwirlsTup = SwirlsTup {_stInstr  :: Instrument
                           ,_stKey    :: KeySignature
                           ,_stScale  :: Scale
                           ,_stTime   :: TimeSignature
                           ,_stMPitss :: NE.NonEmpty (NE.NonEmpty (Maybe Pitch))
                           ,_stDurs   :: NE.NonEmpty Duration
                           ,_stAccts  :: NE.NonEmpty Accent
                           ,_stRange  :: ((Pitch,Octave),(Pitch,Octave))
                           -- ,_stGhosts :: NE.NonEmpty (Int,Int)
                           } deriving Show

cfg2SwirlsTup :: String -> Driver SwirlsTup
cfg2SwirlsTup pre =
  SwirlsTup
    <$> searchConfigParam (pre <> ".instr")
    <*> searchConfigParam (pre <> ".key")
    <*> searchConfigParam (pre <> ".scale")
    <*> searchConfigParam (pre <> ".time")
    <*> searchConfigParam (pre <> ".mpitss")
    <*> searchConfigParam (pre <> ".durs")
    <*> searchConfigParam (pre <> ".accents")
    <*> searchConfigParam (pre <> ".range")
--  <*> searchConfigParam (pre <> ".ghosts")

cfg2SwirlsTups :: String -> NE.NonEmpty String -> Driver (NE.NonEmpty SwirlsTup)
cfg2SwirlsTups = cfg2Tups cfg2SwirlsTup

-- Implicitly works against chromatic scale, fmap normPitch gives Maybe 0..11.
-- Just Int maps to a new interval for a new Pitch, Nothing maps to Rest.
-- Pitch in [Maybe Pitch] is from chromatic scale so stays within one octave
-- where C (Bs, Dff) is 0.  When ordering of range is LT, then all intervals
-- are with respect to C above, else from C below.
genMIntervalList :: Ordering -> [Maybe Pitch] -> [Maybe Int]
genMIntervalList rangeOrd =
  reverse . snd . foldl f (0,[]) . (fmap . fmap) ((+ off) . normPitch)
  where
    f (prev,ret) (Just curr) = (curr, Just (curr - prev):ret)
    f (prev,ret) Nothing     = (prev, Nothing:ret)
    off = case rangeOrd of
            LT -> -12
            GT -> 0
            EQ -> 0

-- tbd: this picks a single permutation of the motifs then repeats it over and over
-- so the voice moves pretty consistently and fairly rapidly in one direction until
-- it reaches the target boundary
-- if I change the rhythm to uniform eighth notes then I hear the repetitions clearly
-- if I keep the odd count--as in 4 8 8 8 4 8 8 8--then it gets more regular, but
-- the mismatch with the count of notes in the motifs keeps things off balance
-- spread things out, maybe stringing together a series of shuffles of the motifs?
genSwirl :: NE.NonEmpty Duration -> NE.NonEmpty Accent -> NE.NonEmpty (NE.NonEmpty (Maybe Pitch)) -> Scale -> Range -> Driver (NE.NonEmpty NoteOrRest)
genSwirl durs accts motifs scale (Range (start,stop)) = do
  mSteps <- randomizeList (nes2arrs motifs) <&> concatMap (genMIntervalList rangeOrd)
  let stepOrd = sum (fromMaybe 0 <$> mSteps) `compare` 0
      compareOp = if stepOrd == rangeOrd && stepOrd /= EQ
                  then if stepOrd == LT then (<=) else (>=)
                  else error $ "invalid step order " <> show stepOrd <> " compared with range order " <> show rangeOrd
      manyMSteps = concat $ repeat mSteps
      manyAccts = NE.cycle accts
      manyDurs = NE.cycle durs
      mPOs :: NE.NonEmpty (Maybe (Pitch,Octave)) = NE.unfoldr (unfoldf compareOp) (start,manyMSteps)
  pure $ neZipWith3 mkNoteOrRest mPOs manyDurs manyAccts
  where
    rangeOrd = swap stop `compare` swap start
    unfoldf cmp (prev,(Just step1):(Just step2):mSteps)
      | swap nextnext `cmp` swap stop = (Just next, Nothing)
      | otherwise = (Just next, Just (next, Just step2:mSteps))
      where
        next = xp scale prev step1
        nextnext = xp scale next step2
    unfoldf cmp (prev,(Just step1):Nothing:mSteps)
      | swap next `cmp` swap stop = (Nothing,Nothing)
      | otherwise = (Just next, Just (next, Nothing:mSteps))
      where
        next = xp scale prev step1
    unfoldf _ (prev, Nothing:mSteps) =
      (Nothing, Just (prev, mSteps))
    unfoldf _ steps = error $ "invalid list of steps, (" <> show (fst steps) <> "," <> show (take 10 (snd steps)) <> ")"
    mkNoteOrRest (Just (p,o)) d a = Left $ Note p o d (Staccatissimo NE.:| [a]) NoDynamic  NoSwell False -- tbd: magic constant Staccatissimo
    mkNoteOrRest Nothing d _ = Right $ Rest d  NoDynamic

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
    addLenToVes ves = ves ++ (spacerOrRest <$> addEndDurs timeSig vesLen addLen)
      where
        spacerOrRest = if isSpacer (last ves) then VeSpacer . flip Spacer NoDynamic else VeRest . flip Rest NoDynamic

isSpacer :: VoiceEvent -> Bool
isSpacer VeSpacer {} = True
isSpacer _           = False

tagTempo :: Tempo -> NE.NonEmpty Voice -> NE.NonEmpty Voice
tagTempo tempo (v1 NE.:| rest) = tagVoice v1 NE.:| rest
  where
    tagVoice ::  Voice -> Voice
    tagVoice PitchedVoice{..} = PitchedVoice _ptvInstrument (VeTempo tempo NE.<| _ptvVoiceEvents)
    tagVoice PercussionVoice{..} = PercussionVoice _pcvInstrument (VeTempo tempo NE.<| _pcvVoiceEvents)
    tagVoice (PolyVoice instr (ves NE.:| vess)) = PolyVoice instr ((VeTempo tempo NE.<| ves) NE.:| vess)
    tagVoice (VoiceGroup (v1' NE.:| r)) = VoiceGroup (tagVoice v1' NE.:| r)

-- tied notes have no accent, no dynamic,
stripNoteOrRest :: Bool -> NoteOrRest -> NoteOrRest
stripNoteOrRest tie (Left Note{..}) = Left $ Note _notePit _noteOct _noteDur (singleton NoAccent) NoDynamic NoSwell tie
stripNoteOrRest _ (Right rest) = Right rest

{--
genLNotes :: Int -> Int -> TimeSignature -> Note -> [Either Note Rest]
genLNotes addLen curLen timeSig note =
  (dur2LeftNote True <$> init durs) <> [dur2LeftNote False (last durs)]
  where
    durs = addEndDurs timeSig curLen addLen
    dur2LeftNote tie dur = Left (note { _noteDur = dur, _noteTie = tie })

genRRests :: Int -> Int -> TimeSignature -> [Either Note Rest]
genRRests addLen curLen timeSig =
  dur2RightRest <$> durs
  where
    durs = addEndDurs timeSig curLen addLen
    dur2RightRest = Right . flip Rest NoDynamic
-- using counts in pair (#rests, #notes) combine #rests of NoteOrRest together into one rest
-- followed by #notes of notes of NoteOrRest together into one note *if* the first NoteOrRest
-- is a (Left Note), else accumulate duration of #notes and duration of #rests with duration
-- of previously accumulated rests and continue
squashNoteOrRests :: [(Int,Int)] -> TimeSignature -> [NoteOrRest] -> [NoteOrRest]
squashNoteOrRests prs timeSig =
  flush . foldl foldlf ((0,0),[]) . chunkByPairCounts prs . fmap (stripNoteOrRest False)
  where
    flush ((_,0),nOrRs)                       = nOrRs
    flush ((curDurVal,prevRestsDurVal),nOrRs) = nOrRs <> genRRests prevRestsDurVal curDurVal timeSig
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
--}

nOrRs2DurVal :: [NoteOrRest] -> Int
nOrRs2DurVal = sum . fmap nOrR2DurVal

nOrR2DurVal :: NoteOrRest -> Int
nOrR2DurVal (Left Note{..}) = dur2DurVal _noteDur
nOrR2DurVal (Right Rest{..}) = dur2DurVal _restDur

-- apportion [Right Rest] or [Left Note] durations according to place in bar given time signature
-- 1) map [NoteOrRest] to [[NoteOrRest]] by [[Left Note]..[Right Rest]]
-- 2) fold over uniform [NoteOrRest] into [NoteOrRest] according to current position
--    in total [NoteorRest] by 1/128th notes mapped into position within bar, keeping
--    track of position by first element in pair, output in second element of pair
--    2a) for [Right Rest], sum vals for all contiguous rests, call addEndDurs given
--        time signature and current position and map all to Right Rest
--    2b) for [Left Note], fold again over each calling addEndDurs to break into tied
--         Left Note given time signature and current position
alignNoteOrRestsDurations :: TimeSignature -> [NoteOrRest] -> [NoteOrRest]
alignNoteOrRestsDurations timeSig =
  snd . foldl' foldlf (0,[]) . groupBy ((==) `on` isRight)
  where
    foldlf (curLen,ret) allRests@((Right _):_) =
      (curLen + addLen,ret <> newRests)
      where
        addLen = nOrRs2DurVal allRests
        durs = addEndDurs timeSig curLen addLen
        newRests = Right . flip Rest NoDynamic <$> durs
    foldlf (curLen,ret) allNotes@((Left _):_) =
      foldl' foldlf' (curLen,ret) allNotes
      where
        foldlf' (curLen',ret') (Left note@Note{..}) =
          (curLen' + addLen,ret' <> stripAccents newNotes)
          where
            addLen = dur2DurVal _noteDur
            durs = addEndDurs timeSig curLen' addLen
            newNotes = Left . (\dur -> note {_noteDur = dur}) <$> durs
            stripAccents nOrRs
              | length nOrRs < 2 = nOrRs
              | otherwise        = (stripNoteOrRest True <$> init nOrRs) <> [stripNoteOrRest False (last nOrRs)]
        foldlf' (_,_) (Right rest) = error $ "alignNoteOrRestsDurations foldlf' unexpected Rest: " <> show rest
    foldlf (_,_) l = error $ "alignNoteOrRestsDurations unexpected list: " <> show l

-- for given debounce length, split [NoteOrRest] into ([VoiceEvent],[VoiceEvent]) to be rendered as piano staff
-- *where* debounce length tells how many contiguous Treble or Bass notes determine a change in staff,
-- *allowing for* [Right Rest] instances to ride along carrying current clef
-- note: for inactive clef, need to generate spacer events to maintain synchronization between Treble and Bass
-- 5 for winLen to wait for row of 5 Note with same clef to determine treble vs. bass, 1 for winLen for squashed notes.
splitNoteOrRests :: Int -> [NoteOrRest] -> ([VoiceEvent],[VoiceEvent])
splitNoteOrRests winLen =
  -- 1) chunk [NoteOrRest] into [[NoteOrRest]] where each inner [] is [Left Note] or [Right Rest]
  -- 2) fold over [[NoteOrRest]] pending [Right Rest] until next [Left Note] determines Clef
  -- 3) flush any pending [Right Rest] at end
  flush . foldl' foldlf ((Nothing,[]),([VeClef Treble],[VeClef Bass])) . groupBy equalClefs
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
        allRests = all isRest nOrRs
        isRest (Right _) = True
        isRest (Left _)  = False
        cntNotes = length . filter (not . isTiedNote)
        isTiedNote (Left Note {..}) = _noteTie
        isTiedNote (Right r)        = error $ "isTiedNote unexpected rest: " <> show r
        cl = case headMay (filter (not . isRest) nOrRs) of
               Just (Left note) -> pickNoteClef note
               Just (Right _)   -> error $ "splitNoteOrRests unexpected Rest in nOrRs " <> show nOrRs
               Nothing          -> error "splitNoteOrRests unexpected empty list for nOrRs"
    genVesPr :: Clef -> [NoteOrRest] -> ([VoiceEvent],[VoiceEvent])
    genVesPr cl pending
      | cl == Treble = (either VeNote VeRest <$> pending,either note2Spacer rest2Spacer <$> pending)
      | otherwise    = (either note2Spacer rest2Spacer <$> pending,either VeNote VeRest <$> pending)
      where
        note2Spacer Note{..} = VeSpacer (Spacer _noteDur _noteDyn)
        rest2Spacer Rest{..} = VeSpacer (Spacer _restDur _restDyn)
    equalClefs (Left n1) (Left n2) = pickNoteClef n1 == pickNoteClef n2
    equalClefs (Right _) (Right _) = True
    equalClefs _          _        = False

