{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compositions.Swirls.Utils  where

import Control.Lens hiding (pre)
import Data.Either (isRight)
import Data.Foldable
import Data.Function (on)
import Data.List (findIndex, groupBy, unfoldr)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.Sequence (adjust, fromList)
import Data.Tuple (swap)
import Safe (headMay)

import Driver
       (Driver, cfg2Tups, randomizeList, searchConfigParam, searchMConfigParam)
import Types
import Utils

type NoteOrRest = Either Note Rest

newtype Range = Range ((Pitch,Octave),(Pitch,Octave)) deriving (Eq, Show)

data VoiceConfigTup =
  VoiceConfigTupXPose {_vctxInstr   :: Instrument
                 ,_vctxKey     :: KeySignature
                 ,_vctxScale   :: Scale
                 ,_vctxTime    :: TimeSignature
                 ,_vctxMIntss  :: NE.NonEmpty (NE.NonEmpty (Maybe Int))
                 ,_vctxDurss   :: NE.NonEmpty (NE.NonEmpty Duration)
                 ,_vctxAcctss  :: NE.NonEmpty (NE.NonEmpty Accent)
                 ,_vctxRange   :: ((Pitch,Octave),(Pitch,Octave))
                 }
  | VoiceConfigTupRepeat {_vctrInstr    :: Instrument
                    ,_vctrKey      :: KeySignature
                    ,_vctrScale    :: Scale
                    ,_vctrTime     :: TimeSignature
                    ,_vctrMIntss   :: NE.NonEmpty (NE.NonEmpty (Maybe Int))
                    ,_vctrDurss    :: NE.NonEmpty (NE.NonEmpty Duration)
                    ,_vctrAcctss   :: NE.NonEmpty (NE.NonEmpty Accent)
                    ,_vctrRegister :: (Pitch,Octave)
                    ,_vctrDurVal   :: Int
                 }

tup2VoiceConfigTupXPose :: String -> Driver VoiceConfigTup
tup2VoiceConfigTupXPose pre =
      VoiceConfigTupXPose
        <$> searchConfigParam  (pre <> ".instr")
        <*> searchConfigParam  (pre <> ".key")
        <*> searchConfigParam  (pre <> ".scale")
        <*> searchConfigParam  (pre <> ".time")
        <*> searchConfigParam  (pre <> ".mintss")
        <*> searchConfigParam  (pre <> ".durss")
        <*> searchConfigParam  (pre <> ".accentss")
        <*> searchConfigParam  (pre <> ".range")

tup2VoiceConfigTupRepeat :: String -> Driver VoiceConfigTup
tup2VoiceConfigTupRepeat pre =
      VoiceConfigTupRepeat
        <$> searchConfigParam  (pre <> ".instr")
        <*> searchConfigParam  (pre <> ".key")
        <*> searchConfigParam  (pre <> ".scale")
        <*> searchConfigParam  (pre <> ".time")
        <*> searchConfigParam  (pre <> ".mintss")
        <*> searchConfigParam  (pre <> ".durss")
        <*> searchConfigParam  (pre <> ".accentss")
        <*> searchConfigParam  (pre <> ".register")
        <*> searchConfigParam  (pre <> ".durval")

type2ConfigTup :: M.Map String (String -> Driver VoiceConfigTup)
type2ConfigTup = M.fromList [("xpose",tup2VoiceConfigTupXPose)
                            ,("repeat",tup2VoiceConfigTupRepeat)]

cfg2VoiceConfigTup :: String -> Driver VoiceConfigTup
cfg2VoiceConfigTup pre = do
  mConfigType <- searchMConfigParam (pre <> ".type")
  case mConfigType of
    Nothing -> error "cfg2VoiceConfigTup no \"type\" keyword in voice block"
    Just cfgType -> case M.lookup cfgType type2ConfigTup of
      Nothing -> error $ "cfg2VoiceConfigTup no converter for \"type:\" " <> cfgType
      Just tup2VoiceConfigTup -> tup2VoiceConfigTup pre

cfg2VoiceConfigTups :: String -> NE.NonEmpty String -> Driver [VoiceConfigTup]
cfg2VoiceConfigTups pre keys = cfg2Tups cfg2VoiceConfigTup pre keys <&> NE.toList

-- Unfold random transpositions of [[Maybe Pitch]] across Range, matching up 
-- with random transpositions of [[Duration]] and [[Accent] to generate NoteOrRest.
genXPose :: [[Duration]] -> [[Accent]] -> [[Maybe Int]] -> Scale -> Range -> Driver [NoteOrRest]
genXPose durss acctss mIntss scale (Range (start,stop)) = 
  let rangeOrd  = (compare `on` swap) stop start
      stepOrd   = sum (fromMaybe 0 <$> concat mIntss) `compare` 0
      compareOp = if stepOrd == rangeOrd && stepOrd /= EQ
                  then if stepOrd == LT then (<=) else (>=)
                  else error $ "invalid step order " <> show stepOrd <> " compared with range order " <> show rangeOrd
      go (start',ret) = do
        (durs,accts,mInts) <- (,,) <$> nexts durss <*> nexts acctss <*> nexts mIntss
        let mPOs      = unfoldr (unfoldf compareOp) (start',mInts)
            start''   = fromJust . last . filter isJust $ mPOs
            ret'      = ret <> zipWith3 mkNoteOrRest mPOs (cycle durs) (cycle accts)
        if length mPOs == length mInts
        then go (start'',ret')
        else pure ret'
        where
          nexts xss = randomizeList xss <&> concat
          unfoldf cmp (prev,(Just step):mSteps)
            | swap next `cmp` swap stop = Nothing
            | otherwise = Just (Just next,(next, mSteps))
            where
              next = xp scale prev step
          unfoldf _ (prev, Nothing:mSteps) = Just (Nothing,(prev, mSteps))
          unfoldf _ (_,[]) = Nothing
  in
    go (start,[])

-- static means each [Maybe Int] is interpreted with respect to (Pitch,Octave)
-- instead of continuing to transpose from the end of one [Maybe Int] to the next
genStatic :: [[Duration]] -> [[Accent]] -> [[Maybe Int]] -> Scale -> (Pitch,Octave) -> Int -> Driver [NoteOrRest]
genStatic durss acctss mIntss scale start maxDurVal= do
  manyMPOs   <- randomizeList mIntss <&> cycle . concatMap (mtranspose scale start)
  manyDurs   <- randomizeList durss  <&> cycle . concat
  manyAccts  <- randomizeList acctss <&> cycle . concat
  let allDurs  = unfoldr unfoldDurs (0,manyDurs)
  pure $ zipWith3 mkNoteOrRest manyMPOs allDurs manyAccts
  where
    unfoldDurs (durVal,durs)
      | durVal >= maxDurVal = Nothing
      | otherwise = Just (head durs,(durVal + dur2DurVal (head durs),tail durs))

mkNoteOrRest :: Maybe (Pitch, Octave) -> Duration -> Accent -> Either Note Rest
mkNoteOrRest (Just (p,o)) d a = Left $ Note p o d (Staccatissimo NE.:| [a]) NoDynamic  NoSwell False -- tbd: magic constant Staccatissimo
mkNoteOrRest Nothing d _ = Right $ Rest d  NoDynamic

configTup2NoteOrRests :: VoiceConfigTup -> Driver [NoteOrRest]
configTup2NoteOrRests VoiceConfigTupXPose{..} =
  genXPose (nes2arrs _vctxDurss) (nes2arrs _vctxAcctss) (nes2arrs _vctxMIntss) _vctxScale (Range _vctxRange)
configTup2NoteOrRests VoiceConfigTupRepeat{..} =
  genStatic (nes2arrs _vctrDurss) (nes2arrs _vctrAcctss) (nes2arrs _vctrMIntss) _vctrScale _vctrRegister _vctrDurVal

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
    -- must be we never hit winLen Left Note in a row, try again decrementing winLen by onea
    flush ((Nothing,pending),_)
      | winLen == 0 = error $ "splitNoteOrRests flush but no clef for pending " <> show pending
      | otherwise   = splitNoteOrRests (winLen - 1) pending
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
    pickNoteClef :: Note -> Clef
    pickNoteClef Note{..}
      | (_noteOct,_notePit) <= (COct,E) = Bass
      | otherwise = Treble

{--
Graveyard:

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

