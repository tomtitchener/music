{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Compositions.Swirls.Utils  where

import Control.Applicative ((<|>))
import Control.Lens hiding (pre)
import Control.Monad.Extra (concatMapM)
import Data.Either (isRight)
import Data.Foldable
import Data.Function (on)
import Data.List (elemIndex, findIndex, groupBy, unfoldr)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Sequence (adjust, fromList)
import Data.Tuple (swap)
import Safe (headMay)
import Driver
       (Driver, cfg2Tups, randomElements, searchConfigParam)
import Types
import Utils
type NoteOrRest = Either Note Rest

newtype Range = Range ((Pitch,Octave),(Pitch,Octave)) deriving (Eq, Show)

data SectionConfigTup =
  SectionConfigTupNeutral {
                       _scnReps   :: Int
                      ,_scnVoices :: NE.NonEmpty VoiceConfigTup
                 }
  | SectionConfigTupFadeIn {
                       _scfiFIOrder :: NE.NonEmpty Int
                      ,_scfiVoices :: NE.NonEmpty VoiceConfigTup
                      }
  | SectionConfigTupFadeOut {
                       _scfoFOOrder  :: NE.NonEmpty Int
                      ,_scfoVoices :: NE.NonEmpty VoiceConfigTup
                      }
  | SectionConfigTupXPosePitches { -- [(dur,[(target,xpose)])]
                       _scppDurPOPrs :: NE.NonEmpty (Int,NE.NonEmpty (Pitch,Octave)) -- per VoiceConfigTup
                      ,_scppVoices   :: NE.NonEmpty VoiceConfigTup
                      }

data VoiceConfigTup =
  VoiceConfigTupXPose {
                  _vctxInstr   :: Instrument
                 ,_vctxKey     :: KeySignature
                 ,_vctxScale   :: Scale
                 ,_vctxTime    :: TimeSignature
                 ,_vctxMIntss  :: NE.NonEmpty (NE.NonEmpty (Maybe Int))
                 ,_vctxDurss   :: NE.NonEmpty (NE.NonEmpty Duration)
                 ,_vctxAcctss  :: NE.NonEmpty (NE.NonEmpty Accent)
                 ,_vctxRange   :: ((Pitch,Octave),(Pitch,Octave))
                 }
  | VoiceConfigTupRepeat {
                    _vctrInstr    :: Instrument
                    ,_vctrKey      :: KeySignature
                    ,_vctrScale    :: Scale
                    ,_vctrTime     :: TimeSignature
                    ,_vctrMIntss   :: NE.NonEmpty (NE.NonEmpty (Maybe Int))
                    ,_vctrDurss    :: NE.NonEmpty (NE.NonEmpty Duration)
                    ,_vctrAcctss   :: NE.NonEmpty (NE.NonEmpty Accent)
                    ,_vctrRegister :: (Pitch,Octave)
                    ,_vctrDurVal   :: Int
                 }
  | VoiceConfigTupCanon {
                    _vctcInstr    :: Instrument
                    ,_vctcKey      :: KeySignature
                    ,_vctcScale    :: Scale
                    ,_vctcTime     :: TimeSignature
                    ,_vctcMPitss   :: NE.NonEmpty (NE.NonEmpty (Maybe Pitch))
                    ,_vctcOctss    :: NE.NonEmpty (NE.NonEmpty Octave)
                    ,_vctcDurss    :: NE.NonEmpty (NE.NonEmpty Duration)
                    ,_vctcAcctss   :: NE.NonEmpty (NE.NonEmpty Accent)
                    ,_vctcRegister :: (Pitch,Octave)
                    ,_vctcDurVal   :: Int
                    ,_vctcRotVal   :: Int
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

tup2VoiceConfigTupCanon :: String -> Driver VoiceConfigTup
tup2VoiceConfigTupCanon pre =
      VoiceConfigTupCanon
        <$> searchConfigParam  (pre <> ".instr")
        <*> searchConfigParam  (pre <> ".key")
        <*> searchConfigParam  (pre <> ".scale")
        <*> searchConfigParam  (pre <> ".time")
        <*> searchConfigParam  (pre <> ".mpitss")
        <*> searchConfigParam  (pre <> ".octss")
        <*> searchConfigParam  (pre <> ".durss")
        <*> searchConfigParam  (pre <> ".accentss")
        <*> searchConfigParam  (pre <> ".register")
        <*> searchConfigParam  (pre <> ".durval")
        <*> searchConfigParam  (pre <> ".rotval")

tup2SectionConfigTupNeutral :: String -> NE.NonEmpty String -> Driver SectionConfigTup
tup2SectionConfigTupNeutral section voices =
      SectionConfigTupNeutral
      <$> searchConfigParam (section <> ".reps")
      <*> cfg2Tups cfg2VoiceConfigTup section voices
tup2SectionConfigTupFadeIn :: String -> NE.NonEmpty String -> Driver SectionConfigTup
tup2SectionConfigTupFadeIn section voices =
      SectionConfigTupFadeIn
      <$> searchConfigParam  (section <> ".delays")
      <*> cfg2Tups cfg2VoiceConfigTup section voices
tup2SectionConfigTupFadeOut :: String -> NE.NonEmpty String -> Driver SectionConfigTup
tup2SectionConfigTupFadeOut section voices =
      SectionConfigTupFadeOut
      <$> searchConfigParam  (section <> ".drops")
      <*> cfg2Tups cfg2VoiceConfigTup section voices
tup2SectionConfigTupXPosePitches :: String -> NE.NonEmpty String -> Driver SectionConfigTup
tup2SectionConfigTupXPosePitches section voices =
      SectionConfigTupXPosePitches
      <$> searchConfigParam (section <> ".durxps")
      <*> cfg2Tups cfg2VoiceConfigTup section voices

type2SectionConfigTup :: M.Map String (String -> NE.NonEmpty String -> Driver SectionConfigTup)
type2SectionConfigTup = M.fromList [("neutral",tup2SectionConfigTupNeutral)
                                   ,("fadein" ,tup2SectionConfigTupFadeIn)
                                   ,("fadeout",tup2SectionConfigTupFadeOut)
                                   ,("xpose"  ,tup2SectionConfigTupXPosePitches)]

type2VoiceConfigTup :: M.Map String (String -> Driver VoiceConfigTup)
type2VoiceConfigTup = M.fromList [("xpose" ,tup2VoiceConfigTupXPose)
                                 ,("repeat",tup2VoiceConfigTupRepeat)
                                 ,("canon" ,tup2VoiceConfigTupCanon)]

cfg2VoiceConfigTup :: String -> Driver VoiceConfigTup
cfg2VoiceConfigTup section =
  searchConfigParam (section <> ".vtype") >>= runConfigType
  where
    runConfigType cfgType =
      case M.lookup cfgType type2VoiceConfigTup of
        Nothing -> error $ "cfg2VoiceConfigTup no converter for \"vtype:\" " <> cfgType
        Just tup2VoiceConfigTup -> tup2VoiceConfigTup section

cfg2SectionConfigTup :: String -> NE.NonEmpty String -> Driver SectionConfigTup
cfg2SectionConfigTup section voices =
  searchConfigParam (section <> ".stype") >>= runConfigType
  where
    runConfigType cfgType =
      case M.lookup cfgType type2SectionConfigTup of
        Nothing -> error $ "cfg2VoiceConfigTup no converter for \"stype:\" " <> cfgType
        Just tup2SectionConfigTup -> tup2SectionConfigTup section voices

-- args are path to section, voices per section: "test.section1" ["voice1","voice2","voice3","voice4"]
cfg2VoiceConfigTups :: String -> NE.NonEmpty String -> Driver [VoiceConfigTup]
cfg2VoiceConfigTups section voices = cfg2Tups cfg2VoiceConfigTup section voices <&> NE.toList

-- Unfold repeated transpositions of [[Maybe Pitch]] across Range
-- matching up with repetitions of [[Duration]] and [[Accent] to generate NoteOrRest.
genXPose :: [[Duration]] -> [[Accent]] -> [[Maybe Int]] -> Scale -> Range -> Driver [NoteOrRest]
genXPose durss acctss mIntss scale (Range (start,stop)) = do
  mSteps    <- randomElements mIntss <&> concat
  manyDurs  <- randomElements durss  <&> concat
  manyAccts <- randomElements acctss <&> concat
  let mPOs = unfoldr (unfoldf compareOp) (start,mSteps)
  pure $ zipWith3 mkNoteOrRest mPOs manyDurs manyAccts
  where
    stepOrd   = sum (fromMaybe 0 <$> concat mIntss) `compare` 0
    rangeOrd  = swap stop `compare` swap start
    compareOp = if stepOrd == rangeOrd && stepOrd /= EQ
                then if stepOrd == LT then (<=) else (>=)
                else error $ "invalid step order " <> show stepOrd <> " compared with range order " <> show rangeOrd
    unfoldf cmp (prev,(Just step1):mSteps)
      | swap next `cmp` swap stop = Nothing
      | otherwise = Just (Just next,(next, mSteps))
      where
        next = xp scale prev step1
    unfoldf _ (prev, Nothing:mSteps) = Just (Nothing,(prev, mSteps))
    unfoldf _ steps = error $ "invalid list of steps, (" <> show (fst steps) <> "," <> show (take 10 (snd steps)) <> ")"

-- static means each [Maybe Int] is interpreted with respect to (Pitch,Octave)
-- instead of continuing to transpose from the end of one [Maybe Int] to the next
genStatic :: [[Duration]] -> [[Accent]] -> [[Maybe Int]] -> Scale -> (Pitch,Octave) -> Int -> Driver [NoteOrRest]
genStatic durss acctss mIntss scale start maxDurVal= do
  manyMPOs   <- randomElements mIntss <&> concatMap (mtranspose scale start)
  manyAccts  <- randomElements acctss <&> concat
  manyDurs   <- randomElements durss  <&> concat
  let allDurs  = unfoldr (unfoldDurs maxDurVal) (0,manyDurs)
  pure $ zipWith3 mkNoteOrRest manyMPOs allDurs manyAccts

genCanon :: [[Duration]] -> [[Accent]] -> [[Maybe Pitch]] -> [[Octave]] -> Scale -> (Pitch,Octave) -> Int -> Int -> Driver [NoteOrRest]
genCanon durss acctss mPitss octss scale (startPit,_) maxDurVal rotVal = do
  manyMIntss <- randomElements mPitss <&> fmap (mInts2IntDiffs . fmap (mPitch2MScaleDegree scale) . rotN rotVal)
  manyOcts   <- randomElements octss  <&> concat
  manyDurs   <- randomElements durss  <&> concat
  manyAccts  <- randomElements acctss <&> concat
  let manyPOs    = (,) startPit <$> manyOcts
      manyMPOs   = concat $ zipWith (mtranspose scale) manyPOs manyMIntss
      allDurs  = unfoldr (unfoldDurs maxDurVal) (0,manyDurs)
  pure $ zipWith3 mkNoteOrRest manyMPOs allDurs manyAccts

unfoldDurs :: Int -> (Int, [Duration]) -> Maybe (Duration, (Int, [Duration]))
unfoldDurs maxDurVal (durVal,durs)
  | durVal >= maxDurVal = Nothing
  | otherwise = Just (head durs,(durVal + dur2DurVal (head durs),tail durs))

rotN :: Int -> [a] -> [a]
rotN cnt as
  | cnt >= length as = error $ "rotN cnt: " <> show cnt <> " >= length as " <> show (length as)
  | otherwise = drop cnt as <> take cnt as

mPitch2MScaleDegree :: Scale -> Maybe Pitch -> Maybe Int
mPitch2MScaleDegree _ Nothing = Nothing
mPitch2MScaleDegree Scale{..} (Just pitch) =
  elemIndex pitch pitches <|> err
  where
    pitches = NE.toList _scPitches
    err = error $ "mPitch2ScaleDegree no pitch: " <> show pitch <> " in pitches for scale: " <> show pitches

mInts2IntDiffs :: [Maybe Int] -> [Maybe Int]
mInts2IntDiffs = snd . foldl foldlf (0,[])
  where
    foldlf (prev,ret) Nothing  = (prev,ret <> [Nothing])
    foldlf (prev,ret) (Just i) = (i,ret <> [Just (i - prev)])

mkNoteOrRest :: Maybe (Pitch, Octave) -> Duration -> Accent -> Either Note Rest
mkNoteOrRest (Just (p,o)) d a = Left $ Note p o d (Staccatissimo NE.:| [a]) NoDynamic  NoSwell False -- tbd: magic constant Staccatissimo
mkNoteOrRest Nothing d _ = Right $ Rest d  NoDynamic

voiceConfigTup2NoteOrRests :: VoiceConfigTup -> Driver [NoteOrRest]
voiceConfigTup2NoteOrRests VoiceConfigTupXPose{..} =
  genXPose (nes2arrs _vctxDurss) (nes2arrs _vctxAcctss) (nes2arrs _vctxMIntss) _vctxScale (Range _vctxRange)
voiceConfigTup2NoteOrRests VoiceConfigTupRepeat{..} =
  genStatic (nes2arrs _vctrDurss) (nes2arrs _vctrAcctss) (nes2arrs _vctrMIntss) _vctrScale _vctrRegister _vctrDurVal
voiceConfigTup2NoteOrRests VoiceConfigTupCanon{..} =
  genCanon (nes2arrs _vctcDurss) (nes2arrs _vctcAcctss) (nes2arrs _vctcMPitss) (nes2arrs _vctcOctss) _vctcScale _vctcRegister _vctcDurVal _vctcRotVal

sectionConfigTup2NoteOrRests :: SectionConfigTup -> Driver [[NoteOrRest]]
sectionConfigTup2NoteOrRests (SectionConfigTupNeutral reps voiceConfigTups) =
  traverse (concatMapM voiceConfigTup2NoteOrRests) voiceConfigTupss
  where
    voiceConfigTupss = concat . replicate reps . (:[]) <$> NE.toList voiceConfigTups
sectionConfigTup2NoteOrRests (SectionConfigTupFadeIn _ voiceConfigTups) =
  traverse voiceConfigTup2NoteOrRests (NE.toList voiceConfigTups)
sectionConfigTup2NoteOrRests (SectionConfigTupFadeOut _ voiceConfigTups)
  = traverse voiceConfigTup2NoteOrRests (NE.toList voiceConfigTups)
sectionConfigTup2NoteOrRests (SectionConfigTupXPosePitches _ voiceConfigTups) =
  traverse voiceConfigTup2NoteOrRests (NE.toList voiceConfigTups)

fiTest :: [Int] -> [[Bool]]
fiTest is = snd $ foldl' foldlf (initMap,initRet) is
  where
    initMap = M.fromList $ (,False) <$> is
    initRet = replicate (length is) []
    foldlf (m,rets) i = (m',zipWith app rets r)
      where
        m' = M.insert i True m
        r  = snd <$> M.toAscList m'
        app as a = as <> [a]

genPolyVocs :: Instrument -> KeySignature -> TimeSignature -> (NE.NonEmpty VoiceEvent,NE.NonEmpty VoiceEvent) -> Voice
genPolyVocs instr keySig timeSig (treble,bass) = PolyVoice instr vess
  where
    vess = NE.fromList [veKeySig NE.<| veTimeSig NE.<| treble, veKeySig NE.<| veTimeSig NE.<| bass]
    veKeySig = VeKeySignature keySig
    veTimeSig = VeTimeSignature timeSig

-- to avoid cluttering score with repeats of the same dynamic, accent,
tagFirstNotes :: (Accent,Dynamic) -> ([VoiceEvent],[VoiceEvent]) -> ([VoiceEvent],[VoiceEvent])
tagFirstNotes (acc,dyn) = bimap tagFirstNote tagFirstNote
  where
    tagFirstNote ves = maybe ves (`tagNoteForIdx` ves) (findIndex isNote ves)
    tagNoteForIdx idx = toList . adjust tagNote idx .  fromList
    tagNote (VeNote (Note p o d _ _ swell tie)) = VeNote (Note p o d (singleton acc) dyn swell tie)
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
    flush ((Nothing,pending),vesPr) -- never hit winLen Left Note in a row, try again decrementing winLen by one
      | winLen == 0 = error $ "splitNoteOrRests flush but no clef for pending " <> show pending
      | otherwise   = vesPr <> splitNoteOrRests (winLen - 1) pending
    flush ((Just clef,pending),vesPr) = vesPr <> genVesPr clef pending
    foldlf :: ((Maybe Clef,[NoteOrRest]),([VoiceEvent],[VoiceEvent])) -> [NoteOrRest] -> ((Maybe Clef,[NoteOrRest]),([VoiceEvent],[VoiceEvent]))
    foldlf ((mCl,pending),vesPr) nOrRs
      | allRests || cntNotes nOrRs < winLen = ((mCl,pending <> nOrRs),vesPr)
      | otherwise = case mCl of
                      Nothing -> ((Just nextCl,[]),vesPr <> genVesPr nextCl (pending <> nOrRs))
                      Just cl -> ((Just nextCl,[]),vesPr <> genVesPr cl pending <> genVesPr nextCl nOrRs)
      where
        allRests = all isRest nOrRs
        isRest (Right _) = True
        isRest (Left _)  = False
        cntNotes = length . filter (not . isTiedNote)
        isTiedNote (Left Note {..}) = _noteTie
        isTiedNote (Right r)        = error $ "isTiedNote unexpected rest: " <> show r
        nextCl  = case headMay (filter (not . isRest) nOrRs) of
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

