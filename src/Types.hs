{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE DerivingVia      #-}

{- | Music types for translating to and from Lilypond strings.
     Atomic types (Pitch, Scale, Octave, Accent, Duration, Dynamic, etc.)
     don't require context to be rendered to or parsed from their Lilypond
     equivalents.
     Aggregates (Note, Rhythm, Rest, Chord, etc.) directly mirror Lilypond
     equivalents as well.
     Rendering of a meaningful score requires awareness of context that's
     not necessarily part of the aggregate types.  For example, Duration
     values are atoms only, with aggregate durations accumulated as ties
     between notes and decomposition of e.g. a Duration that extends over
     a bar into the component notes as would have to decomposed into tied
     notes with in the context of the meter and the curent offset within the bar.
-}

module Types where

import Control.Lens
import Data.List.NonEmpty
import Data.Natural

-- Total duration is (denom / num) * (sum durations / unit) * unit
data DurTuplet = DurTuplet {
   _durtupNumerator    :: Int
  ,_durtupDenominator  :: Int
  ,_durtupUnitDuration :: Duration
  ,_durtupDurations    :: NonEmpty DurationVal
  } deriving Show

data PitOct = PitOct Pitch Octave deriving (Eq, Show)

instance Ord PitOct where
  compare (PitOct p1 o1) (PitOct p2 o2) = (o1,p1) `compare` (o2,p2)

-- Derive Eq and Ord from symbols for simplicity.
-- In practice some symbols are enharmonically
-- equivalent, e.g. Bs, C, Dff and there could
-- be a type class to reflect that.
data Pitch = Bs | C   | Bss | Dff | Cs
           | Df | Css | D   | Eff | Ds
           | Ef | Fff | Dss | E   | Ff
           | Es | F   | Gff | Ess | Fs
           | Gf | Fss | G   | Aff | Gs
           | Af | Gss | A   | Bff | As
           | Bf | Cff | Ass | B   | Cf
  deriving (Eq, Ord, Show, Enum, Bounded)

newtype Scale = Scale { _scPitches :: NonEmpty Pitch }
  deriving (Eq, Ord, Show)

data Octave = TwentyNineVBOct | TwentyTwoVBOct | FifteenVBOct | EightVBOct | COct | EightVAOct | FifteenVAOct | TwentyTwoVAOct
  deriving (Eq, Ord, Show, Enum, Bounded)

-- Allows for arithmetic on durations without respect to integral values used in notation as represented by Duration type.
newtype DurationVal = DurationVal { fromVal :: Int }
  deriving (Eq, Ord, Show)
  deriving Num via Int

mkDurationVal :: Int -> DurationVal
mkDurationVal i
  | i <= 0 = error $ "mkDurationVal negative or zero value: " <> show i
  | otherwise = DurationVal i

data Duration =  HTEDur | SFDur | DSFDur | TSDur | DTSDur | SDur | DSDur | EDur | DEDur | QDur | DQDur | HDur | DHDur | WDur | DWDur
  deriving (Eq, Ord, Show, Enum, Bounded)

data Accent = Marcato | Tenuto | Staccatissimo | Staccato | Accent | Portato | NoAccent
  deriving (Eq, Ord, Show, Enum, Bounded)

data Dynamic = PPPPP | PPPP | PPP | PP | Piano | MP | MF | Forte | FF | FFF | FFFF | FFFFF | FP | SF | SFF | SP | SPP | SFZ | RFZ | NoDynamic
  deriving (Eq, Ord, Show, Enum, Bounded)

data Swell = Crescendo | Decrescendo | Espressivo | SwellStop
  deriving (Eq, Ord, Show, Enum, Bounded)

data Sustain = SustainOn | SustainOff 
  deriving (Eq, Ord, Show, Enum, Bounded)

data Sostenuto = SostenutoOn | SostenutoOff 
  deriving (Eq, Ord, Show, Enum, Bounded)

data Slur = SlurOn | SlurOff 
  deriving (Eq, Ord, Show, Enum, Bounded)

data Control =
    CtrlAccent     { _ctrlAccent     :: Accent    }
  | CtrlDynamic    { _ctrlDynamic    :: Dynamic   }
  | CtrlSwell      { _ctrlSwell      :: Swell     }
  | CtrlSustain    { _ctrlSustain    :: Sustain   }
  | CtrlSostenuto  { _ctrlSostenuto  :: Sostenuto }
  | CtrlSlur       { _ctrlSlur       :: Slur      }
  | CtrlAnnotation { _ctrlAnnotation :: String    }
  deriving (Eq, Ord, Show)

data Pan    = Pan Float    deriving (Eq,Ord,Show) -- [-1.0 .. 1.0]
data Reverb = Reverb Float deriving (Eq,Ord,Show) -- [0.0 .. 1.0]
data Chorus = Chorus Float deriving (Eq,Ord,Show) -- [0.0 .. 1.0]

data MidiControl =
    MidiCtrlPan     { _mctrlPan     :: Pan     }
  | MidiCtrlReverb  { _mctrlReverb  :: Reverb  }
  | MidiCtrlChorus  { _mctrlChorus  :: Chorus  }
  | MidiCtrlAccent  { _mctrlAccent  :: Accent  }
  | MidiCtrlDynamic { _mctrlDynamic :: Dynamic }
  deriving (Eq, Ord, Show)

data Note = Note { _notePit :: Pitch, _noteOct :: Octave, _noteDur :: DurationVal, _noteMidiCtrls :: [MidiControl], _noteCtrls :: [Control], _noteTie :: Bool}
  deriving (Eq, Ord, Show)

data Rest = Rest { _restDur :: DurationVal, _restCtrls :: [Control] }
  deriving (Eq, Ord, Show)

data Spacer = Spacer { _spacerDur :: DurationVal, _spacerDyn :: Dynamic, _spacerAnn :: String }
  deriving (Eq, Ord, Show)

data Rhythm = Rhythm { _rhythmInstr :: String, _rhythmDur :: DurationVal, _rhythmMidiCtrls :: [MidiControl], _rhythmCtrls :: [Control] } -- TBD: _rhythmTie :: Bool
  deriving (Eq, Ord, Show)

-- NB: when rendering, double _tupDenom for Lilypond
data Tuplet = Tuplet { _tupNum :: Int, _tupDenom :: Int, _tupDur :: Duration, _tupNotes :: NonEmpty VoiceEvent }
  deriving (Eq, Ord, Show)

data Chord = Chord { _chordPitOctPairs :: NonEmpty PitOct, _chordDur :: DurationVal, _chordMidiCtrls :: [MidiControl], _chordCtrls :: [Control], _chordTie :: Bool }
  deriving (Eq, Ord, Show)

data Clef = Bass8VB | Bass | Tenor | Alto | Treble | Treble8VA
  deriving (Eq, Ord, Show, Enum, Bounded)

data Tempo =
    TempoText   { _ttText :: String }
  | TempoDur    { _tdDur :: Duration, _tdPerMin :: Natural }
  | TempoLong   { _tlText :: String, _tlDur :: Duration, _tlPerMin :: Natural  }
  | TempoRange  { _trDur :: Duration, _trPerMinLo :: Natural, _trPerMinHi :: Natural  }
  deriving (Eq, Ord, Show)

-- ChordTremolo must have same Duration for both chords.
-- ChordTremolo chords can have accents, swells and dynamics, which aren't renedered via midi.
data Tremolo =
    NoteTremolo { _ntrNote :: Note }
  | ChordTremolo { _ctrLeftChord :: Chord, _ctrRightChord :: Chord }
  deriving (Eq, Ord, Show)

data Mode = Major | Minor
  deriving (Eq, Ord, Show, Enum, Bounded)

data KeySignature = KeySignature { _kspit :: Pitch, _ksmode :: Mode }
  deriving (Eq, Ord, Show)

data TimeSignature = TimeSignatureSimple { _tsNum :: Int, _tsDenom :: Duration }
                   | TimeSignatureGrouping { _tsgGroups :: NonEmpty Int, _tsgNum :: Int, _tsgDenom :: Duration }
  deriving (Eq, Ord, Show)

data VoiceEvent =
    VeNote { _veNote :: Note }
  | VeRest { _veRest :: Rest }
  | VeSpacer { _veSpacer :: Spacer }
  | VeRhythm { _veRhythm :: Rhythm }
  | VeTuplet { _veTuplet :: Tuplet }
  | VeChord { _veChord :: Chord }
  | VeTremolo { _veTremolo :: Tremolo }
  | VeClef { _veClef :: Clef }
  | VeTempo { _veTempo :: Tempo }
  | VeKeySignature { _veKeySig :: KeySignature }
  | VeTimeSignature { _veTimeSig :: TimeSignature }
   deriving (Eq, Ord, Show)

makeLenses ''Note
makeLenses ''Rest
makeLenses ''Spacer
makeLenses ''Rhythm
makeLenses ''Tuplet
makeLenses ''Chord
makeLenses ''Tempo
makeLenses ''Tremolo
makeLenses ''TimeSignature
makeLenses ''VoiceEvent

-- From MIDI
data Instrument =
       AcousticGrand |           Contrabass |         LeadFfths |
       BrightAcoustic |          TremoloStrings |     LeadBassPlusLead |
       ElectricGrand |           PizzicatoStrings |   PadNewAge |
       HonkyTonk |               OrchestralHarp |     PadWarm |
       ElectricPiano1 |          Timpani |            PadPolysynth |
       ElectricPiano2 |          StringEnsemble1 |    PadChoir |
       Harpsichord |             StringEnsemble2 |    PadBowed |
       Clav |                    Synthstrings1 |      PadMetallic |
       Celesta |                 Synthstrings2 |      Halo |
       Glockenspiel |            ChoirAahs |          Sweep |
       MusicBox |                VoiceOohs |          Rain |
       Vibraphone |              SynthVoice |         Soundtrack |
       Marimba |                 OrchestraHit |       Crystal |
       Xylophone |               Trumpet |            Atmosphere |
       TubularTells |            Trombone |           Bbrightness |
       Dulcimer |                Tuba |               Goblins |
       DrawbarOrgan |            MutedTrumpet |       Echoes |
       PercussiveOrgan |         FrenchHorn |         SciFi |
       RockOrgan |               BrassSection |       Sitar |
       ChurchOrgan |             Synthbrass1 |        Banjo |
       ReedOrgan |               Synthbrass2 |        Shamisen |
       Accordion |               SopranoSax |         Koto |
       Harmonica |               AltoSax |            Kalimba |
       Concertina |              TenorSax |           Bagpipe |
       AcousticGuitarNylon |     BaritoneSax |        Fiddle |
       AcousticGuitarSteel |     Oboe |               Shanai |
       ElectricGuitarJazz |      EnglishHorn |        TinkleBell |
       ElectricGuitarClean |     Bassoon |            Agogo |
       ElectricGuitarMuted |     Clarinet |           SteelDrums |
       OverdrivenGuitar |        Piccolo |            Woodblock |
       DistortedGuitar |         Flute |              TaikoDrum |
       GuitarHarmonics |         Recorder |           MelodicTom |
       AcousticBass |            PanFlute |           SynthDrum |
       ElectricBassFinger |      BlownBottle |        ReverseCymbal |
       ElectricBassPick |        Shakuhachi |         GuitarFretNoise |
       FretlessBass |            Whistle |            BreathNoise |
       SlapBass1 |               Ocarina |            Seashore |
       SlapBass2 |               LeadSquare |         BirdTweet |
       SynthBass1 |              LeadSawtooth |       TelephoneRing |
       SynthBass2 |              LeadCalliope |       Helicopter |
       Violin |                  LeadChiff |          Applause |
       Viola |                   LeadCharang |        Gunshot |
       Cello |                   LeadVoice
  deriving (Eq, Ord, Show, Enum, Bounded)

-- Lilypond formats:  what I seem to have here is:
-- 1) Mono voice, with only one staff per [VoiceEvent], divided between pitched and percussion,
--    though percussion is unexplored and maybe needs a special version of VoiceEvent?
-- 2) SplitStaff voice, with one [VoiceEvent] where Lilypond divides the events between two staffs.
-- 3) Keyboard voice, with ([VoiceEvent],[VoiceEvent]) divided explicitly between two staffs.
-- 4) Group of voices, a recursive type with a list of the other voices including possibly a subgroup
--    with a list of voices e.g. for a wind section, probably will never be used?
--
-- To date, I have only ever generated scores using the SplitStaffVoice, which uses the Lilypond
-- autoChange control, after trying to arrange a single voice across multiple bass and treble myself.
-- But planning for right and left hands in the configuration file starts to make the split staff
-- choice difficult, as you need to have one stream of Lilypond events with multiple lines in it.
-- What I could have would be a tag in the config file per voice to select the translation method?
--
-- Don't forget Pan setting e.g. after \new Staff as in \tag #'midi \set Staff.midiPanPosition [-1.0..1.0]
-- Seems like I need this on a per Voice basis, though intuitively, it feels like a post-processing sort of
-- trick, once you have the Voices corralled in a Score?  But the translation of Score to Lilypond file
-- doesn't work that way, if there was a post-processing step, it'd have to be Lilypond -> Lilypond, and
-- I'm certain I don't want to write a Lilypond parser.  Seems like the best thing is to tuck it in as
-- a Maybe Double field in the inclusive range [-1.0..1.0].  For simplicity, it's best to have a single
-- Pan setting even for aggregate voices like PolyVoice, which means repeating the same Pan setting for
-- each of the inner list of VoiceEvent.  For VoiceGroup, I can just map the Pan setting for each 
-- embedded Voice instance.
--
data Voice =
  PitchedVoice      { _ptvInstrument :: Instrument, _ptvVoiceEvents :: NonEmpty VoiceEvent }
  | PercussionVoice { _pcvInstrument :: Instrument, _pcvVoiceEvents :: NonEmpty VoiceEvent }
  | SplitStaffVoice { _ssvInstrument :: Instrument, _ssvVoiceEvents :: NonEmpty VoiceEvent }
  | KeyboardVoice   { _kbvInstrument :: Instrument, _kbvVoiceEvents :: (NonEmpty VoiceEvent,NonEmpty VoiceEvent) }
  | PolyVoice       { _povInstrument :: Instrument, _povVoiceEvents :: NonEmpty (NonEmpty VoiceEvent) }
  | VoiceGroup      { _vgVoices :: NonEmpty Voice }
  deriving (Eq, Ord, Show)

data Score = Score { _scoreTitle :: String, _scoreSeed :: String,  _scoreVoices :: NonEmpty Voice }
  deriving (Eq, Ord, Show)

makeLenses ''DurTuplet

-- synonyms

type DurValOrDurTuplet = Either DurationVal DurTuplet

type DurValAccOrDurTupletAccs = Either (DurationVal,Accent) (DurTuplet,[Accent])

type IntOrInts = Either Int [Int]

type PitOctOrNEPitOcts =  Either PitOct (NonEmpty PitOct)

type PitOctOrPitOcts = Either PitOct [PitOct]

type Range = (PitOct,PitOct)

-- What's parsed from YAML file into SectionConfigExp type

type NoteRestOrChordNETuple = (Maybe PitOctOrNEPitOcts,DurationVal,Accent)

type TupletNETuple = (NonEmpty (Maybe PitOctOrNEPitOcts),DurTuplet,NonEmpty Accent)

type NoteDurOrNoteDurNETup = Either NoteRestOrChordNETuple TupletNETuple

-- Equivalents with NonEmpty replaced by []

type NoteRestOrChordTuple = (Maybe PitOctOrPitOcts,DurationVal,Accent)

type TupletTuple = ([Maybe PitOctOrPitOcts],DurTuplet,[Accent])

type NoteDurOrNoteDurTup = Either NoteRestOrChordTuple TupletTuple

-- Interval equivalents 

type NoteRestOrChordIsTuple = (Maybe IntOrInts,DurationVal,Accent)

type TupletIsTuple = ([Maybe IntOrInts],DurTuplet,[Accent])

type NoteDurOrNoteDurIsTup = Either NoteRestOrChordIsTuple TupletIsTuple

