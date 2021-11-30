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

module Types (Pitch (..)
             ,Scale (..)
             ,Octave (..)
             ,Duration (..)
             ,Accent (..)
             ,Dynamic (..)
             ,Swell (..)
             ,Note (..)
             ,Rhythm (..)
             ,Rest (..)
             ,Spacer (..)
             ,Tuplet (..)
             ,Chord (..)
             ,Clef (..)
             ,Tempo (..)
             ,Tremolo (..)
             ,Mode (..)
             ,KeySignature (..)
             ,TimeSignature (..)
             ,VoiceEvent (..)
             ,Instrument (..)
             ,Voice (..)
             ,Score (..)
             ) where

import Data.List.NonEmpty
import Data.Natural

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

data Duration =  HTEDur | SFDur | DSFDur | TSDur | DTSDur | SDur | DSDur | EDur | DEDur | QDur | DQDur | HDur | DHDur | WDur | DWDur
  deriving (Eq, Ord, Show, Enum, Bounded)

data Accent = Marcato | Tenuto | Staccatissimo | Staccato | Accent | Portato | NoAccent
  deriving (Eq, Ord, Show, Enum, Bounded)

data Dynamic = PPPPP | PPPP | PPP | PP | Piano | MP | MF | Forte | FF | FFF | FFFF | FFFFF | FP | SF | SFF | SP | SPP | SFZ | RFZ | NoDynamic
  deriving (Eq, Ord, Show, Enum, Bounded)

data Swell = Crescendo | Decrescendo | Espressivo | SwellStop | NoSwell
  deriving (Eq, Ord, Show, Enum, Bounded)

data Note = Note { _notePit :: Pitch, _noteOct :: Octave, _noteDur :: Duration, _noteAccs :: NonEmpty Accent, _noteDyn :: Dynamic, _noteSwell :: Swell, _noteAnn :: String,  _noteTie :: Bool}
  deriving (Eq, Ord, Show)

data Rhythm = Rhythm { _rhythmInstr :: String, _rhythmDur :: Duration, _rhythmAccs :: NonEmpty Accent, _rhythmDyn :: Dynamic, _rhythmSwell :: Swell }
  deriving (Eq, Ord, Show)

data Rest = Rest { _restDur :: Duration, _restDyn :: Dynamic }
  deriving (Eq, Ord, Show)

data Spacer = Spacer { _spacerDur :: Duration, _spacerDyn :: Dynamic }
  deriving (Eq, Ord, Show)

data Tuplet = Tuplet { _tupNum :: Int, _tupDenom :: Int, _tupDur :: Duration, _tupNotes :: NonEmpty Note }
  deriving (Eq, Ord, Show)

data Chord = Chord { _chordPitOctPairs :: NonEmpty (Pitch, Octave) , _chordDur :: Duration, _chordAccs :: NonEmpty Accent, _chordDyn :: Dynamic, _chordSwell :: Swell, _chordTie :: Bool }
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
  | VeClef { _veClef :: Clef }
  | VeTempo { _veTempo :: Tempo }
  | VeTremolo { _veTremolo :: Tremolo }
  | VeKeySignature { _veKeySig :: KeySignature }
  | VeTimeSignature { _veTimeSig :: TimeSignature }
   deriving (Eq, Ord, Show)

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

data Voice =
  PitchedVoice      { _ptvInstrument :: Instrument, _ptvVoiceEvents :: NonEmpty VoiceEvent }
  | PercussionVoice { _pcvInstrument :: Instrument, _pcvVoiceEvents :: NonEmpty VoiceEvent }
  | PolyVoice       { _povInstrument :: Instrument, _povVoiceEvents :: NonEmpty (NonEmpty VoiceEvent) }
  | VoiceGroup      { _vgVoices :: NonEmpty Voice }
  deriving (Eq, Ord, Show)

data Score = Score { _scoreComment :: String, _scoreVoices :: NonEmpty Voice }
  deriving (Eq, Ord, Show)
