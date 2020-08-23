{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types (Pitch (..)
             ,Octave (..)
             ,Duration (..)
             ,DurationSum (..)
             ,Accent (..)
             ,Dynamic (..)
             ,Swell (..)
             ,Note (..)
             ,Rhythm (..)
             ,Rest (..)
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

import Data.Natural

data Pitch = Bs | C   | Bss | Dff | Cs
           | Df | Css | D   | Eff | Ds
           | Ef | Fff | Dss | E   | Ff
           | Es | F   | Gff | Ess | Fs
           | Gf | Fss | G   | Aff | Gs
           | Af | Gss | A   | Bff | As
           | Bf | Cff | Ass | B   | Cf
  deriving (Eq, Ord, Show, Enum, Bounded)

data Octave = TwentyNineVBOct | TwentyTwoVBOct | FifteenVBOct | EightVBOct | COct | EightVAOct | FifteenVAOct | TwentyTwoVAOct
  deriving (Eq, Ord, Show, Enum, Bounded)

data Duration =  HTEDur | SFDur | DSFDur | TSDur | DTSDur | SDur | DSDur | EDur | DEDur | QDur | DQDur | HDur | DHDur | WDur | DWDur
  deriving (Eq, Ord, Show, Enum, Bounded)

newtype DurationSum = DurationSum { getDurSum :: Int }
  deriving (Eq, Ord, Show, Num)

data Accent = Marcato | Tenuto | Staccatissimo | Staccato | Accent | Portato | NoAccent
  deriving (Eq, Ord, Show, Enum, Bounded)

data Dynamic = PPPPP | PPPP | PPP | PP | Piano | MP | MF | Forte | FF | FFF | FFFF | FFFFF | FP | SF | SFF | SP | SPP | SFZ | RFZ | NoDynamic
  deriving (Eq, Ord, Show, Enum, Bounded)

data Swell = Crescendo | Decrescendo | Espressivo | SwellStop | NoSwell
  deriving (Eq, Ord, Show, Enum, Bounded)

data Note = Note { _notePit :: Pitch, _noteOct :: Octave, _noteDur :: Duration, _noteAcc :: Accent, _noteDyn :: Dynamic, _noteSwell :: Swell, _noteSlur :: Bool }
  deriving (Eq, Ord, Show)

data Rhythm = Rhythm { _rhythmInstr :: String, _rhythmDur :: Duration, _rhythmAcc :: Accent, _rhythmDyn :: Dynamic, _rhythmSwell :: Swell }
  deriving (Eq, Ord, Show)

data Rest = Rest { _rdur :: Duration, _rdyn :: Dynamic }
  deriving (Eq, Ord, Show)

data Chord = Chord { _chordPitOctPairs :: [(Pitch, Octave)] , _chordDur :: Duration, _chordAcc :: Accent, _chordDyn :: Dynamic, _chordSwell :: Swell, _chordSlur :: Bool }
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

data TimeSignature = TimeSignature { _tsNum :: Int, _tsDenom :: Duration }
                   | TimeSignatureGrouping { _tsgGroups :: [Int], tsgNum :: Int, tsgDenom :: Duration }
  deriving (Eq, Ord, Show)

data VoiceEvent =
    VeNote { _veNote :: Note }
  | VeRest { _veRest :: Rest }
  | VeRhythm { _veRhythm :: Rhythm }
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
  PitchedVoice { _svInstrument :: Instrument, _svVoiceEvents :: [VoiceEvent] }
  | PercussionVoice { _pInstrument :: Instrument, _svVoiceEvents :: [VoiceEvent] }
  | VoiceGroup { _vgVoices :: [Voice] }
  | PolyVoice { _pvInstrument :: Instrument, _pvVoiceEvents :: [[VoiceEvent]] }
  deriving (Eq, Ord, Show)

data Score = Score { _scoreComment :: String, _scoreVoices :: [Voice] }
  deriving (Eq, Ord, Show)

