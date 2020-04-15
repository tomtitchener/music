
module Instrument (Instrument (..)
                  ,parseInstrument
                  ,midiName
                  ,shortName
                  ,shortName2LongName
                  ) where

import Data.List
import Data.Maybe
import Text.Parsec
import Text.Parsec.String

import Lily
import Utils

-- Ord order
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

-- parse order
lilySyms :: [String]
lilySyms =
     ["acoustic grand",           "contrabass",         "lead 7 (fifths)",
      "bright acoustic",          "tremolo strings",    "lead 8 (bass+lead)",
      "electric grand",           "pizzicato strings",  "pad 1 (new age)",
      "honky-tonk",               "orchestral harp",    "pad 2 (warm)",
      "electric piano 1",         "timpani",            "pad 3 (polysynth)",
      "electric piano 2",         "string ensemble 1",  "pad 4 (choir)",
      "harpsichord",              "string ensemble 2",  "pad 5 (bowed)",
      "clav",                     "synthstrings 1",     "pad 6 (metallic)",
      "celesta",                  "synthstrings 2",     "pad 7 (halo)",
      "glockenspiel",             "choir aahs",         "pad 8 (sweep)",
      "music box",                "voice oohs",         "fx 1 (rain)",
      "vibraphone",               "synth voice",        "fx 2 (soundtrack)",
      "marimba",                  "orchestra hit",      "fx 3 (crystal)",
      "xylophone",                "trumpet",            "fx 4 (atmosphere)",
      "tubular bells",            "trombone",           "fx 5 (brightness)",
      "dulcimer",                 "tuba",               "fx 6 (goblins)",
      "drawbar organ",            "muted trumpet",      "fx 7 (echoes)",
      "percussive organ",         "french horn",        "fx 8 (sci-fi)",
      "rock organ",               "brass section",      "sitar",
      "church organ",             "synthbrass 1",       "banjo",
      "reed organ",               "synthbrass 2",       "shamisen",
      "accordion",                "soprano sax",        "koto",
      "harmonica",                "alto sax",           "kalimba",
      "concertina",               "tenor sax",          "bagpipe",
      "acoustic guitar (nylon)",  "baritone sax",       "fiddle",
      "acoustic guitar (steel)",  "oboe",               "shanai",
      "electric guitar (jazz)",   "english horn",       "tinkle bell",
      "electric guitar (clean)",  "bassoon",            "agogo",
      "electric guitar (muted)",  "clarinet",           "steel drums",
      "overdriven guitar",        "piccolo",            "woodblock",
      "distorted guitar",         "flute",              "taiko drum",
      "guitar harmonics",         "recorder",           "melodic tom",
      "acoustic bass",            "pan flute",          "synth drum",
      "electric bass (finger)",   "blown bottle",       "reverse cymbal",
      "electric bass (pick)",     "shakuhachi",         "guitar fret noise",
      "fretless bass",            "whistle",            "breath noise",
      "slap bass 1",              "ocarina",            "seashore",
      "slap bass 2",              "lead 1 (square)",    "bird tweet",
      "synth bass 1",             "lead 2 (sawtooth)",  "telephone ring",
      "synth bass 2",             "lead 3 (calliope)",  "helicopter",
      "violin",                   "lead 4 (chiff)",     "applause",
      "viola",                    "lead 5 (charang)",   "gunshot",
      "cello",                    "lead 6 (voice)"]

shortNames :: [String]
shortNames =
          ["piano",                    "contra",             "ffths",
           "piano",                    "strs",               "basld",
           "piano",                    "pizzs",              "pad1",
           "piano",                    "harp",               "pad2",
           "piano",                    "timp",               "pad3",
           "piano",                    "strs",               "pad4",
           "hpscd",                    "strs",               "pad5",
           "clav",                     "synstr",             "pad6",
           "clsta",                    "synstr",             "pad7",
           "glock",                    "aahs",               "pad8",
           "mbox",                     "oohs",               "fx1",
           "vibes",                    "synv",               "fx2",
           "marimba",                  "orcht",              "fx3",
           "xyl",                      "tpt",                "fx4",
           "tube",                     "tbn",                "fx5",
           "dulc",                     "tuba",               "fx6",
           "organ",                    "mutpt",              "fx7",
           "organ",                    "horn",               "fx8",
           "organ",                    "brass",              "sitr",
           "organ",                    "synbr",              "banj",
           "organ",                    "synbr",              "sham",
           "accrd",                    "sopsx",              "koto",
           "harmo",                    "altsx",              "klmb",
           "ctina",                    "tensx",              "bagp",
           "guitr",                    "barsx",              "fddl",
           "guitr",                    "oboe",               "shni",
           "guitr",                    "enhrn",              "tnkl",
           "guitr",                    "bsn",                "aggo",
           "guitr",                    "clnt",               "stldr",
           "guitr",                    "picc",               "wdblk",
           "guitr",                    "fl",                 "tiko",
           "guitr",                    "rec",                "mtom",
           "bass",                     "pan",                "syndr",
           "bass",                     "bot",                "rvcbl",
           "bass",                     "shaki",              "fret",
           "bass",                     "whstl",              "brth",
           "bass",                     "ocrna",              "sea",
           "bass",                     "sqre",               "btwt",
           "bass",                     "sawth",              "ring",
           "bass",                     "call",               "cptr",
           "vln",                      "chiff",              "appl",
           "va",                       "chang",              "gun",
           "cello",                    "voice"]

-- parse order
lilyVals :: [Instrument]
lilyVals = [AcousticGrand .. LeadVoice]

instance ToLily Instrument where
  toLily = mkToLily "instrument" lilyVals lilySyms

parseInstrument :: Parser Instrument
parseInstrument = choice (zipWith mkParser lilySyms lilyVals)

instance FromLily Instrument  where
  parseLily = mkParseLily parseInstrument

midiName :: Instrument -> String
midiName = (lilySyms !!) . fromEnum

shortName :: Instrument -> String
shortName = (shortNames !!) . fromEnum

shortName2LongName :: String -> String
shortName2LongName = (lilySyms !!) . fromJust . flip elemIndex shortNames


