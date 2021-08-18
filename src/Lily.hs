{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lily (ToLily(..)
            ,FromLily(..)
            ,parsePitch
            ,parseInstrument
            ,parseDuration
            ,parseDynamic
            ,parseSwell
            ,mkParseLily
            ,splitTremolo -- temporary
            ) where

import Control.Monad
import Data.List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe
import Data.Natural
import Data.String.Interpolation
import Text.Parsec
import Text.Parsec.String

import Types
import Utils

class ToLily a where
  -- | Convert a Haskell value to a Lilypond string
  toLily :: a -> String

class FromLily a where
  -- | Convert a Lilypond string to a Haskell value
  parseLily :: String -> a

-----------
-- Pitch --
-----------

pitchSyms :: [String]
pitchSyms = ["ceses","ces","cisis","cis","c"
           ,"deses","des","disis","dis","d"
           ,"eeses","ees","eisis","eis","e"
           ,"feses","fes","fisis","fis","f"
           ,"geses","ges","gisis","gis","g"
           ,"aeses","aes","aisis","ais","a"
           ,"beses","bes","bisis","bis","b"]

pitchVals :: [Pitch]
pitchVals = [Cff, Cf, Css, Cs, C
            ,Dff, Df, Dss, Ds, D
            ,Eff, Ef, Ess, Es, E
            ,Fff, Ff, Fss, Fs, F
            ,Gff, Gf, Gss, Gs, G
            ,Aff, Af, Ass, As, A
            ,Bff, Bf, Bss, Bs, B]

instance ToLily Pitch where
  toLily = mkToLily "pitch" pitchVals pitchSyms

parsePitch :: Parser Pitch
parsePitch = choice (zipWith mkParser pitchSyms pitchVals)

instance FromLily Pitch  where
  parseLily = mkParseLily parsePitch

------------
-- Octave --
------------

octaveVals :: [Octave]
octaveVals = [TwentyNineVBOct,TwentyTwoVBOct,FifteenVBOct,TwentyTwoVAOct,FifteenVAOct,EightVAOct,COct,EightVBOct]

octaveSyms :: [String]
octaveSyms = [",,,"
             ,",,"
             ,","
             ,"''''"
             ,"'''"
             ,"''"
             ,"'"    -- COct
             ,""]    -- EightVBOct

instance ToLily Octave where
  toLily = mkToLily "octave" octaveVals octaveSyms

parseOctave :: Parser Octave
parseOctave = choice (zipWith mkParser (init octaveSyms) (init octaveVals)) <|> pure EightVBOct

instance FromLily Octave  where
  parseLily = mkParseLily parseOctave

--------------
-- Duration --
--------------

durationVals :: [Duration]
durationVals = [HTEDur,DSFDur,SFDur,DTSDur,TSDur,DSDur,SDur,DEDur,EDur,DQDur,QDur,DHDur,HDur,DWDur,WDur]

durationSyms :: [String]
durationSyms = ["128","64.","64","32.","32","16.","16","8.","8","4.","4","2.","2","1.","1"]

instance ToLily Duration where
  toLily = mkToLily "duration" durationVals durationSyms

parseDuration :: Parser Duration
parseDuration = choice $ zipWith mkParser durationSyms durationVals

instance FromLily Duration  where
  parseLily = mkParseLily parseDuration

------------
-- Accent --
------------

accentSyms :: [String]
accentSyms = ["-^", "--", "-!", "-.",  "->", "-_", ""]

accentVals :: [Accent]
accentVals = [Marcato .. NoAccent]

instance ToLily Accent where
  toLily = mkToLily "accent" accentVals accentSyms

parseAccent :: Parser Accent
parseAccent = choice (zipWith mkParser (init accentSyms) (init accentVals)) <|> pure NoAccent

instance FromLily Accent  where
  parseLily = mkParseLily parseAccent

-------------
-- Dynamic --
-------------

dynamicSyms :: [String]
dynamicSyms = ["\\ppppp", "\\pppp", "\\ppp", "\\pp", "\\p", "\\mp", "\\mf", "\\fffff", "\\ffff", "\\fff", "\\ff", "\\fp", "\\f", "\\sff", "\\sfz", "\\sf", "\\spp", "\\sp", "\\rfz", ""]

dynamicVals :: [Dynamic]
dynamicVals = [PPPPP, PPPP, PPP, PP, Piano, MP, MF, FFFFF, FFFF, FFF, FF, FP, Forte, SFF, SFZ, SF, SPP, SP, RFZ, NoDynamic]

instance ToLily Dynamic where
  toLily = mkToLily "dynamic" dynamicVals dynamicSyms

parseDynamic :: Parser Dynamic
parseDynamic = choice (zipWith mkParser (init dynamicSyms) (init dynamicVals)) <|> pure NoDynamic

instance FromLily Dynamic  where
  parseLily = mkParseLily parseDynamic

-------------
-- Swell --
-------------

swellSyms :: [String]
swellSyms = ["\\<", "\\>", "\\espressivo", "\\!", ""]

swellVals :: [Swell]
swellVals = [Crescendo, Decrescendo, Espressivo, SwellStop, NoSwell]

instance ToLily Swell where
  toLily = mkToLily "swell" swellVals swellSyms

parseSwell :: Parser Swell
parseSwell = choice (zipWith mkParser (init swellSyms) (init swellVals)) <|> pure NoSwell

instance FromLily Swell  where
  parseLily = mkParseLily parseSwell

----------
-- Note --
----------

instance ToLily Note where
  toLily (Note pit oct dur acc dyn swell slr) = toLily pit <> toLily oct <> toLily dur <> toLily acc <> toLily dyn <> toLily swell <> if slr then "~" else ""

parseNote :: Parser Note
parseNote = Note <$> parsePitch <*> parseOctave <*> parseDuration <*> parseAccent <*> parseDynamic <*> parseSwell <*> parseBool

instance FromLily Note  where
  parseLily = mkParseLily parseNote

----------
-- Rhythm --
----------

instance ToLily Rhythm where
  toLily (Rhythm instr dur acc dyn swell) = instr  <> toLily dur <> toLily acc <> toLily dyn <> toLily swell

parseRhythm :: Parser Rhythm
parseRhythm = Rhythm <$> manyTill anyChar eof <*> parseDuration <*> parseAccent <*> parseDynamic <*> parseSwell

instance FromLily Rhythm  where
  parseLily = mkParseLily parseRhythm

----------
-- Rest --
----------

instance ToLily Rest where
  toLily (Rest dur dyn) = "r" <> toLily dur <> toLily dyn

parseRest :: Parser Rest
parseRest = Rest <$> (char 'r' *> parseDuration) <*> parseDynamic

instance FromLily Rest  where
  parseLily = mkParseLily parseRest

------------
-- Tuplet --
------------

instance ToLily Tuplet where
  toLily (Tuplet num denom dur notes) = "\\tuplet " <> show num <> "/" <> show denom <> " " <> toLily dur <> " {" <> (unwords . NE.toList . NE.map toLily $ notes) <> "}"

parseNotes :: Parser (NE.NonEmpty Note)
parseNotes = NE.fromList <$> (parseNote `sepBy` spaces)

parseTuplet :: Parser Tuplet
parseTuplet = Tuplet <$> (string "\\tuplet " *> parseInt) <*> (string "/" *> parseInt) <*> (spaces *> parseDuration) <*> (string " {" *> parseNotes <* string "}")

instance FromLily Tuplet where
  parseLily = mkParseLily parseTuplet

-----------
-- Chord --
-----------

pitchOctavePairToLily :: (Pitch,Octave) -> String
pitchOctavePairToLily (p,o) = toLily p <> toLily o

pitchOctavePairsToLily :: NE.NonEmpty (Pitch,Octave) -> String
pitchOctavePairsToLily = unwords . NE.toList . NE.map pitchOctavePairToLily

instance ToLily Chord where
  toLily (Chord prs dur acc dyn swell slr) = "<" <> pitchOctavePairsToLily prs <> ">" <> toLily dur <> toLily acc <> toLily dyn <> toLily swell <> if slr then "~" else ""

parsePair :: Parser (Pitch,Octave)
parsePair = (,) <$> parsePitch <*> parseOctave

parsePairs :: Parser (NE.NonEmpty (Pitch,Octave))
parsePairs = NE.fromList <$> (parsePair `sepBy` spaces)

parseChord :: Parser Chord
parseChord = Chord <$> (string "<" *> parsePairs <* string ">") <*> parseDuration <*> parseAccent <*> parseDynamic <*> parseSwell <*> parseBool

instance FromLily Chord  where
  parseLily = mkParseLily parseChord

----------
-- Clef --
----------

clefSyms :: [String]
clefSyms = ["\\clef bass_8", "\\clef bass", "\\clef tenor", "\\clef alto", "\\clef treble", "\\clef treble^8"]

clefVals :: [Clef]
clefVals = [Bass8VB .. Treble8VA]

instance ToLily Clef where
  toLily = mkToLily "clef" clefVals clefSyms

parseClef :: Parser Clef
parseClef = choice (zipWith mkParser clefSyms clefVals)

instance FromLily Clef  where
  parseLily = mkParseLily parseClef

-----------
-- Tempo --
-----------

instance ToLily Tempo where
  toLily (TempoText txt) = [str|\tempo $txt$|]
  toLily (TempoDur dur perMin) =  [str|\tempo $toLily dur$ = $:perMin$|]
  toLily (TempoLong txt dur perMin) = [str|\tempo $txt$ ($toLily dur$ = $:perMin$)|]
  toLily (TempoRange dur loPerMin hiPerMin) = [str|\tempo $toLily dur$ = $:loPerMin$ - $:hiPerMin$|]

parseTempo :: Parser Tempo
parseTempo = choice [try (TempoRange <$> (string "\\tempo " *> parseDuration) <*> (string " = " *> parseNatural) <*> (string " - " *> parseNatural))
                    ,try (TempoLong <$> (string "\\tempo " *> manyTill anyChar (try (string " ("))) <*> parseDuration <*> (string " = " *> parseNatural <* char ')'))
                    ,try (TempoDur <$> (string "\\tempo " *> parseDuration) <*> (string " = " *> parseNatural))
                    ,try (TempoText <$> (string "\\tempo " *> manyTill anyChar eof))]

instance FromLily Tempo where
  parseLily = mkParseLily parseTempo

-------------
-- Tremolo --
-------------

instance ToLily Tremolo where
  toLily (NoteTremolo (Note pit oct dur acc dyn swell slr)) =
    [str|\repeat tremolo $:reps$ {$toLily pit <> toLily oct <> toLily barring <> toLily acc <> toLily dyn <> toLily swell <> if slr then "~" else ""$}|]
    where
      (reps,barring) = splitTremolo [dur] [SFDur, HTEDur]
  toLily (ChordTremolo (Chord prsL durL accL dynL swellL slrL) (Chord prsR durR accR dynR swellR slrR)) =
    [str|\repeat tremolo $:reps$ {<$prs2Lily prsL$>$toLily barring <> toLily accL <> toLily dynL <> toLily swellL <> if slrL then "~" else ""$ <$prs2Lily prsR$>$toLily barring <> toLily accR <> toLily dynR <> toLily swellR <> if slrR then "~" else ""$}|]
    where
      (reps,barring) = splitTremolo [durL,durR] [SFDur, HTEDur]
      pr2Lily (p,o) = toLily p <> toLily o
      prs2Lily = unwords . map pr2Lily . NE.toList

splitTremolo :: [Duration] -> [Duration] -> (Int,Duration)
splitTremolo durTot [] = error $ "splitTremolo unable to split " <> show durTot
splitTremolo durTot (dur:durs)
  | 0 == dsTot `mod` dsTry = (dsTot `div` dsTry `div` length durTot,dur)
  | otherwise = splitTremolo durTot durs
  where
    dsTot = getDurSum $ sumDurs durTot
    dsTry = getDurSum $ sumDurs [dur]

parseNoteTremolo :: Parser Tremolo
parseNoteTremolo = do
    reps <- string "\\repeat tremolo" *> spaces *> parseInt
    Note{..} <- spaces *> string "{" *> parseNote <* string "}"
    pure $ NoteTremolo (Note _notePit _noteOct (composedDur reps _noteDur) _noteAcc _noteDyn _noteSwell _noteSlur)

parseChordTremolo :: Parser Tremolo
parseChordTremolo = do
    reps <- string "\\repeat tremolo" *> spaces *> parseInt
    Chord c1prs c1dur c1acc c1dyn c1swell c1slur <- spaces *> string "{" *> parseChord
    Chord c2prs c2dur c2acc c2dyn c2swell c2slur <- spaces *> parseChord <* string "}"
    pure $ ChordTremolo (Chord c1prs (composedDur reps c1dur) c1acc c1dyn c1swell c1slur) (Chord c2prs (composedDur reps c2dur) c2acc c2dyn c2swell c2slur)

parseTremolo :: Parser Tremolo
parseTremolo = choice [try parseNoteTremolo, try parseChordTremolo]

instance FromLily Tremolo where
  parseLily = mkParseLily parseTremolo

----------
-- Mode --
----------

modeSyms :: [String]
modeSyms = ["\\major", "\\minor"]

modeVals :: [Mode]
modeVals = [Major .. Minor]

instance ToLily Mode where
  toLily = mkToLily "mode" modeVals modeSyms

parseMode :: Parser Mode
parseMode = choice (zipWith mkParser modeSyms modeVals)


instance FromLily Mode where
  parseLily = mkParseLily parseMode

------------------
-- KeySignature --
------------------

instance ToLily KeySignature where
  toLily (KeySignature pit mode) = "\\key " <> toLily pit <> " " <> toLily mode

parseKeySignature :: Parser KeySignature
parseKeySignature = KeySignature <$> (string "\\key " *> parsePitch) <*> (spaces *> parseMode)

instance FromLily KeySignature where
  parseLily = mkParseLily parseKeySignature

-------------------
-- TimeSignature --
-------------------

instance ToLily TimeSignature where
  toLily (TimeSignature num denom) = "\\time " <> show num <> "/" <> toLily denom
  toLily (TimeSignatureGrouping nums num denom) = "\\time #'(" <>  intercalate "," (map show (NE.toList nums)) <> ")"  <> " " <> show num <> "/" <> toLily denom

parseTimeSignature :: Parser TimeSignature
parseTimeSignature = choice [try (TimeSignatureGrouping <$> (string "\\time #'" *> parseIntList) <*> parseInt  <*> (string "/" *> parseDuration))
                            ,try (TimeSignature <$> (string "\\time " *> parseInt) <*> (string "/" *> parseDuration))]

parseIntList :: Parser (NE.NonEmpty Int)
parseIntList = NE.fromList <$> between (symbol '(') (symbol ')') (parseInt `sepBy` char ',')

instance FromLily TimeSignature where
  parseLily = mkParseLily parseTimeSignature

----------------
-- VoiceEvent --
----------------

instance ToLily VoiceEvent where
  toLily (VeNote note) = toLily note
  toLily (VeRhythm rhythm) = toLily rhythm
  toLily (VeRest rest) = toLily rest
  toLily (VeTuplet tup) = toLily tup
  toLily (VeChord chord) = toLily chord
  toLily (VeClef clef) = toLily clef
  toLily (VeTempo tempo) = toLily tempo
  toLily (VeTremolo tremolo) = toLily tremolo
  toLily (VeKeySignature keySignature) = toLily keySignature
  toLily (VeTimeSignature timeSignature) = toLily timeSignature

parseVoiceEvent :: Parser VoiceEvent
parseVoiceEvent = choice [try (VeClef <$> parseClef)
                         ,try (VeNote <$> parseNote)
                         ,try (VeRhythm <$> parseRhythm)
                         ,try (VeRest <$> parseRest)
                         ,try (VeChord <$> parseChord)
                         ,try (VeTempo <$> parseTempo)
                         ,try (VeKeySignature <$> parseKeySignature)
                         ,try (VeTimeSignature <$> parseTimeSignature)
                         ,try (VeTremolo <$> parseTremolo)]

instance FromLily VoiceEvent where
  parseLily = mkParseLily parseVoiceEvent

----------------
-- Instrument --
----------------

instrumentSyms :: [String]
instrumentSyms =
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

shortInstrNames :: [String]
shortInstrNames =
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

instrumentVals :: [Instrument]
instrumentVals = [AcousticGrand .. LeadVoice]

instance ToLily Instrument where
  toLily = mkToLily "instrument" instrumentVals instrumentSyms

parseInstrument :: Parser Instrument
parseInstrument = choice (zipWith mkParser instrumentSyms instrumentVals)

instance FromLily Instrument  where
  parseLily = mkParseLily parseInstrument

midiName :: Instrument -> String
midiName = (instrumentSyms !!) . fromEnum

shortInstrName :: Instrument -> String
shortInstrName = (shortInstrNames !!) . fromEnum

-- shortInstrName2LongName :: String -> String
-- shortInstrName2LongName = (instrumentSyms !!) . fromJust . flip elemIndex instrumentNames

-----------
-- Voice --
-----------

instance ToLily Voice where
  toLily (PitchedVoice instr events) = toPitchedVoice instr events
  toLily (PercussionVoice instr events) = toPercussionVoice instr events
  toLily (VoiceGroup voices) = toVoiceGroup voices
  toLily (PolyVoice instr eventss) = toPolyVoice instr eventss

toPitchedVoice :: Instrument -> NE.NonEmpty VoiceEvent -> String
toPitchedVoice instr events =
  [str|\new Voice
      {\set Staff.instrumentName = ##"$shortInstrName instr$"\set Staff.midiInstrument = ##"$midiName instr$"
      $unwords (map toLily (NE.toList events))$ \bar "|."
      }
      |]

toPercussionVoice :: Instrument -> NE.NonEmpty VoiceEvent -> String
toPercussionVoice instr events =
  [str|\new DrumStaff \with
      {drumStyleTable = ##percussion-style \override StaffSymbol ##'line-count = ##1 instrumentName = ##"$midiName instr$"}
      \drummode {
      $unwords (map toLily (NE.toList events))$ \bar "|."
      }
      |]

toVoiceGroup :: NE.NonEmpty Voice -> String
toVoiceGroup voices = [str|\new StaffGroup
                          <<
                          $mconcat (map toLily (NE.toList voices))$>>
                          |]

eventsToPolyVoice :: NE.NonEmpty VoiceEvent -> String
eventsToPolyVoice events  =
  [str|\new Staff {
      \new Voice {
      $unwords (map toLily (NE.toList events))$ \bar "|."
      }
      }
      |]

toPolyVoice :: Instrument -> NE.NonEmpty (NE.NonEmpty VoiceEvent) -> String
toPolyVoice instr eventss =
  [str|\new PianoStaff {
      <<
      \set PianoStaff.instrumentName = ##"$shortInstrName instr$"\set PianoStaff.midiInstrument = ##"$midiName instr$"
      $mconcat (map eventsToPolyVoice (NE.toList eventss))$>>
      }
      |]

parsePolyVoiceEvents :: Parser (NE.NonEmpty VoiceEvent)
parsePolyVoiceEvents = NE.fromList <$> (string [str|\new Staff {
                                               \new Voice {
                                              |]
                                       *> parseVoiceEvent `endBy` space
                                       <* string [str|\bar "|."
                                         }
                                         }|])

parseVoice :: Parser Voice
parseVoice = choice [
  try (PitchedVoice <$> (string [str|\new Voice
                                   {\set Staff.instrumentName = ##|]
                        *> parseQuotedIdentifier
                        *> string [str|\set Staff.midiInstrument = ##"|]
                        *> parseInstrument <* string [str|"$endline$|])
                   <*> (NE.fromList <$> (parseVoiceEvent `endBy` space
                        <* string [str|\bar "|."
                                      }|])))
  ,try (PercussionVoice <$> (string [str|\new Voice
                                   {\set Staff.instrumentName = ##|]
                        *> parseQuotedIdentifier
                        *> string [str|\set Staff.midiInstrument = ##"|]
                        *> parseInstrument <* string [str|"$endline$|])
                   <*> (NE.fromList <$> (parseVoiceEvent `endBy` space
                        <* string [str|\bar "|."
                                      }|])))
  ,try (VoiceGroup <$> (string [str|\new StaffGroup
                                   <<
                                   |]
                        *> (NE.fromList <$> (parseVoice `endBy` newline
                            <* string ">>"))))
  ,try (PolyVoice <$> (string [str|\new PianoStaff {
                                  <<
                                  \set PianoStaff.instrumentName = ##|]
                       *> parseQuotedIdentifier
                       *> string [str|\set PianoStaff.midiInstrument = ##"|]
                       *> parseInstrument <* string [str|"$endline$|])
                       <*> (NE.fromList <$> (parsePolyVoiceEvents `endBy` newline
                                             <* string [str|>>
                                                           }|])))
  ]

instance FromLily Voice where
  parseLily = mkParseLily parseVoice

-----------
-- Score --
-----------

instance ToLily Score where
  toLily (Score comment voices) =
    [str|% "$comment$"
        \include "articulate.ly"
        \version "2.18.2"
        structure = {
        <<
        $mconcat (map toLily (NE.toList voices))$>>
        }
        \score {\structure \layout { \context { \Voice \remove "Note_heads_engraver" \consists "Completion_heads_engraver" \remove "Rest_engraver" \consists "Completion_rest_engraver" } } }
        \score { \unfoldRepeats \articulate \structure \midi {  } }
        |]

parseScore :: Parser Score
parseScore = Score <$> (string "% " *> parseQuotedString
                         <* string [str|
                                       \include "articulate.ly"
                                       \version "2.18.2"
                                       structure = {
                                       <<
                                       |])
                       <*> (NE.fromList <$> parseVoice `endBy` newline)
                       <* string [str|>>
                                     }
                                     \score {\structure \layout { \context { \Voice \remove "Note_heads_engraver" \consists "Completion_heads_engraver" \remove "Rest_engraver" \consists "Completion_rest_engraver" } } }
                                     \score { \unfoldRepeats \articulate \structure \midi {  } }
                                     |]

instance FromLily Score where
  parseLily = mkParseLily parseScore

-----------
-- Utils --
-----------

mkParser :: String -> a -> Parser a
mkParser s d = try (string s >> pure d)

parseInt :: Parser Int
parseInt = read <$> many1 digit

parseNatural :: Parser Natural
parseNatural = read <$> many1 digit

parseBool :: Parser Bool
parseBool = (string "~" >> pure True) <|> pure False

mkToLily :: (Show a, Ord a) => String -> [a] -> [String] -> a -> String
mkToLily name vals syms v = fromMaybe (error $ "Invalid " <> name <> " val " <> show v <> " not in " <> show vals) $ M.lookup v (M.fromList (zip vals syms))

mkParseLily :: Parser a -> String -> a
mkParseLily parser  = either (error . show) id . parse parser ""

-- https://jakewheat.github.io/intro_to_parsing

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

symbol :: Char -> Parser ()
symbol = void . lexeme . char

identifier :: Parser String
identifier = lexeme ((:) <$> firstChar <*> many nonFirstChar)
  where
    firstChar = letter <|> char '_'
    nonFirstChar = digit <|> firstChar

parseQuotedString :: Parser String
parseQuotedString = lexeme (char '\"' *> manyTill anyChar (char '\"'))

parseQuotedIdentifier :: Parser String
parseQuotedIdentifier = between (symbol '"') (symbol '"') identifier
