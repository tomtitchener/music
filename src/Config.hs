{-# LANGUAGE FlexibleInstances #-}

module Config (FromConfig(..)) where

-- import Data.Function (on)
import Data.Functor ((<&>))
import Data.Natural (Natural)
import Text.Parsec
    ( letter,
      many,
      parse,
      digit,
      between,
      try,
      string,
      spaces,
      char,
      (<|>),
      choice,
      sepBy1 )
import Text.Parsec.Number ( int )
import Text.Parsec.String ( Parser )

import qualified Data.List.NonEmpty as NE

import Data.List (sort)

import Data.Tuple.Extra (second) 

import Lily
    ( parseDuration, parseInstrument, parsePitch, parseNat, parseNatural )

import Types

import Utils 

class FromConfig a where
  -- | Convert a config string to a Haskell value
  parseConfig :: String -> a

instance FromConfig Int where
  parseConfig = mkParseConfig (string "int" *> spaces *> int)

instance FromConfig String where
  parseConfig = mkParseConfig identifier
  
instance FromConfig (NE.NonEmpty String) where
  parseConfig = mkParseConfig (mkPs identifier)

instance FromConfig Instrument where
  parseConfig = mkParseConfig parseInstrument

instance FromConfig KeySignature where
  parseConfig = mkParseConfig pKeySignature

instance FromConfig TimeSignature where
  parseConfig = mkParseConfig pTimeSignature

instance FromConfig Clef where
  parseConfig = mkParseConfig pClefStr

instance FromConfig Octave where
  parseConfig = mkParseConfig pOctaveStr
  
instance FromConfig (NE.NonEmpty Octave) where
  parseConfig = mkParseConfig (mkPs pOctaveStr)
  
instance FromConfig (NE.NonEmpty (NE.NonEmpty Octave)) where
  parseConfig = mkParseConfig (mkPss pOctaveStr)

instance FromConfig PitOct where
  parseConfig = mkParseConfig pPitOct
  
instance FromConfig (NE.NonEmpty (PitOct,PitOct)) where
  parseConfig = mkParseConfig (mkPs pPitOctPr)

instance FromConfig Range where
  parseConfig = mkParseConfig pPitOctPr 

instance FromConfig (NE.NonEmpty (Maybe PitOct,Duration)) where
  parseConfig = mkParseConfig (mkPs (pPr (pM pPitOct) parseDuration))

-- List of Either PitOct (NE.NonEmty PitOct) for note or chord motto, no rests.
instance FromConfig (NE.NonEmpty PitOctOrNEPitOcts) where
  parseConfig = mkParseConfig (mkPs pPitOctOrPitOcts)
  
-- List of Either PitOct (NE.NonEmty PitOct) for list of note or chord motto, no rests.
instance FromConfig (NE.NonEmpty (NE.NonEmpty PitOctOrNEPitOcts)) where
  parseConfig = mkParseConfig (mkPs (mkPs pPitOctOrPitOcts))

instance FromConfig (NE.NonEmpty (NE.NonEmpty (Maybe PitOctOrNEPitOcts))) where
  parseConfig = mkParseConfig (mkPs (mkPs (pM pPitOctOrPitOcts)))

instance FromConfig (NE.NonEmpty PitOctOrNEPitOcts,NE.NonEmpty  PitOctOrNEPitOcts) where
  parseConfig = mkParseConfig (pPr (mkPs pPitOctOrPitOcts) (mkPs pPitOctOrPitOcts))
  
instance FromConfig (NE.NonEmpty PitOct) where
  parseConfig = mkParseConfig (mkPs pPitOct)
  
instance FromConfig (NE.NonEmpty (NE.NonEmpty PitOct)) where
  parseConfig = mkParseConfig (mkPs (mkPs pPitOct))

instance FromConfig Scale where
  parseConfig = mkParseConfig (Scale <$> mkPs parsePitch)

instance FromConfig (NE.NonEmpty (NE.NonEmpty Pitch)) where
  parseConfig = mkParseConfig (mkPss parsePitch)

instance FromConfig (NE.NonEmpty (NE.NonEmpty (Maybe Pitch))) where
  parseConfig = mkParseConfig (mkPss pMPitch)

instance FromConfig (NE.NonEmpty (Maybe Pitch,Int)) where
  parseConfig = mkParseConfig (mkPs pMPitOctPr)
  
instance FromConfig (NE.NonEmpty (NE.NonEmpty (Maybe Pitch,Int))) where
  parseConfig = mkParseConfig (mkPss pMPitOctPr)

instance FromConfig Accent where
  parseConfig = mkParseConfig pAccentStr
  
instance FromConfig (NE.NonEmpty Accent) where
  parseConfig = mkParseConfig (mkPs pAccentStr)
  
instance FromConfig Dynamic where
  parseConfig = mkParseConfig pDynamicStr
  
instance FromConfig (NE.NonEmpty Dynamic) where
  parseConfig = mkParseConfig (mkPs pDynamicStr)

instance FromConfig Sustain where
  parseConfig = mkParseConfig pSustainStr
  
instance FromConfig (NE.NonEmpty Sustain) where
  parseConfig = mkParseConfig (mkPs pSustainStr)

instance FromConfig (NE.NonEmpty Duration) where
  parseConfig = mkParseConfig (mkPs parseDuration)

instance FromConfig Control where
  parseConfig = mkParseConfig pControl 

instance FromConfig (NE.NonEmpty (Maybe Int)) where
  parseConfig = mkParseConfig (mkPs pMInt)

instance FromConfig (NE.NonEmpty (NE.NonEmpty (Maybe (NE.NonEmpty (Pitch,Int))))) where
  parseConfig = mkParseConfig (mkPs (mkPs (pM (mkPs pPitIntPr))))
  
instance FromConfig (NE.NonEmpty (NE.NonEmpty (Maybe (Either (Pitch,Int) (NE.NonEmpty (Pitch,Int)))))) where
  parseConfig = mkParseConfig (mkPs (mkPs (pM pPitIntPrOrPitIntPrs)))

instance FromConfig (NE.NonEmpty (NE.NonEmpty Accent)) where
  parseConfig = mkParseConfig (mkPss pAccentStr)

instance FromConfig (NE.NonEmpty (NE.NonEmpty Dynamic)) where
  parseConfig = mkParseConfig (mkPss pDynamicStr)

instance FromConfig (NE.NonEmpty (NE.NonEmpty Duration)) where
  parseConfig = mkParseConfig (mkPss parseDuration)

instance FromConfig (NE.NonEmpty (NE.NonEmpty DurValOrDurTuplet)) where
  parseConfig = mkParseConfig (mkPss parseDurOrDurTup)  

instance FromConfig (NE.NonEmpty (NE.NonEmpty NoteDurOrNoteDurNETup)) where
  parseConfig = mkParseConfig (mkPss parseNoteDurOrNoteDurTup)
  
instance FromConfig (NE.NonEmpty NoteDurOrNoteDurNETup) where
  parseConfig = mkParseConfig (mkPs parseNoteDurOrNoteDurTup)

instance FromConfig (NE.NonEmpty NoteDurOrNoteDurNETup,NE.NonEmpty NoteDurOrNoteDurNETup) where
  parseConfig = mkParseConfig (pPr (mkPs parseNoteDurOrNoteDurTup) (mkPs parseNoteDurOrNoteDurTup))

instance FromConfig (NE.NonEmpty RestDurValOrNoteDurVal,NE.NonEmpty RestDurValOrNoteDurVal) where
  parseConfig = mkParseConfig (pPr (mkPs pRestDurValOrNoteDurVal) (mkPs pRestDurValOrNoteDurVal))
  
instance FromConfig (NE.NonEmpty (NE.NonEmpty RestDurValOrNoteDurVal,NE.NonEmpty RestDurValOrNoteDurVal)) where
  parseConfig = mkParseConfig (mkPs (pPr (mkPs pRestDurValOrNoteDurVal) (mkPs pRestDurValOrNoteDurVal)))

-- Left is for Rest signaled via leading 'r', Left is for Note e.g. (8,8,r8,16,16)
pRestDurValOrNoteDurVal :: Parser RestDurValOrNoteDurVal
pRestDurValOrNoteDurVal = try (Left <$> (char 'r' *> parseDurationVal)) <|> (Right <$> parseDurationVal)

-- To allow DurationVal that exceeds maximum value in Lily:durationSyms of a dotted whole note,
-- allow either simple duration of one of durationSyms OR list of durationSym which get summed
-- into DurationVal.
parseDurationVal :: Parser DurationVal
parseDurationVal = try  (duration2DurationVal <$> parseDuration) <|>
                        (mkDurationVal . getDurSum . sumDurs . NE.toList <$> mkPs parseDuration)

parseDurOrDurTup :: Parser DurValOrDurTuplet
parseDurOrDurTup = try (Left <$> parseDurationVal) <|> (Right <$> parseDurTup)

parseDurTup :: Parser DurTuplet
parseDurTup = (DurTuplet <$> (char '(' *> int)
                         <*> (char ',' *> int)
                         <*> (char ',' *> parseDuration)
                         <*> (char ',' *> mkPs parseDurationVal <* char ')'))
              <&> verifyDurTuplet

instance FromConfig (NE.NonEmpty Int) where
  parseConfig = mkParseConfig (mkPs int)

instance FromConfig (NE.NonEmpty (NE.NonEmpty Int)) where
  parseConfig = mkParseConfig (mkPss int)

instance FromConfig (NE.NonEmpty (NE.NonEmpty (Maybe Int))) where
  parseConfig = mkParseConfig (mkPss pMInt)

instance FromConfig (NE.NonEmpty (NE.NonEmpty Bool)) where
  parseConfig = mkParseConfig (mkPss pTieStr)

instance FromConfig (NE.NonEmpty (Int,Int)) where
  parseConfig = mkParseConfig (mkPs pIntPr)

instance FromConfig (NE.NonEmpty (NE.NonEmpty (Int,Int))) where
  parseConfig = mkParseConfig (mkPss pIntPr)

instance FromConfig (KeySignature,PitOct) where
  parseConfig = mkParseConfig pKeySigPitOctPr
  
instance FromConfig (NE.NonEmpty (KeySignature,PitOct)) where
  parseConfig = mkParseConfig (mkPs pKeySigPitOctPr)

instance FromConfig (NE.NonEmpty (NE.NonEmpty (Maybe (Either Int (NE.NonEmpty Int))))) where
  parseConfig = mkParseConfig (mkPss pMIntOrInts)

pKeySigPitOctPr :: Parser (KeySignature,PitOct)
pKeySigPitOctPr = pPr pKeySignature pPitOct

-- 0 is not a legal interval, unison is 1/-1, second is 2/-2 and etc. (enforced by int2Off)
pMIntOrInts :: Parser (Maybe (Either Int (NE.NonEmpty Int)))
pMIntOrInts = pM (try (Left <$> (int2Off <$> int)) <|> (Right <$> mkPs (int2Off <$> int)))

instance FromConfig (NE.NonEmpty (NE.NonEmpty DurValAccOrDurTupletAccs)) where
  parseConfig = mkParseConfig (mkPss parseDurValAccOrDurTupAccs)

parseDurValAccOrDurTupAccs :: Parser DurValAccOrDurTupletAccs
parseDurValAccOrDurTupAccs = try (Left <$> parseDurValAcc) <|> (Right . second NE.toList <$> parseDurTupletAccs)

parseDurValAcc :: Parser (DurationVal,Accent)
parseDurValAcc = pPr parseDurationVal pAccentStr

parseDurTupletAccs :: Parser (DurTuplet,NE.NonEmpty Accent)
parseDurTupletAccs = pPr parseDurTup (mkPs pAccentStr)

parseNoteDurOrNoteDurTup :: Parser NoteDurOrNoteDurNETup
parseNoteDurOrNoteDurTup = try (Left <$> parseNoteDur) <|> (Right <$> parseNoteDurTup)

parseNoteDur :: Parser (Maybe PitOctOrNEPitOcts,DurationVal,Accent)
parseNoteDur = pTup (pM pPitOctOrPitOcts) parseDurationVal pAccentStr

parseNoteDurTup :: Parser (NE.NonEmpty (Maybe PitOctOrNEPitOcts),DurTuplet,NE.NonEmpty Accent)
parseNoteDurTup = pTup (mkPs (pM pPitOctOrPitOcts)) parseDurTup (mkPs pAccentStr)

-- Tempo
instance FromConfig Tempo where
  parseConfig = mkParseConfig parseTempo

mkParseConfig :: Parser a -> String -> a
mkParseConfig parser input  = either (error . show) id . parse parser input $ input

lexeme :: Parser a -> Parser a
lexeme p = spaces *> p

-- TBD: bug, fails if there's only one element, e.g. with no ',' separator.
mkPs :: Parser a -> Parser (NE.NonEmpty a)
mkPs p = NE.fromList <$> between (char '(') (char ')') (lexeme p `sepBy1` char ',')

mkPss :: Parser a -> Parser (NE.NonEmpty (NE.NonEmpty a))
mkPss = mkPs . mkPs

mkParser :: String -> a -> Parser a
mkParser s d = try (string s >> pure d)

slurStrs :: [String]
slurStrs = ["s", "~"]

pTieStr :: Parser Bool
pTieStr = choice (zipWith mkParser slurStrs [True,False])

accentStrs :: [String]
accentStrs = ["^", "-", "!", ".", ">", "_", "~"]

pAccentStr :: Parser Accent
pAccentStr = choice (zipWith mkParser accentStrs [Marcato .. NoAccent])

dynamicStrs :: [String]
dynamicStrs = ["ppppp", "pppp", "ppp", "pp", "p", "mp", "mf", "fffff", "ffff", "fff", "ff", "fp", "f", "sff", "sfz", "sf", "spp", "sp", "rfz", "~"]

dynamicVals :: [Dynamic]
dynamicVals = [PPPPP, PPPP, PPP, PP, Piano, MP, MF, FFFFF, FFFF, FFF, FF, FP, Forte, SFF, SFZ, SF, SPP, SP, RFZ, NoDynamic]

pDynamicStr :: Parser Dynamic
pDynamicStr = choice (zipWith mkParser dynamicStrs dynamicVals)

sustainStrs :: [String]
sustainStrs = ["sustOn", "sustOff"]

sustainVals :: [Sustain]
sustainVals = [SustainOn, SustainOff]

pSustainStr :: Parser Sustain
pSustainStr = choice (zipWith mkParser sustainStrs sustainVals)

-- in practice, "Int" stands for Interval, which in musical terms,
-- maps to 1/-1 for unison, 2/-2 for a second or one scale step,
-- 3/-3 for a third or two scale steps and etc.  Zero is illegal.
-- in interval arithmetic 0 doesn't make any sense, 1/-1 is unison, etc.
-- convert to zero-based offset, 0 => exception, 1/-1 => 0, 2/-2 = 1/-1, etc.
int2Off :: Int -> Int
int2Off i = case i `compare` 0 of
  LT -> i + 1
  EQ -> error "int2Off invalid interval 0"
  GT -> i - 1

pM :: Parser a -> Parser (Maybe a)
pM p = Just <$> p <|> (char 'r' >> pure Nothing)

pMInt :: Parser (Maybe Int)
pMInt = pM (int2Off <$> int)

pMPitch :: Parser (Maybe Pitch)
pMPitch = pM parsePitch

modeStrs :: [String]
modeStrs = ["major", "minor"]

pModeStr :: Parser Mode
pModeStr = choice (zipWith mkParser modeStrs [Major .. Minor])

pKeySignature :: Parser KeySignature
pKeySignature = KeySignature <$> parsePitch <*> (spaces *> pModeStr)

pTimeSignatureGrouping :: Parser TimeSignature
pTimeSignatureGrouping = pIntsIntDurPr <&> \(groups,(num,denom)) -> TimeSignatureGrouping groups num denom

pIntsIntDurPr :: Parser (NE.NonEmpty Int,(Int,Duration))
pIntsIntDurPr = pPr (mkPs int) pIntDurPr

pTimeSig :: Parser TimeSignature
pTimeSig = pIntDurPr <&> uncurry TimeSignatureSimple

parseTempo :: Parser Tempo
parseTempo = pNatDurPr <&> uncurry (flip TempoDur)

pNatDurPr :: Parser (Natural,Duration)
pNatDurPr = pPr parseNatural parseDuration

pTimeSignature :: Parser TimeSignature
pTimeSignature = try pTimeSignatureGrouping <|> pTimeSig

pControl :: Parser Control
pControl = try (CtrlSustain <$> pSustainStr) <|> (CtrlDynamic <$> pDynamicStr) <|> (CtrlSustain <$> pSustainStr)

pIntDurPr :: Parser (Int,Duration)
pIntDurPr = pPr parseNat parseDuration

pIntPr :: Parser (Int,Int)
pIntPr = pPr parseNat parseNat

octaveIntStrings :: [String]
octaveIntStrings = ["-4","-3","-2","-1","0","1","2","3"]

pOctaveStr :: Parser Octave
pOctaveStr = choice (zipWith mkParser octaveIntStrings [TwentyNineVBOct .. TwentyTwoVAOct])

pPitOctPr :: Parser (PitOct,PitOct)
pPitOctPr = pPr pPitOct pPitOct

pPitOct :: Parser PitOct
pPitOct = between (char '(') (char ')') (PitOct <$> parsePitch <*> (char ',' *> pOctaveStr))

pMPitOctPr :: Parser (Maybe Pitch,Int)
pMPitOctPr = pPr pMPitch int

pPitIntPr :: Parser (Pitch,Int)
pPitIntPr = pPr parsePitch int

pPitIntPrOrPitIntPrs :: Parser (Either (Pitch,Int) (NE.NonEmpty (Pitch,Int)))
pPitIntPrOrPitIntPrs = try (Left <$> pPitIntPr) <|> (Right <$> mkPs pPitIntPr)

pPr :: Parser a -> Parser b -> Parser (a,b)
pPr pFst pSnd = between (char '(') (char ')') ((,) <$> pFst <*> (char ',' *> pSnd))

pTup :: Parser a -> Parser b -> Parser c -> Parser (a,b,c)
pTup pFst pSnd pThrd = between (char '(') (char ')') ((,,) <$> pFst <*> (char ',' *> pSnd) <*> (char ',' *> pThrd))


-- sort by NE.List (Pitch,Octave) pairs by (Octave,Pitch) from low to hi for
-- easier transpose of lists of PitOctOrPitOcts
pPitOctOrPitOcts :: Parser PitOctOrNEPitOcts
pPitOctOrPitOcts = try (Left <$> pPitOct) <|> (Right . sortLoToHi <$> mkPs pPitOct)
  where
    sortLoToHi = NE.fromList . sort . NE.toList
  
clefStrs :: [String]
clefStrs = ["bass_8", "bass", "tenor", "alto", "treble", "treble^8"]

pClefStr :: Parser Clef
pClefStr = choice (zipWith mkParser clefStrs [Bass8VB ..Treble8VA])

-- from https://jakewheat.github.io/intro_to_parsing
identifier :: Parser String
identifier = lexeme ((:) <$> firstChar <*> many nonFirstChar)
  where
    firstChar = letter <|> char '_'
    nonFirstChar = digit <|> firstChar
