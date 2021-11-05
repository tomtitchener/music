{-# LANGUAGE FlexibleInstances #-}

module Config (FromConfig(..)) where

import Data.Functor ((<&>))
import Text.Parsec
import Text.Parsec.Number
import Text.Parsec.String

import qualified Data.List.NonEmpty as NE
import Lily
import Types

class FromConfig a where
  -- | Convert a config string to a Haskell value
  parseConfig :: String -> a

instance FromConfig Int where
  parseConfig = mkParseConfig (string "int" *> spaces *> int)

instance FromConfig Instrument where
  parseConfig = mkParseConfig parseInstrument

instance FromConfig KeySignature where
  parseConfig = mkParseConfig pKeySignature

instance FromConfig TimeSignature where
  parseConfig = mkParseConfig pTimeSignature

instance FromConfig Clef where
  parseConfig = mkParseConfig pClefStr

instance FromConfig (Pitch,Octave) where
  parseConfig = mkParseConfig pPitOctPr

instance FromConfig ((Pitch,Octave),(Pitch,Octave)) where
  parseConfig = mkParseConfig pPitOctsPr

instance FromConfig Scale where
  parseConfig = mkParseConfig (Scale <$> mkPs parsePitch)

instance FromConfig (NE.NonEmpty (NE.NonEmpty Pitch)) where
  parseConfig = mkParseConfig (mkPss parsePitch)

instance FromConfig (NE.NonEmpty (NE.NonEmpty (Maybe Pitch))) where
  parseConfig = mkParseConfig (mkPss pMPitch)

instance FromConfig (NE.NonEmpty Accent) where
  parseConfig = mkParseConfig (mkPs pAccentStr)

instance FromConfig (NE.NonEmpty Dynamic) where
  parseConfig = mkParseConfig (mkPs pDynamicStr)

instance FromConfig (NE.NonEmpty Duration) where
  parseConfig = mkParseConfig (mkPs parseDuration)

instance FromConfig (NE.NonEmpty (Maybe Int)) where
  parseConfig = mkParseConfig (mkPs pMInt)

instance FromConfig (NE.NonEmpty (Pitch,Octave)) where
  parseConfig = mkParseConfig (mkPs pPitOctPr)

instance FromConfig (NE.NonEmpty ((Pitch,Octave),(Pitch,Octave))) where
  parseConfig = mkParseConfig (mkPs pPitOctsPr)

instance FromConfig (NE.NonEmpty (NE.NonEmpty Accent)) where
  parseConfig = mkParseConfig (mkPss pAccentStr)

instance FromConfig (NE.NonEmpty (NE.NonEmpty Dynamic)) where
  parseConfig = mkParseConfig (mkPss pDynamicStr)

instance FromConfig (NE.NonEmpty (NE.NonEmpty Duration)) where
  parseConfig = mkParseConfig (mkPss parseDuration)

instance FromConfig (NE.NonEmpty Int) where
  parseConfig = mkParseConfig (mkPs parseInt)

instance FromConfig (NE.NonEmpty (NE.NonEmpty Int)) where
  parseConfig = mkParseConfig (mkPss parseInt)

instance FromConfig (NE.NonEmpty (NE.NonEmpty (Maybe Int))) where
  parseConfig = mkParseConfig (mkPss pMInt)

instance FromConfig (NE.NonEmpty (NE.NonEmpty Bool)) where
  parseConfig = mkParseConfig (mkPss pTieStr)

instance FromConfig (NE.NonEmpty (Int,Int)) where
  parseConfig = mkParseConfig (mkPs pIntPr)

instance FromConfig (NE.NonEmpty (NE.NonEmpty (Int,Int))) where
  parseConfig = mkParseConfig (mkPss pIntPr)

mkParseConfig :: Parser a -> String -> a
mkParseConfig parser  = either (error . show) id . parse parser ""

lexeme :: Parser a -> Parser a
lexeme p = spaces *> p

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
accentStrs = ["^", "-", "!", ".",  ">", "_", "~"]

pAccentStr :: Parser Accent
pAccentStr = choice (zipWith mkParser accentStrs [Marcato .. NoAccent])

dynamicStrs :: [String]
dynamicStrs = ["ppppp", "pppp", "ppp", "pp", "p", "mp", "mf", "fffff", "ffff", "fff", "ff", "fp", "f", "sff", "sfz", "sf", "spp", "sp", "rfz", "~"]

dynamicVals :: [Dynamic]
dynamicVals = [PPPPP, PPPP, PPP, PP, Piano, MP, MF, FFFFF, FFFF, FFF, FF, FP, Forte, SFF, SFZ, SF, SPP, SP, RFZ, NoDynamic]

pDynamicStr :: Parser Dynamic
pDynamicStr = choice (zipWith mkParser dynamicStrs dynamicVals)

-- in practice, "Int" stands for Interval, which in musical terms,
-- maps to 1/-1 for unison, 2/-2 for a second or one scale step,
-- 3/-3 for a third or two scale steps and etc.  Zero is illegal.
-- in interval arithmetic 0 doesn't make any sense, 1/-1 is unison, etc.
-- convert to zero-based offset, 0 => exception, 1/-1 => 0, 2/-2 = 1/-1, etc.
int2Off :: Int -> Int
int2Off i
  | i < 0 = i + 1
  | i == 0 = error "int2Off invalid interval 0"
  | otherwise = i - 1

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
pIntsIntDurPr = between (char '(') (char ')') ((,) <$> mkPs parseInt <*> (char ',' *> pIntDurPr))

pTimeSig :: Parser TimeSignature
pTimeSig = pIntDurPr <&> uncurry TimeSignatureSimple

pTimeSignature :: Parser TimeSignature
pTimeSignature = try pTimeSignatureGrouping <|> pTimeSig

pIntDurPr :: Parser (Int,Duration)
pIntDurPr = between (char '(') (char ')') ((,) <$> parseInt <*> (char ',' *> parseDuration))

pIntPr :: Parser (Int,Int)
pIntPr = between (char '(') (char ')') ((,) <$> parseInt <*> (char ',' *> parseInt))

octaveInts :: [String]
octaveInts = ["-4","-3","-2","-1","0","1","2","3"]

pOctaveStr :: Parser Octave
pOctaveStr = choice (zipWith mkParser octaveInts [TwentyNineVBOct .. TwentyTwoVAOct])

pPitOctPr :: Parser (Pitch,Octave)
pPitOctPr = between (char '(') (char ')') ((,) <$> parsePitch <*> (char ',' *> pOctaveStr))

pPitOctsPr :: Parser ((Pitch,Octave),(Pitch,Octave))
pPitOctsPr = between (char '(') (char ')') ((,) <$> pPitOctPr <*> (char ',' *> pPitOctPr))

clefStrs :: [String]
clefStrs = ["bass_8", "bass", "tenor", "alto", "treble", "treble^8"]

pClefStr :: Parser Clef
pClefStr = choice (zipWith mkParser clefStrs [Bass8VB ..Treble8VA])
