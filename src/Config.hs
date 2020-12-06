{-# LANGUAGE FlexibleInstances #-}

module Config (FromConfig(..)) where

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

instance FromConfig Clef where
  parseConfig = mkParseConfig pClefStr

instance FromConfig (Pitch,Octave) where
  parseConfig = mkParseConfig pPitOctPr

instance FromConfig Scale where
  parseConfig = mkParseConfig (Scale <$> mkPs parsePitch)

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

instance FromConfig (NE.NonEmpty (NE.NonEmpty Accent)) where
  parseConfig = mkParseConfig (mkPss pAccentStr)

instance FromConfig (NE.NonEmpty (NE.NonEmpty Dynamic)) where
  parseConfig = mkParseConfig (mkPss pDynamicStr)

instance FromConfig (NE.NonEmpty (NE.NonEmpty Duration)) where
  parseConfig = mkParseConfig (mkPss parseDuration)

instance FromConfig (NE.NonEmpty (NE.NonEmpty (Maybe Int))) where
  parseConfig = mkParseConfig (mkPss pMInt)

instance FromConfig (NE.NonEmpty (NE.NonEmpty Bool)) where
  parseConfig = mkParseConfig (mkPss pSlurStr)

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

pSlurStr :: Parser Bool
pSlurStr = choice (zipWith mkParser slurStrs [True,False])

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

pMInt :: Parser (Maybe Int)
pMInt = (Just <$> int) <|> (char 'r' >> pure Nothing)

modeStrs :: [String]
modeStrs = ["major", "minor"]

pModeStr :: Parser Mode
pModeStr = choice (zipWith mkParser modeStrs [Major .. Minor])

pKeySignature :: Parser KeySignature
pKeySignature = KeySignature <$> parsePitch <*> (spaces *> pModeStr)

octaveInts :: [String]
octaveInts = ["-4","-3","-2","-1","0","1","2","3"]

pOctaveStr :: Parser Octave
pOctaveStr = choice (zipWith mkParser octaveInts [TwentyNineVBOct .. TwentyTwoVAOct])

pPitOctPr :: Parser (Pitch,Octave)
pPitOctPr = between (char '(') (char ')') ((,) <$> parsePitch <*> (char ',' *> pOctaveStr))

clefStrs :: [String]
clefStrs = ["bass_8", "bass", "tenor", "alto", "treble", "treble^8"]

pClefStr :: Parser Clef
pClefStr = choice (zipWith mkParser clefStrs [Bass8VB ..Treble8VA])
