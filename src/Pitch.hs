
module Pitch (Pitch (..)
              ,toLily
              ,parseLily
              ,parsePitch
              ) where

import Text.Parsec
import Text.Parsec.String
import Utils

import Lily

-- Ord order
data Pitch = Bs | C   | Bss | Dff | Cs
           | Df | Css | D   | Eff | Ds
           | Ef | Fff | Dss | E   | Ff
           | Es | F   | Gff | Ess | Fs
           | Gf | Fss | G   | Aff | Gs
           | Af | Gss | A   | Bff | As
           | Bf | Cff | Ass | B   | Cf
  deriving (Eq, Ord, Show, Enum, Bounded)

-- parse order
lilySyms :: [String]
lilySyms = ["ceses","ces","cisis","cis","c"
           ,"deses","des","disis","dis","d"
           ,"eeses","ees","eisis","eis","e"
           ,"feses","fes","fisis","fis","f"
           ,"geses","ges","gisis","gis","g"
           ,"aeses","aes","aisis","ais","a"
           ,"beses","bes","bisis","bis","b"]

-- parse order
lilyVals :: [Pitch]
lilyVals = [Cff, Cf, Css, Cs, C
           ,Dff, Df, Dss, Ds, D
           ,Eff, Ef, Ess, Es, E
           ,Fff, Ff, Fss, Fs, F
           ,Gff, Gf, Gss, Gs, G
           ,Aff, Af, Ass, As, A
           ,Bff, Bf, Bss, Bs, B]

instance ToLily Pitch where
  toLily = mkToLily "pitch" lilyVals lilySyms

parsePitch :: Parser Pitch
parsePitch = choice (zipWith mkParser lilySyms lilyVals)

instance FromLily Pitch  where
  parseLily = mkParseLily parsePitch
