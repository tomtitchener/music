{-# LANGUAGE QuasiQuotes #-}

module Score (Score (..)) where

import Data.String.Interpolation
import Text.Parsec
import Text.Parsec.String

import Lily
import Utils
import Voice

data Score = Score { _scoreComment :: String, _scoreVoices :: [Voice] }
  deriving (Eq, Ord, Show)

instance ToLily Score where
  toLily (Score comment voices) =
    [str|% "$comment$"$endline$
        \include "articulate.ly"$endline$
        \version "2.18.2"$endline$
        structure = {$endline$
        <<$endline$
        $unwords (map toLily voices)$
        >>$endline$
        }$endline$
        \score {\structure  \layout { \context { \Voice \remove "Note_heads_engraver" \consists "Completion_heads_engraver" \remove "Rest_engraver" \consists "Completion_rest_engraver" } } }$endline$
        \score { \unfoldRepeats \articulate \structure \midi {  } }$endline$
        |]

parseScore :: Parser Score
parseScore = Score <$> (string "% " *> parseQuotedString <* string [str|$endline$\include "articulate.ly"$endline$\version "2.18.2"$endline$ structure = {$endline$<<$endline$|])
                   <*> parseVoice `sepBy` space <* string [str|>>$endline$\score {\structure \layout { \context { \Voice \remove "Note_heads_engraver" \consists "Completion_heads_engraver" \remove "Rest_engraver" \consists "Completion_rest_engraver" } } }$endline$\score { \unfoldRepeats \articulate \structure \midi { } }$endline$|]
                       
instance FromLily Score where
  parseLily = mkParseLily parseScore
