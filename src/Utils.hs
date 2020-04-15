
module Utils (symbol
             ,parseInt
             ,parseNatural
             ,parseBool
             ,mkParser
             ,mkToLily
             ,mkParseLily
             ,parseQuotedString
             ,parseQuotedIdentifier) where

import Control.Monad
import qualified Data.Map as M
import Data.Maybe
import Data.Natural
import Text.Parsec
import Text.Parsec.String

parseInt :: Parser Int
parseInt = read <$> many1 digit

parseNatural :: Parser Natural
parseNatural = read <$> many1 digit

parseBool :: Parser Bool
parseBool = (string "~" >> pure True) <|> pure False

mkParser :: String -> a -> Parser a
mkParser s d = try (string s >> pure d)

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
