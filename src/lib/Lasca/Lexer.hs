{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE FlexibleContexts #-}
module Lasca.Lexer where

import Data.Void
import Data.Scientific
import Text.Megaparsec
import Control.Monad (void)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

ops = ["+","*","-","/",";", "==", ":=", "=",",",".","<",">","|",":"]
keywords = ["module", "import", "data", "def", "extern", "if", "then", "else", "in", "let", "true", "false", "match", "do", "lazy", "var"]

sc :: Parser () -- ‘sc’ stands for “space consumer”
sc = L.space (void space1) lineComment blockComment
  where lineComment  = L.skipLineComment "--"
        blockComment = L.skipBlockComment "{-" "-}"

identChar = alphaNumChar

lexeme = L.lexeme sc

symbol = L.symbol sc

integer       = lexeme L.decimal

stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

float         = lexeme L.float
signedInteger = L.signed sc integer
signedFloat   = L.signed sc float
parens = between (symbol "(") (symbol ")")
brackets  = between (symbol "[") (symbol "]")
braces  = between (symbol "{") (symbol "}")
comma = symbol ","
semi = symbol ";"
commaSep p  = p `sepBy` comma
semiSep  p  = p `sepBy` semi

reserved :: String -> Parser ()
reserved w = string w *> notFollowedBy identChar *> sc

reservedOp :: String -> Parser ()
reservedOp w = string w *> notFollowedBy opChar *> sc

identOp = lexeme $ some opChar

upperIdentifier = (lexeme . try) (p)
  where
      p       = (:) <$> upperChar <*> many alphaNumChar

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` keywords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

opChar :: Parser Char
opChar = oneOf ("!$%&*+./<=>?@\\^|-~" :: String)

operator :: Parser String
operator = lexeme $ some opChar
