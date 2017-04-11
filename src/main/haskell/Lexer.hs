--------------------------------------------------------------------
-- |
-- Module    :  Lexer
-- Copyright :  (c) Stephen Diehl 2013
-- License   :  MIT
-- Maintainer:  stephen.m.diehl@gmail.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Lexer where

import Text.Megaparsec
import Text.Megaparsec.String
import Control.Monad (void)
import qualified Text.Megaparsec.Lexer as L

ops = ["+","*","-","/",";", "==", "=",",","<",">","|",":"]
keywords = ["data", "def", "extern", "if", "then", "else", "end", "in",
            "binary", "unary", "let", "true", "false"
            ]

sc :: Parser () -- ‘sc’ stands for “space consumer”
sc = L.space (void spaceChar) lineComment blockComment
  where lineComment  = L.skipLineComment "--"
        blockComment = L.skipBlockComment "{-" "-}"

identChar = alphaNumChar

lexeme = L.lexeme sc

symbol = L.symbol sc

integer       = lexeme L.integer

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

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` keywords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

opChar :: Parser Char
opChar = oneOf "!#$%&*+./<=>?@\\^|-~"

operator :: Parser String
operator = lexeme $ some opChar
