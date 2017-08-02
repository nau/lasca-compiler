module Lexer where

import Text.Megaparsec
import Text.Megaparsec.String
import Control.Monad (void)
import qualified Text.Megaparsec.Lexer as L

ops = ["+","*","-","/",";", "==", ":=", "=",",",".","<",">","|",":"]
keywords = ["data", "def", "extern", "if", "then", "else", "in", "let", "true", "false", "match", "do", "lazy", "var"]

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
