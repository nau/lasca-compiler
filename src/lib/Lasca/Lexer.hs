{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE FlexibleContexts #-}
module Lasca.Lexer where

import Data.Void
import Data.Text (Text)
import qualified Data.Text as T
import Data.Scientific
import Text.Megaparsec
import Control.Monad (void)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

ops = ["+","*","-","/",";", "==", ":=", "=",",",".","<",">","|",":"]
keywords = ["module", "import", "data", "def", "extern", 
  "if", "then", "else", "in", "let", "true", "false", "match", "do", "lazy", "var", "and", "not", "or"
  ]

sc :: Parser () -- ‘sc’ stands for “space consumer”
sc = L.space (void space1) lineComment blockComment
  where lineComment  = L.skipLineComment "--"
        blockComment = L.skipBlockComment "{-" "-}"

identChar = alphaNumChar

lexeme = L.lexeme sc

symbol = L.symbol sc

integer = lexeme (try (char '0' *> char' 'x' *> L.hexadecimal)
  <|> try (char '0' *> char' 'o' *> L.octal)
  <|> try L.decimal)

stringLiteral :: Parser Text
stringLiteral = do
    char '"'
    l <- manyTill L.charLiteral (char '"')
    return $ T.pack l

float         = lexeme L.float
signedInteger = L.signed sc integer
signedFloat   = L.signed sc float
parens = between (symbol "(") (symbol ")")
brackets  = between (symbol "[") (symbol "]")
braces  = between (symbol "{") (symbol "}")
comma = symbol ","
semi = symbol ";"
commaSep p  = p `sepBy` comma
trailCommaSep p  = p `sepEndBy` comma
semiSep  p  = p `sepBy` semi

reserved :: Text -> Parser ()
reserved w = string w *> notFollowedBy identChar *> sc

reservedOp :: Text -> Parser ()
reservedOp w = string w *> notFollowedBy opChar *> sc

identOp = lexeme $ some opChar

upperIdentifier = (lexeme . try) (p)
  where
      p       = (\c cs -> T.cons c (T.pack cs)) <$> upperChar <*> many alphaNumChar

identifier :: Parser Text
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` keywords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return $ T.pack x

opChar :: Parser Char
opChar = oneOf ("!$%&*+./<=>?@\\^|-~" :: String)

operator :: Parser Text
operator = do
    op <- some opChar
    lexeme $ return $ T.pack op
