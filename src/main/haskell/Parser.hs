--------------------------------------------------------------------
-- |
-- Module    :  Parser
-- Copyright :  (c) Stephen Diehl 2013
-- License   :  MIT
-- Maintainer:  stephen.m.diehl@gmail.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Data.Text as T
import Control.Applicative ((<$>))

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

integerLit :: Parser Expr
integerLit = Literal . IntLit . fromIntegral <$> integer

floating :: Parser Expr
floating = Literal . FloatLit <$> float

strToBool :: String -> Bool
strToBool "true" = True
strToBool _ = False

boolLit :: Parser Expr
boolLit = Literal . BoolLit . strToBool <$> (string "true" <|> string "false")

stringLit :: Parser Expr
stringLit = Literal . StringLit <$> stringLiteral

binop = Ex.Infix (BinaryOp <$> op) Ex.AssocLeft
unop = Ex.Prefix (UnaryOp <$> op)

binary s assoc = Ex.Infix (reservedOp s >> return (BinaryOp s)) assoc

op :: Parser String
op = do
  whitespace
  o <- operator
  whitespace
  return o

binops = [
          [binary "*" Ex.AssocLeft, binary "/" Ex.AssocLeft],
          [binary "+" Ex.AssocLeft, binary "-" Ex.AssocLeft],
          [binary "<" Ex.AssocLeft, binary "==" Ex.AssocLeft],
          [binary "and" Ex.AssocLeft],
          [binary "or" Ex.AssocLeft]
          ]

expr :: Parser Expr
expr =  Ex.buildExpressionParser (binops ++ [[unop], [binop]]) factor

variable :: Parser Expr
variable = Var <$> identifier

typeAscription :: Parser Type
typeAscription = do
  reservedOp ":"
  name <- identifier
  return $ Type name

funcArgument :: Parser Name
funcArgument = do
  name <- identifier
  typeAsc <- option AnyType typeAscription
  return name

function :: Parser Expr
function = do
  reserved "def"
  name <- identifier
  args <- parens $ commaSep arg
  tpe <- option AnyType typeAscription
  reservedOp "="
  body <- expr
  reserved "end"
  return (Function name tpe args body)

extern :: Parser Expr
extern = do
  reserved "extern"
  reserved "def"
  name <- identifier
  args <- parens $ commaSep arg
  reservedOp ":"
  tpe <- identifier
  return (Extern name (Type tpe) args)

arg :: Parser Arg
arg = do
  name <- identifier
  tpe <- option AnyType typeAscription
  return (Arg name tpe)

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return (Apply name args)

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reserved "then"
  tr <- expr
  reserved "else"
  fl <- expr
  reserved "end"
  return (If cond tr fl)

letins :: Parser Expr
letins = do
  reserved "let"
  defs <- commaSep $ do
    var <- identifier
    reservedOp "="
    val <- expr
    return (var, val)
  reserved "in"
  body <- expr
  return $ foldr (uncurry Let) body defs

factor :: Parser Expr
factor = try floating
      <|> try letins
      <|> try boolLit
      <|> try stringLit
      <|> try integerLit
      <|> try call
      <|> try variable
      <|> ifthen
      <|> (parens expr)

defn :: Parser Expr
defn = try extern
    <|> try function
    <|> expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many $ do
    def <- defn
    return def

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents toplevel) "<stdin>" s
