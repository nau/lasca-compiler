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
import Type

integerLit :: Parser Expr
integerLit = Literal . IntLit . fromIntegral <$> integer

floating :: Parser Expr
floating = Literal . FloatLit <$> float

strToBool :: String -> Bool
strToBool "true" = True
strToBool _ = False

boolLit :: Parser Expr
boolLit = Literal . BoolLit . strToBool <$> (true <|> false)
  where
    true = reserved "true" >> return "true"
    false = reserved "false" >> return "false"

stringLit :: Parser Expr
stringLit = Literal . StringLit <$> stringLiteral

binop = Ex.Infix parser Ex.AssocLeft
  where parser = (\op lhs rhs -> Apply (Var op) [lhs, rhs]) <$> op
unop = Ex.Prefix parser
  where parser = (\op expr -> Apply (Var ("unary" ++ op)) [expr]) <$> op

binary s assoc = Ex.Infix parser assoc
  where parser = reservedOp s >> return (\lhs rhs -> Apply (Var s) [lhs, rhs])

op :: Parser String
op = do
  whitespace
  o <- operator
  whitespace
  return o

binops = [
          [binary "*" Ex.AssocLeft, binary "/" Ex.AssocLeft],
          [binary "+" Ex.AssocLeft, binary "-" Ex.AssocLeft],
          [binary "<" Ex.AssocLeft, binary "==" Ex.AssocLeft, binary "!=" Ex.AssocLeft],
          [binary "and" Ex.AssocLeft],
          [binary "or" Ex.AssocLeft]
          ]

operatorTable = binops ++ [[unop], [binop]]

expr :: Parser Expr
expr =  Ex.buildExpressionParser operatorTable factor

variable :: Parser Expr
variable = Var <$> identifier

typeAscription :: Parser Type
typeAscription = do
  reservedOp ":"
  name <- identifier
  return $ TCon name

funcArgument :: Parser Name
funcArgument = do
  name <- identifier
  typeAsc <- option typeAny typeAscription
  return name

function :: Parser Expr
function = do
  reserved "def"
  name <- identifier
  args <- parens $ commaSep arg
  tpe <- option typeAny typeAscription
  reservedOp "="
  body <- expr
  return (Function name tpe args body)

extern :: Parser Expr
extern = do
  reserved "extern"
  reserved "def"
  name <- identifier
  args <- parens $ commaSep arg
  reservedOp ":"
  tpe <- identifier
  return (Extern name (TCon tpe) args)

arg :: Parser Arg
arg = do
  name <- identifier
  tpe <- option typeAny typeAscription
  return (Arg name tpe)

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return (Apply (Var name) args)

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reserved "then"
  tr <- expr
  reserved "else"
  fl <- expr
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

block :: Parser Expr
block = do
  exprs <- braces (expr `sepEndBy` semi)
  let letins = if null exprs
               then Literal UnitLit
               else do
                 let init' = init exprs
                 let last' = last exprs
                 let namedExprs = zip ['a'..] init'
                 let letin = foldr (\(name, e) acc -> (Let (show name) e acc)) last' namedExprs
                 letin
  return letins

factor :: Parser Expr
factor = try floating
      <|> try boolLit
      <|> try letins
      <|> try stringLit
      <|> try integerLit
      <|> try call
      <|> try variable
      <|> ifthen
      <|> block
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
