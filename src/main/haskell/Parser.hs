{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Strict            #-}
module Parser (
  parseExpr,
  parseInterpol,
  parseToplevel
) where

import           Control.Applicative    ((<$>))
import Control.Monad.State
import qualified Data.Char as Char
import           Data.List as List
import qualified Data.Map.Strict as Map
import           Data.List.NonEmpty
import           Data.Foldable
import           Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Expr   as Ex
import           Text.Megaparsec.String
import Text.Megaparsec.Pos


import           Lexer
import Syntax
import           Type

getMeta = do
  state <- getParserState
  let SourcePos _ sl sc :| _ = statePos state
  let meta = emptyMeta { pos = Position {Syntax.sourceLine = unPos sl, Syntax.sourceColumn = unPos sc} }
  return meta

integerLit :: Parser Expr
integerLit = do
  meta <- getMeta
  Literal meta . IntLit . fromIntegral <$> signedInteger

floating :: Parser Expr
floating = Literal emptyMeta . FloatLit <$> signedFloat

strToBool :: String -> Bool
strToBool "true" = True
strToBool _      = False

boolLit :: Parser Expr
boolLit = Literal emptyMeta . BoolLit . strToBool <$> (true <|> false)
  where
    true = reserved "true" >> return "true"
    false = reserved "false" >> return "false"

arrayLit = Array <$> brackets (commaSep expr)

stringLit :: Parser Expr
stringLit = Literal emptyMeta . StringLit <$> stringLiteral

interpolatedString :: Parser Expr
interpolatedString = do
  list <- pTemplate
  return $ go list
  where go [] = Literal emptyMeta (StringLit "")
        go list = case foldr go' [] list of
                    [s] -> s
                    strings -> Apply emptyMeta (Ident "concat") [Array strings]

        go' (Left s) acc  = Literal emptyMeta (StringLit s) : acc
        go' (Right e) acc = Apply emptyMeta (Ident "toString") [e] : acc


pTemplate :: Parser [Either String Expr] -- Left = text, Right = variable
pTemplate = char '\"' *> manyTill piece (char '\"')
  where
    -- piece of text or interpolated variable
    piece =
      (Left <$> some ch) <|>
      (Right <$> var)

    -- interpolated variable
    var = string "${" *> between sc (char '}') pVar

    -- normal character, plain or escaped
    ch = noneOf escapable <|> ppp

    -- set of escapable characters

    ppp = do
      char '\\'
      k <- oneOf escapes
      return $ mapping Map.! k

    mapping = Map.union escapableMap escapesMap

    escapable = ['"', '\\', '$']
    
    escapableMap = Map.fromList $ List.zip escapable escapable
    
    escapesMap = Map.fromList [('n', '\n'), ('t', '\t'), ('r', '\r'), ('b', '\b'), ('0', '\0')] -- TODO complete it
    escapes = escapable ++ Map.keys escapesMap

    pVar = expr

binop = Ex.InfixL parser
  where parser = do
          meta <- getMeta
          (\op lhs rhs -> Apply meta (Ident op) [lhs, rhs]) <$> anyOperatorParser

unop = Ex.Prefix parser
  where parser = do
          meta <- getMeta
          (\op expr -> Apply meta (Ident ("unary" ++ op)) [expr]) <$> anyOperatorParser

binary s = Ex.InfixL parser
  where parser = do
           meta <- getMeta
           reservedOp s >> return (\lhs rhs -> Apply meta (Ident s) [lhs, rhs])

anyOperatorParser = do
  traverse_ (notFollowedBy . reservedOp) ops
  x <- identOp
  fail $ "operator (" ++ x ++ ") is not defined"

postfixApply = Ex.Postfix parser  -- FIXME shouldn't it be InfixL?
  where parser = do
          argss <- many argsApply
          meta <- getMeta
          let apply e = foldl (Apply meta) e argss
          return apply


select = do
  reservedOp "."
  meta <- getMeta
  return $ Select meta

binops = [
          [Ex.InfixL select, postfixApply],
          [binary "*", binary "/" ],
          [binary "+", binary "-" ],
          [binary "<=", binary ">=", binary "<", binary ">", binary "==" , binary "!="],
          [binary "and"],
          [binary "or"]
         ]

operatorTable = binops ++ [[unop], [binop]]

expr :: Parser Expr
expr =  Ex.makeExprParser factor operatorTable

variable :: Parser Expr
variable = Ident <$> identifier

makeType name = if Char.isLower $ List.head name then TVar $ TV name else TypeIdent name

typeTerm = makeType <$> identifier
       <|> parens typeExpr

typeFunc =  do
  reservedOp "->"
  return TypeFunc

typeApply = do
--  args <- typeExpr
  let apply (TypeApply ll rr) r = TypeApply ll (rr ++ [r])
      apply l r = TypeApply l [r]
  return apply

typeOperatorTable = [[Ex.InfixL typeApply], [Ex.InfixR typeFunc]]

typeExpr = Ex.makeExprParser typeTerm typeOperatorTable

typeAscription :: Parser Type
typeAscription = do
  reservedOp ":"
  typeExpr

function :: Parser Expr
function = do
  reserved "def"
  name <- identifier
  args <- parens (commaSep arg)
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
  tpe <- typeAscription
  return (Extern name tpe args)

arg :: Parser Arg
arg = do
  name <- identifier
  tpe <- option typeAny typeAscription
  return (Arg name tpe)

argsApply = parens (commaSep expr)

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
    option typeAny typeAscription
    reservedOp "="
    val <- expr
    return (var, val)
  reserved "in"
  body <- expr
  return $ foldr (uncurry Let) body defs

closure = braces cls
    where cls = do
            args <- commaSep arg
            reservedOp "->"
            letin <- blockStmts
            let lambdas = foldr (\(Arg a _) expr -> Lam a expr) letin args
            return lambdas

data LetVal = Named Name Expr | Stmt Expr

valdef f = do
  id <- identifier
  option typeAny typeAscription
  reservedOp "="
  e <- expr
  return (f id e)

unnamedStmt = do
  e <- expr
  return (Stmt e)

blockStmts = do
  exprs <- (try (valdef Named) <|> unnamedStmt) `sepEndBy` semi
  let letin = foldStmtsIntoOneLetExpr (List.reverse exprs)
  return letin
  where
        foldStmtsIntoOneLetExpr [] = Literal emptyMeta UnitLit
        foldStmtsIntoOneLetExpr exprs@(lst : init) = do
          let (init', last') = case lst of
                                (Stmt e)    -> (init, e)
                                (Named _ _) -> (exprs, Literal emptyMeta UnitLit)
          let namedExprs = go init' 1
          foldl' (\acc (name, e) -> Let name e acc) last' namedExprs

        go (Stmt e : exprs) idx = ('_' : show idx, e) : go exprs (idx + 1)
        go (Named id e : exprs) idx = (id, e) : go exprs idx
        go [] _ = []


block :: Parser Expr
block = braces blockStmts

dataDef :: Parser Expr
dataDef = do
  reserved "data"
  typeName <- identifier
  reservedOp "="
  optional $ reservedOp "|"
  constructors <-  dataConstructor `sepBy` reservedOp "|"
  return (Data typeName constructors)

dataConstructor = do
  name <- identifier
  args <- option [] $ parens (arg `sepEndBy` comma)
  return (DataConst name args)

factor :: Parser Expr -- TODO remove unneeded try's, reorder
factor = try floating
      <|> try boolLit
      <|> try letins
      <|> try arrayLit
--      <|> try stringLit
      <|> try interpolatedString
      <|> try integerLit
      <|> try variable
      <|> try closure
      <|> ifthen
      <|> block
      <|> parens expr

defn :: Parser Expr
defn = try extern
    <|> try function
    <|> try dataDef
    <|> valdef Val

contents p = between sc eof p

toplevel :: Parser [Expr]
toplevel = many $ defn

parseExpr s = parse (contents expr) "<stdin>" s

parseInterpol :: String -> Either (ParseError Char Dec) [Either String Expr]
parseInterpol s = parse (contents pTemplate) "<stdin>" s

parseToplevel s = parse (contents toplevel) "<stdin>" s
