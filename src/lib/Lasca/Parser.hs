module Lasca.Parser (
  parseExpr,
  parseToplevelFilename,
  parseToplevel
) where

import           Control.Applicative    ((<$>))
import Control.Monad.State
import qualified Data.Char as Char
import           Data.List as List
import           Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.List.NonEmpty
import Data.Set (Set)
import qualified Data.Set as Set
import           Data.Foldable
import qualified Data.Scientific as Sci
import           Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Expr   as Ex
import Text.Megaparsec.Char
import Text.Megaparsec.Pos


import           Lasca.Lexer
import  Lasca.Syntax as S
import           Lasca.Type
import Lasca.Infer

getMeta = do
    state <- getParserState
    let SourcePos _ sl sc :| _ = statePos state
    let meta = emptyMeta { pos = Position {S.sourceLine = unPos sl, S.sourceColumn = unPos sc} }
    return meta

integerLiteral = do
    meta <- getMeta
    Literal meta <$> integerLit

integerLit = IntLit . fromIntegral <$> signedInteger

floatingLiteral :: Parser Expr
floatingLiteral = do
    meta <- getMeta
    Literal meta <$> floatLit

floatLit = FloatLit <$> signedFloat

strToBool :: String -> Bool
strToBool "true" = True
strToBool _      = False

boolLiteral :: Parser Expr
boolLiteral = do
    meta <- getMeta
    Literal meta <$> boolLit

boolLit :: Parser Lit
boolLit = BoolLit . strToBool <$> (true <|> false)
  where
    true = reserved "true" >> return "true"
    false = reserved "false" >> return "false"


arrayLit = do
    meta <- getMeta
    Array meta <$> brackets (commaSep expr)

stringLit = StringLit <$> stringLiteral

interpolatedString :: Parser Expr
interpolatedString = do
    list <- pTemplate
    meta <- getMeta
    return $ go meta list
  where go meta [] = Literal meta (StringLit "")
        go meta list = case foldr (go' meta) [] list of
                    [s] -> s
                    strings -> Apply meta (Ident meta "concat") [Array meta strings]

        go' meta (Left s) acc  = Literal meta (StringLit s) : acc
        go' meta (Right e) acc = Apply meta (Ident meta "toString") [e] : acc


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
            (\op lhs rhs -> Apply meta (Ident meta op) [lhs, rhs]) <$> anyOperatorParser

unop = Ex.Prefix parser
  where parser = do
            meta <- getMeta
            (\op expr -> Apply meta (Ident meta (Name $ "unary" ++ op)) [expr]) <$> anyOperatorParser

unary s = Ex.Prefix parser
  where parser = do
            meta <- getMeta
            reservedOp s
            return (\expr -> Apply meta (Ident meta (Name $ "unary" ++ s)) [expr])

binary s = Ex.InfixL parser
  where parser = do
            meta <- getMeta
            reservedOp s >> return (\lhs rhs -> Apply meta (Ident meta (Name s)) [lhs, rhs])

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


postfixIndex = Ex.Postfix parser
  where parser = do
            meta <- getMeta
            index <- brackets expr
            return $ \e -> Apply meta (Ident meta "arrayApply") [e, index]

select = do
    reservedOp "."
    meta <- getMeta
    return $ Select meta

matchExpr = do
    reserved "match"
    meta <- getMeta
    ex <- expr
    cs <- bracedCases
    return $ Match meta ex cs

bracedCases = braces $ some acase

acase = do
--    reservedOp "|"
    p <- ptrn
    reservedOp "->"
    e <- expr
    return $ Case p e

ptrn = litPattern
    <|> constrPattern
    <|> (VarPattern . Name <$> identifier)
    <|> wildcardPattern

constrPattern = do
    id <- upperIdentifier
    patterns <- option [] $ parens (ptrn `sepBy` comma)
    return $ ConstrPattern (Name id) patterns

wildcardPattern = do
    reservedOp "_"
    return WildcardPattern

litPattern = LitPattern <$> (boolLit <|> stringLit <|> try floatLit <|> integerLit)

qi = do
         i <- identifier
         reservedOp "."
         r <- qualIdent
         return $ NS (Name i) r

qualIdent = try qi <|> (Name <$> identifier)

binops = [
          [postfixIndex, Ex.InfixL select],
          [postfixApply],
          [unary "-"],
          [binary "*", binary "/" ],
          [binary "+", binary "-" ],
          [binary "<=", binary ">=", binary "<", binary ">", binary "==" , binary "!="],
          [binary "and"],
          [binary "or"],
          [binary ":="]
         ]

operatorTable = binops ++ [[unop], [binop]]

expr :: Parser Expr
expr =  Ex.makeExprParser factor operatorTable

variable :: Parser Expr
variable = do
    meta <- getMeta
    Ident meta . Name <$> identifier

makeType name = if Char.isLower $ List.head name then TVar $ TV name else TypeIdent (Name name)

typeTerm = (TypeArray <$> brackets typeExpr)
       <|> parens typeExpr
       <|> (makeType <$> identifier)

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
    annots <- optional annotations
    reserved "def"
    meta <- getMeta
    name <- identifier
    args <- parens (commaSep arg)
    tpe <- option TypeAny typeAscription
    reservedOp "="
    body <- expr
    let meta' = meta {
        _exprType = foldr (TypeFunc . const TypeAny) tpe args, -- We need this for dynamic mode code generation
        _annots = fromMaybe [] annots
    }
    return (Function meta' (Name name) tpe args body)

extern :: Parser Expr
extern = do
    reserved "extern"
    meta <- getMeta
    reserved "def"
    name <- identifier
    args <- parens $ commaSep arg
    tpe <- typeAscription
    reservedOp "="
    externName <- lexeme stringLit
    let body = Literal meta externName
    let funcType = foldr (\(Arg n at) ft -> TypeFunc at ft) tpe (List.reverse args)
    let scheme = normalizeType funcType
    let meta' = meta { _isExternal = True, _exprType = scheme }
    return (Function meta' (Name name) tpe args body)

arg :: Parser Arg
arg = do
    name <- identifier
    tpe <- option TypeAny typeAscription
    return (Arg (Name name) tpe)

argsApply = parens (commaSep expr)

ifthen :: Parser Expr
ifthen = do
    reserved "if"
    meta <- getMeta
    cond <- expr
    reserved "then"
    tr <- expr
    reserved "else"
    fl <- expr
    return (If meta cond tr fl)

letins :: Parser Expr
letins = do
    reserved "let"
    meta <- getMeta
    defs <- commaSep $ do
        var <- identifier
        option TypeAny typeAscription
        reservedOp "="
        val <- expr
        return (Name var, val)
    reserved "in"
    body <- expr
    return $ foldr (uncurry $ Let meta) body defs

closure = braces cls
  where cls = do
            args <- commaSep arg
            reservedOp "->"
            letin <- blockStmts
            meta <- getMeta
            let (lambdas, _) = foldr (\arg (body, tpe) ->
                        let lambdaType = TypeFunc TypeAny tpe in
                        (Lam (meta `withType` lambdaType) arg body, lambdaType)) (letin, TypeAny) args
            return lambdas

data LetVal = Named Name Expr | Stmt Expr

valdef f = do
    ident <- identifier
    option TypeAny typeAscription
    reservedOp "="
    e <- expr
    return (f (Name ident) e)

vardef f = do
    reserved "var"
    meta <- getMeta
    ident <- identifier
    option TypeAny typeAscription
    reservedOp "="
    e <- expr
    let refExpr = Apply meta (Ident meta "Ref") [e]
    return (f (Name ident) refExpr)


unnamedStmt = do
    e <- expr
    return (Stmt e)

blockStmts = do
    exprs <- (try (vardef Named) <|> try (valdef Named) <|> unnamedStmt) `sepEndBy` semi
    let letin = foldStmtsIntoOneLetExpr (List.reverse exprs)
    return letin
  where
    foldStmtsIntoOneLetExpr [] = Literal emptyMeta UnitLit
    foldStmtsIntoOneLetExpr exprs@(lst : init) = do
        let (init', last') = case lst of
                              (Stmt e)    -> (init, e)
                              (Named _ _) -> (exprs, Literal emptyMeta UnitLit)
        let namedExprs = go init' 1
        foldl' (\acc (name, e) -> Let emptyMeta name e acc) last' namedExprs

    go (Stmt e : exprs) idx = (Name $ '_' : show idx, e) : go exprs (idx + 1)
    go (Named id e : exprs) idx = (id, e) : go exprs idx
    go [] _ = []


block :: Parser Expr
block = braces blockStmts

dataDef :: Parser Expr
dataDef = do
    reserved "data"
    meta <- getMeta
    typeName <- upperIdentifier
    tvars <- many identifier
    constructors <- option [] $ do
        reservedOp "="
        optional $ reservedOp "|"
        dataConstructor `sepBy` reservedOp "|"
    return (Data meta (Name typeName) (List.map TV tvars) constructors)

dataConstructor = do
    name <- identifier
    args <- option [] $ parens (arg `sepEndBy` comma)
    return (DataConst (Name name) args)

factor :: Parser Expr -- TODO remove unneeded try's, reorder
factor =  try floatingLiteral
      <|> try boolLiteral
      <|> try letins
      <|> try arrayLit
      <|> try interpolatedString
      <|> try integerLiteral
      <|> try variable
      <|> try matchExpr
      <|> try closure
      <|> try function
      <|> ifthen
      <|> block
      <|> parens expr

globalValDef = do
    meta <- getMeta
    valdef $ (\name e -> Let meta name e EmptyExpr)

moduleDef = do
    reserved "module"
    meta <- getMeta
    name <- qualIdent
    return $ Module meta name

importDef = do
    reserved "import"
    meta <- getMeta
    name <- qualIdent
    return $ Import meta name

annotations = some $ do
    reservedOp "@"
    identifier

defn :: Parser Expr
defn =  try moduleDef
    <|> try importDef
    <|> try extern
    <|> try function
    <|> try dataDef
    <|> globalValDef

contents p = between sc eof p

toplevel :: Parser [Expr]
toplevel = many $ defn

parseExpr s = parse (contents expr) "<stdin>" s

parseToplevelFilename fileName s = parse (contents toplevel) fileName s

parseToplevel s = parseToplevelFilename "<stdin>" s
