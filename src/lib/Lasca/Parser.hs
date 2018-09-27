{-# LANGUAGE LambdaCase #-}
module Lasca.Parser (
  parseExpr,
  parseType,
  parseToplevelFilename,
  parseToplevel
) where

import           Control.Applicative    ((<$>))
import Control.Monad.State
import qualified Data.Char as Char
import           Data.List as List
import           Data.Maybe
import           Data.Void
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.List.NonEmpty
import Data.Set (Set)
import qualified Data.Set as Set
import           Data.Foldable
import qualified Data.Scientific as Sci
import Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Expr   as Ex
import Text.Megaparsec.Char
import Text.Megaparsec.Pos
import qualified Debug.Trace as Debug


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

unitLiteral = do
    meta <- getMeta
    reserved "()"
    return $ Literal meta UnitLit

floatLit = FloatLit <$> signedFloat

boolLiteral :: Parser Expr
boolLiteral = do
    meta <- getMeta
    Literal meta <$> boolLit

boolLit :: Parser Lit
boolLit = BoolLit <$> (true <|> false)
  where
    true = reserved "true" >> return True
    false = reserved "false" >> return False


arrayLit = do
    meta <- getMeta
    Array meta <$> brackets (trailCommaSep expr)
    <?> "array literal (like [1, 2])"

stringLit :: Parser Lit
stringLit = StringLit <$> lexeme stringLiteral

interpolatedString :: Parser Expr
interpolatedString = do
    list <- lexeme pTemplate
    meta <- getMeta
    return $ go meta list
    <?> "interpolated string literal"
  where go meta [] = Literal meta (StringLit "")
        go meta list = case foldr (go' meta) [] list of
                    [s] -> s
                    strings -> Apply meta (Ident meta (NS "Prelude" "concat")) [Array meta strings]

        go' meta (Left s) acc  = Literal meta (StringLit s) : acc
        go' meta (Right e) acc = Apply meta (Ident meta "toString") [e] : acc


pTemplate :: Parser [Either Text Expr] -- Left = text, Right = variable
pTemplate = char '\"' *> manyTill piece (char '\"') <?> "interpolated string"
  where
    -- piece of text or interpolated variable
    piece =
        (Left . T.pack <$> some ch) <|>
        (Right <$> var)

    -- interpolated variable
    var = string "${" *> between sc (char '}') pVar <?> "interpolator"

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

{-binop = Ex.InfixL parser
  where parser = do
            meta <- getMeta
            (\op lhs rhs -> Apply meta (Ident meta (Name op)) [lhs, rhs]) <$> anyOperatorParser

unop = Ex.Prefix parser
  where parser = do
            meta <- getMeta
            (\op expr -> Apply meta (Ident meta (Name $ T.append "unary" op)) [expr]) <$> anyOperatorParser-}

unary s = Ex.Prefix parser
  where parser = do
            meta <- getMeta
            reservedOp s
            return (\expr -> Apply meta (Ident meta (Name $ T.append "unary" s)) [expr])

binary s = Ex.InfixL parser
  where parser = do
            meta <- getMeta
            reservedOp s >> return (\lhs rhs -> Apply meta (Ident meta (Name s)) [lhs, rhs])

{-anyOperatorParser :: Parser Text
anyOperatorParser = do
    traverse_ (notFollowedBy . reservedOp) ops
    x <- identOp
    fail $ "operator (" ++ x ++ ") is not defined"-}

postfixApply = Ex.Postfix parser
  where parser = do
            argss <- many argsApply
            meta <- getMeta
            let apply e = foldl' (Apply meta) e argss
            return apply


postfixIndex = Ex.Postfix parser
  where parser = do
            meta <- getMeta
            index <- brackets expr
            return $ \e -> Apply meta (Ident meta (NS "Array" "getIndex")) [e, index]

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
    <?> "match expression"

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
          [unary "not"],
          [binary "*", binary "/" ],
          [binary "+", binary "-" ],
          [binary "<=", binary ">=", binary "<", binary ">", binary "==" , binary "!="],
          [binary "and"],
          [binary "or"],
          [binary ":="]
         ]

operatorTable = binops -- ++ [[unop], [binop]]

expr :: Parser Expr
expr =  Ex.makeExprParser factor operatorTable

variable :: Parser Expr
variable = do
    meta <- getMeta
    Ident meta . Name <$> identifier

makeType name = if Char.isLower $ T.head name then TVar $ TV name else TypeIdent (Name name)

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
    args <- parens (trailCommaSep arg)
    tpe <- option TypeAny typeAscription
    reservedOp "="
    body <- expr
    let (lam, tpe) = curryLambda meta args body
        meta' = meta {
        _exprType = tpe, -- We need this for dynamic mode code generation
        _annots = fromMaybe [] annots
    }
    return (Let True meta' (Name name) tpe lam EmptyExpr)

extern :: Parser Expr
extern = do
    reserved "extern"
    meta <- getMeta
    reserved "def"
    name <- identifier
    args <- parens $ trailCommaSep arg
    tpe <- typeAscription
    reservedOp "="
    externName <- stringLit
    let body = Literal meta externName
    let (lam, funcType) = curryLambda meta args body
    let scheme = generalizeType funcType
    let meta' = meta { _isExternal = True, _exprType = scheme }
    return (Let True meta' (Name name) tpe lam EmptyExpr)

arg :: Parser Arg
arg = do
    name <- identifier
    tpe <- option TypeAny typeAscription
    return (Arg (Name name) tpe)

argsApply = parens (trailCommaSep expr)

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
    let folder (var, val) body = Let False meta var TypeAny val body
    return $ foldr folder body defs

closure = braces cls
  where cls = do
            args <- commaSep arg
            reservedOp "->"
            letin <- blockStmts
            meta <- getMeta
            let (lambdas, _) = curryLambda meta args letin
            return lambdas

data LetVal = Named Name Expr | Stmt Expr

valdef f = do
    try $ lookAhead valdefPrefix -- if expression starts with 'ident =' then we consume it and expect a valid value binding,
    i <- valdefPrefix            -- otherwise, alternate to unnamedStmt
    e <- expr
    return (f (Name i) e)
    <?> "name binding (e.g. four = 2 + 2)"
  where
    valdefPrefix = do
        ident <- identifier
        option TypeAny typeAscription
        reservedOp "="
        return ident

vardef f = do
    reserved "var"
    meta <- getMeta
    ident <- identifier
    option TypeAny typeAscription
    reservedOp "="
    e <- expr
    let refExpr = Apply meta (Ident meta (NS "Prelude" "Var")) [e]
    return (f (Name ident) refExpr)


unnamedStmt = do
    e <- expr
    return (Stmt e)

blockStmts = do
    exprs <- (vardef Named <|> (valdef Named) <|> unnamedStmt) `sepEndBy` semi
    let letin = foldStmtsIntoOneLetExpr (List.reverse exprs)
    return letin
  where
    foldStmtsIntoOneLetExpr [] = Literal emptyMeta UnitLit
    foldStmtsIntoOneLetExpr exprs@(lst : init) = do
        let (init', last') = case lst of
                              Stmt f@(Let True meta name tpe e EmptyExpr) -> (init, Let True meta name tpe e (Ident meta name))
                              Stmt e    -> (init, e)
                              Named _ _ -> (exprs, Literal emptyMeta UnitLit)
        let namedExprs = go init' 1
        foldl' folder last' namedExprs

    folder acc (_, Let True m name tpe e1 EmptyExpr) = {-Debug.trace (show m)-} Let True m name tpe e1 acc
    folder acc (name, e) = Let False emptyMeta name TypeAny e acc

    go (Stmt e : exprs) idx = (Name $ T.pack $ '_' : show idx, e) : go exprs (idx + 1) -- TODO make Name with number
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
        dataConstructor `sepBy1` reservedOp "|"
    return (Data meta (Name typeName) (List.map TV tvars) constructors)
    <?> "data definition"

dataConstructor = do
    name <- upperIdentifier
    args <- option [] $ parens (trailCommaSep arg)
    return (DataConst (Name name) args)

factor :: Parser Expr -- TODO remove unneeded try's, reorder
factor =  try floatingLiteral
      <|> integerLiteral
      <|> boolLiteral
      <|> letins
      <|> arrayLit
      <|> interpolatedString
      <|> variable
      <|> matchExpr
      <|> try closure
      <|> function
      <|> try unitLiteral
      <|> ifthen
      <|> block
      <|> parens expr
      <?> "expression"

globalValDef = do
    meta <- getMeta
    valdef $ (\name e -> Let False meta name TypeAny e EmptyExpr)

moduleDef = do
    reserved "module"
    meta <- getMeta
    name <- qualIdent <?> "qualified identifier (like My.Qualified.Name or SomeName)"
    hidden $ optional semi
    return $ Module meta name
    <?> "module declaration"

importDef = do
    reserved "import"
    meta <- getMeta
    name <- qualIdent <?> "qualified identifier (like My.Qualified.Name or SomeName)"
    hidden $ optional semi
    return $ Import meta name
    <?> "import statement"

annotations = some $ do
    reservedOp "@"
    identifier

defn :: Parser Expr
defn = extern
    <|> function
    <|> dataDef
    <|> globalValDef
    <?> "top-level declaration"

contents p = between sc eof p

toplevel :: Parser [Expr]
toplevel = do
    meta <- getMeta
    mod <- optional moduleDef
    imports <- many importDef
    defs <- many defn
    return (maybeToList mod ++ imports ++ defs)

parseWith :: Parser a -> Text ->Either (Megaparsec.ParseError Char Void) a
parseWith parser s = parse (contents parser) "<stdin>" s

parseExpr = parseWith expr

parseToplevelFilename fileName s = parse (contents toplevel) fileName s

parseToplevel s = parseToplevelFilename "<stdin>" s

parseType = parseWith typeExpr
