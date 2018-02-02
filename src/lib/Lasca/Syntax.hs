{-# LANGUAGE TemplateHaskell #-}
module Lasca.Syntax where

--import           Data.Text
import           Data.List
import           Text.Printf
import qualified Text.Megaparsec as Megaparsec
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Debug.Trace as Debug
import Control.Monad.State
import Control.Monad.Except
import Control.Applicative
import qualified Control.Lens as Lens
import Control.Lens.Operators
import Control.Lens.TH
import           Lasca.Type
import Lasca.Options

data Position = NoPosition | Position {sourceLine :: Int, sourceColumn :: Int} deriving (Eq, Ord)

data Meta = Meta {
    pos :: Position,
    _exprType :: Type,
    _isExternal :: Bool,
    _annots :: [String]
} deriving (Eq, Ord)
makeLenses ''Meta

instance Show Meta where
--  show (Meta pos tpe) = "Meta{pos=" ++ show pos ++ ", tpe=" ++ show tpe ++ "}"
    show meta = show (_annots meta) ++ " " ++ show (_exprType meta)

instance Show Position where
    show NoPosition = "<unknown>"
    show Position{sourceLine = sl, sourceColumn = sc} = show sl ++ ":" ++ show sc

emptyMeta = Meta { pos = NoPosition, _exprType = typeAny, _isExternal = False, _annots = [] }

withMetaPos line col = emptyMeta { pos = Position {sourceLine = line, sourceColumn = col} }

emptyMetaWithType s = emptyMeta { _exprType = s }

withType meta t = meta { _exprType = t }

data Expr
    = EmptyExpr
    | Literal Meta Lit
    | Ident Meta Name
    | Apply Meta Expr [Expr]
    | Lam Meta Arg Expr
    | Select Meta Expr Expr
    | Match Meta Expr [Case]
    | BoxFunc Meta Name [Arg]   -- LLVM codegen only
    | Function Meta Name Type [Arg] Expr
    | If Meta Expr Expr Expr
    | Let Meta Name Expr Expr
    | Array Meta [Expr]
    | Data Meta Name [TVar] [DataConst]
    | Package Meta Name
    | Import Meta Name
    deriving (Ord, Show)

metaLens :: Lens.Lens' Expr Meta
metaLens = Lens.lens (fst . getset) (snd . getset)
    where getset expr = case expr of
              Literal meta lit -> (meta, \ m -> Literal m lit)
              Ident meta name -> (meta, \ m -> Ident m name)
              Apply meta expr args -> (meta, \ m -> Apply m expr args)
              Lam meta name expr -> (meta, \ m -> Lam m name expr)
              Select meta tree expr -> (meta, \ m -> Select m tree expr)
              Match meta expr cases -> (meta, \ m -> Match m expr cases)
              Function meta name tpe args body -> (meta, \ m -> Function m name tpe args body)
              If meta cond tr fl -> (meta, \ m -> If m cond tr fl)
              Let meta name expr body -> (meta, \ m -> Let m name expr body)
              Array meta exprs -> (meta, \ m -> Array m exprs)
              Data meta name tvars constrs -> (meta, \ m -> Data m name tvars constrs)
              BoxFunc meta name args -> (meta, \m -> BoxFunc m name args)
              Package meta name -> (meta, \m -> Package m name)
              Import meta name -> (meta, \m -> Import m name)
              _ -> error $ "Should not happen :) " ++ show expr

exprPosition expr = pos (expr ^. metaLens)

showPosition meta = show (pos meta)

getExprType expr = expr^.metaLens.exprType

typeOf = fromSchema . getExprType

fromSchema (Forall _ t) = t
fromSchema t = t

instance Eq Expr where
    EmptyExpr == EmptyExpr = True
    (Literal _ l) == (Literal _ r) = l == r
    (Ident _ l) == (Ident _ r) = l == r
    (Apply _ nl l) == (Apply _ nr r) = nl == nr && l == r
    (Lam _ nl l) == (Lam _ nr r) = nl == nr && l == r
    (Select _ nl l) == (Select _ nr r) = nl == nr && l == r
    (Match _ nl l) == (Match _ nr r) = nl == nr && l == r
    (BoxFunc _ nl l) == (BoxFunc _ nr r) = nl == nr && l == r
    (Function metal nl _ al l) == (Function metar nr _ ar r) = nl == nr && al == ar && l == r && (metal^.isExternal) == (metar^.isExternal)
    (If _ nl al l) == (If _ nr ar r) = nl == nr && al == ar && l == r
    (Let _ nl al l) == (Let _ nr ar r) = nl == nr && al == ar && l == r
    (Array _ l) == (Array _ r) = l == r
    (Data _ nl ltvars l) == (Data _ nr rtvars r) = nl == nr && ltvars == rtvars && l == r
    Package _ ln == Package _ rn = ln == rn
    Import _ ln == Import _ rn = ln == rn
    _ == _ = False

{-
instance Show Expr where
  show (Literal _ l) = show l
  show (Ident meta n) = printf "%s: %s" n (show meta)
  show (Apply meta f e) = printf "%s(%s): %s" (show f) (intercalate "," $ map show e) (show meta)
  show (Lam _ a e) = printf "{ %s -> %s }\n" (show a) (show e)
  show (Select _ e f) = printf "%s.%s" (show e) (show f)
  show (Match _ e cs) = printf "match %s {\n%s}\n" (show e) (show cs)
  show (BoxFunc meta f args) = printf "BoxFunc %s($args)" (show f) (intercalate "," $ map show args)
  show (Function meta f t args b) =
      if _isExternal meta then
           printf "extern def %s(%s): %s\n" f (intercalate "," $ map show args) (show t)
      else printf "%s : %s\ndef %s(%s): %s = %s\n" f (show meta) f (intercalate "," $ map show args) (show t) (show b)
  show (If meta c t f) = printf "if %s then {\n%s \n} else {\n%s\n}: %s" (show c) (show t) (show f) (show meta)
  show (Let meta n e b) = printf "%s = %s;\n%s: %s" n (show e) (show b) (show meta)
  show (Array _ es) = printf "[%s]" (intercalate "," $ map show es)
  show (Data _ n cs) = printf "data %s = %s\n" n (intercalate "\n| " $ map show cs)
  show EmptyExpr = ""
-}

class Show a => DebugPrint a where
    printExprWithType :: a -> String
    printExprWithType a = show a

instance DebugPrint Arg where
    printExprWithType (Arg a t) = printf "%s: %s" (show a) (show t)

instance DebugPrint Expr where
    printExprWithType expr = case expr of
        Literal _ l -> show l
        Ident meta n -> printf "%s: %s" (show n) (show meta)
        Apply meta f e -> printf "%s(%s): %s" (printExprWithType f) (intercalate "," $ map printExprWithType e) (show meta)
        Lam _ a e -> printf "{ %s -> %s }\n" (printExprWithType a) (printExprWithType e)
        Select _ e f -> printf "%s.%s" (printExprWithType e) (printExprWithType f)
        Match _ e cs -> printf "match %s {\n%s}\n" (printExprWithType e) (show cs)
        BoxFunc meta f args -> printf "BoxFunc %s(%s)" (show f) (intercalate "," $ map show args)
        Function meta f t args b -> if _isExternal meta
            then printf "extern def %s(%s): %s\n" (show f) (intercalate "," $ map printExprWithType args) (show t)
            else printf "--  %s : %s\ndef %s(%s): %s = %s\n" (show f) (show meta) (show f) (intercalate "," $ map printExprWithType args) (show t) (printExprWithType b)
        If meta c t f -> printf "if %s then {\n%s \n} else {\n%s\n}: %s" (printExprWithType c) (printExprWithType t) (printExprWithType f) (show meta)
        Let meta n e b -> printf "%s = %s;\n%s: %s" (show n) (printExprWithType e) (printExprWithType b) (show meta)
        Array _ es -> printf "[%s]" (intercalate "," $ map printExprWithType es)
        Data _ n tvars cs -> printf "data %s %s = %s\n" (show n) (show tvars) (intercalate "\n| " $ map show cs)
        Package meta name -> printf "package %s" (show name)
        Import meta name -> printf "import %s" (show name)
        EmptyExpr -> ""


data Case = Case Pattern Expr deriving (Eq, Ord, Show)
data Pattern
    = LitPattern Lit
    | ConstrPattern Name [Pattern]
    | VarPattern Name
    | WildcardPattern
    deriving (Eq, Ord, Show)

data DataConst = DataConst Name [Arg] deriving (Eq, Ord, Show)

data DataDef = DataDef Name [DataConst]
    deriving (Show, Eq)

emptyCtx opts = Context {
    _lascaOpts = opts,
    _packageName = Name defaultPackageName,
    _globalFunctions = Map.empty,
    _globalVals = Set.empty,
    _dataDefs = [],
    _dataDefsNames = Set.empty,
    _dataDefsFields = Map.empty
}

data Lit = IntLit Int
    | FloatLit Double
    | BoolLit Bool
    | UnitLit
    | StringLit String
    deriving (Eq, Ord)

instance Show Lit where
    show (IntLit i) = show i
    show (FloatLit f) = show f
    show (BoolLit b) = show b
    show  UnitLit = "()"
    show (StringLit s) = "\"" ++ s ++ "\""


data Arg = Arg Name Type deriving (Eq, Ord, Show)

data Ctx = Context {
    _lascaOpts :: LascaOpts,
    _packageName :: Name,
    _globalFunctions :: Map Name Expr,
    _globalVals :: Set Name,
    _dataDefs :: [DataDef],
    _dataDefsNames :: Set Name,
    _dataDefsFields :: Map Name (Map Name (Arg, Int))
} deriving (Show, Eq)
makeLenses ''Ctx

builtinFunctions :: Map Name Type
builtinFunctions = Map.fromList [
    ("unary-", Forall [a] (ta `TypeFunc` ta)),
    (":=", Forall [a] ((typeRef ta) `TypeFunc` ta `TypeFunc` (typeRef ta))),
    ("+",  Forall [a] (ta `TypeFunc` ta `TypeFunc` ta)),
    ("-",  Forall [a] (ta `TypeFunc` ta `TypeFunc` ta)),
    ("*",  Forall [a] (ta `TypeFunc` ta `TypeFunc` ta)),
    ("/",  Forall [a] (ta `TypeFunc` ta `TypeFunc` ta)),
    ("==", Forall [a] (ta `TypeFunc` ta `TypeFunc` TypeBool)),
    ("!=", Forall [a] (ta `TypeFunc` ta `TypeFunc` TypeBool)),
    ("<",  Forall [a] (ta `TypeFunc` ta `TypeFunc` TypeBool)),
    ("<=", Forall [a] (ta `TypeFunc` ta `TypeFunc` TypeBool)),
    (">",  Forall [a] (ta `TypeFunc` ta `TypeFunc` TypeBool)),
    (">=", Forall [a] (ta `TypeFunc` ta `TypeFunc` TypeBool))
  ]
  where a = TV "a"
        ta = TVar a