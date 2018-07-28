{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
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

emptyMeta = Meta { pos = NoPosition, _exprType = TypeAny, _isExternal = False, _annots = [] }

withMetaPos line col = emptyMeta { pos = Position {sourceLine = line, sourceColumn = col} }

emptyMetaWithType s = emptyMeta { _exprType = s }

withType meta t = meta { _exprType = t }

setType expr t = Lens.set (metaLens . exprType) t expr

metaType t = withType emptyMeta t

data Expr
    = EmptyExpr
    | Literal Meta Lit
    | Ident Meta Name
    | Apply Meta Expr [Expr]
    | Lam Meta Arg Expr
    | Select Meta Expr Expr
    | Match Meta Expr [Case]
    | Closure Meta Name [Arg]   -- LLVM codegen only
    | If Meta Expr Expr Expr
    | Let Bool Meta Name Type Expr Expr -- True for recursive
    | Array Meta [Expr]
    | Data Meta Name [TVar] [DataConst]
    | Module Meta Name
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
              If meta cond tr fl -> (meta, \ m -> If m cond tr fl)
              Let r meta name t expr body -> (meta, \ m -> Let r m name t expr body)
              Array meta exprs -> (meta, \ m -> Array m exprs)
              Data meta name tvars constrs -> (meta, \ m -> Data m name tvars constrs)
              Closure meta name args -> (meta, \m -> Closure m name args)
              Module meta name -> (meta, \m -> Module m name)
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
    (Closure _ nl l) == (Closure _ nr r) = nl == nr && l == r
    (Let rl metal nl _ al l) == (Let rr metar nr _ ar r) = rl == rr && nl == nr && al == ar && l == r && (metal^.isExternal) == (metar^.isExternal)
    (If _ nl al l) == (If _ nr ar r) = nl == nr && al == ar && l == r
    (Array _ l) == (Array _ r) = l == r
    (Data _ nl ltvars l) == (Data _ nr rtvars r) = nl == nr && ltvars == rtvars && l == r
    Module _ ln == Module _ rn = ln == rn
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
  show (Closure meta f args) = printf "Closure %s($args)" (show f) (intercalate "," $ map show args)
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

uncurryLambda :: Expr -> ([Arg], Expr) -- arguments, and body of innermost lambda
uncurryLambda expr = go expr ([], expr) where
  go (Lam _ name e) result = let (args, body) = go e result in (name : args, body)
  go e (args, _) = (args, e)

curryLambda :: Meta -> [Arg] -> Expr -> (Expr, Type)
curryLambda meta args expr = foldr (\arg@(Arg n t) (body, tpe) ->
     let lambdaType = TypeFunc t tpe in
     (Lam (meta `withType` lambdaType) arg body, lambdaType)) (expr, TypeAny) args


instance DebugPrint Expr where
    printExprWithType expr = case expr of
        Literal _ l -> show l
        Ident meta n -> printf "%s: %s" (show n) (show meta)
        Apply meta f e -> printf "%s(%s): %s" (printExprWithType f) (intercalate "," $ map printExprWithType e) (show meta)
        Lam _ a e -> printf "{ %s -> %s }\n" (printExprWithType a) (printExprWithType e)
        Select _ e f -> printf "%s.%s" (printExprWithType e) (printExprWithType f)
        Match _ e cs -> printf "match %s {\n%s}\n" (printExprWithType e) (show cs)
        Closure meta f args -> printf "Closure %s(%s)" (show f) (intercalate "," $ map show args)
        Let False meta n t e b -> printf "%s = %s;\n%s: %s" (show n) (printExprWithType e) (printExprWithType b) (show meta)
        Let True meta f t lam _ | _isExternal meta -> do
            let (args, _) = uncurryLambda lam
            printf "extern def %s(%s): %s\n" (show f) (intercalate "," $ map printExprWithType args) (show t)
        Let True meta f t lam next -> do
            let (args, b) = uncurryLambda lam
            printf "--  %s : %s\ndef %s(%s): %s = %s;\n%s" (show f) (show meta) (show f) (intercalate "," $ map printExprWithType args) (show t) (printExprWithType b) (printExprWithType next)
        If meta c t f -> printf "if %s then {\n%s \n} else {\n%s\n}: %s" (printExprWithType c) (printExprWithType t) (printExprWithType f) (show meta)
        Array _ es -> printf "[%s]" (intercalate "," $ map printExprWithType es)
        Data _ n tvars cs -> printf "data %s %s = %s\n" (show n) (show tvars) (intercalate "\n| " $ map show cs)
        Module meta name -> printf "module %s" (show name)
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

emptyCtx opts = Context {
    _lascaOpts = opts,
    _moduleName = Name defaultModuleName,
    _globalFunctions = Map.empty,
    _globalVals = Map.empty,
    _dataDefs = [],
    _constructorArgs = Map.empty,
    _constructorTags = Map.empty,
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
    _moduleName :: Name,
    _globalFunctions :: Map Name Expr,
    _globalVals :: Map Name Expr,
    _dataDefs :: [Expr],
    _constructorArgs :: Map Name [Arg], -- data -> constructor -> fields
    _constructorTags :: Map Name (Map Name Int), -- data -> constructor -> tag
    _dataDefsNames :: Set Name,
    _dataDefsFields :: Map Name (Map Name (Arg, Int))
} deriving (Show, Eq)
makeLenses ''Ctx

isStaticMode ctx = mode (_lascaOpts ctx) == Static

builtinFunctions :: Map Name Type
builtinFunctions = Map.fromList [
    ("unary-", Forall [a] (ta ==> ta)),
    (":=", Forall [a] (TypeRef ta ==> ta ==> TypeRef ta)),
    ("+",  Forall [a] (ta ==> ta ==> ta)),
    ("-",  Forall [a] (ta ==> ta ==> ta)),
    ("*",  Forall [a] (ta ==> ta ==> ta)),
    ("/",  Forall [a] (ta ==> ta ==> ta)),
    ("==", Forall [a] (ta ==> ta ==> TypeBool)),
    ("!=", Forall [a] (ta ==> ta ==> TypeBool)),
    ("<",  Forall [a] (ta ==> ta ==> TypeBool)),
    ("<=", Forall [a] (ta ==> ta ==> TypeBool)),
    (">",  Forall [a] (ta ==> ta ==> TypeBool)),
    (">=", Forall [a] (ta ==> ta ==> TypeBool))
  ]
  where a = TV "a"
        ta = TVar a

{- Expr Builder DSL -}

class Literal a where
    lit :: a -> Lit

instance Literal Int where lit v = IntLit v
instance Literal Double where lit v = FloatLit v
instance Literal Bool where lit v = BoolLit v
instance Literal () where lit v = UnitLit
instance Literal String where lit v = StringLit v

infixr ==>
(==>) = TypeFunc
