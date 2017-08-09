{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
module Syntax where

--import           Data.Text
import           Data.List
import           Text.Printf
import qualified Text.Megaparsec as Megaparsec
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Except
import Control.Applicative
import qualified Control.Lens as Lens
import Control.Lens.Operators
import Control.Lens.TH
import           Type

--type Name = String

data LascaOpts = LascaOpts
    { lascaFiles   :: [String]
    , mode         :: String
    , exec         :: Bool
    , verboseMode  :: Bool
    , printLLVMAsm :: Bool
    , printAst     :: Bool
    , printTypes   :: Bool
    , optimization :: Int
    } deriving (Show, Eq)

data Position = NoPosition | Position {sourceLine :: Word, sourceColumn :: Word} deriving (Eq, Ord)

data Meta = Meta {
    pos :: Position,
    _exprType :: Type,
    _isExternal :: Bool
} deriving (Eq, Ord)
makeLenses ''Meta

instance Show Meta where
--  show (Meta pos tpe) = "Meta{pos=" ++ show pos ++ ", tpe=" ++ show tpe ++ "}"
    show meta = show (_exprType meta)

instance Show Position where
    show NoPosition = "<unknown>"
    show Position{sourceLine = sl, sourceColumn = sc} = show sl ++ ":" ++ show sc

emptyMeta = Meta { pos = NoPosition, _exprType = typeAny, _isExternal = False }

withMetaPos line col = emptyMeta { pos = Position {sourceLine = line, sourceColumn = col} }

metaWithType s = emptyMeta { _exprType = s }

data Expr
    = EmptyExpr
    | Literal Meta Lit
    | Ident Meta Name
    | Val Meta Name Expr
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
              Val meta name expr -> (meta, \ m -> Val m name expr)
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
              _ -> error $ "Should not happen :) " ++ show expr

exprPosition expr = pos (expr ^. metaLens)

getExprType expr = expr^.metaLens.exprType

typeOf = fromSchema . getExprType

fromSchema (Forall _ t) = t
fromSchema t = t

withScheme f (Forall tv t) = Forall tv (f t)

instance Eq Expr where
    (Literal _ l) == (Literal _ r) = l == r
    (Ident _ l) == (Ident _ r) = l == r
    (Val _ nl l) == (Val _ nr r) = nl == nr && l == r
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

{-
instance Show Expr where
  show (Literal _ l) = show l
  show (Ident meta n) = printf "%s: %s" n (show meta)
  show (Val meta n e) = printf "val %s: %s = %s\n" n (show meta) (show e)
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
        Val meta n e -> printf "val %s: %s = %s\n" (show n) (show meta) (printExprWithType e)
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
        EmptyExpr -> ""


data Case = Case Pattern Expr deriving (Eq, Ord, Show)
data Pattern
    = LitPattern Lit
    | ConstrPattern Name [Pattern]
    | VarPattern String
    | WildcardPattern
    deriving (Eq, Ord, Show)

data DataConst = DataConst Name [Arg] deriving (Eq, Ord, Show)

data DataDef = DataDef Int Name [DataConst]
    deriving (Show, Eq)

emptyCtx opts = Context {
    _lascaOpts = opts,
    _globalFunctions = Map.empty,
    _globalVals = Set.empty,
    dataDefs = [],
    dataDefsNames = Set.empty,
    dataDefsFields = Map.empty,
    typeId = 1000
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
    _globalFunctions :: Map.Map Name Expr,
    _globalVals :: Set.Set Name,
    dataDefs :: [DataDef],
    dataDefsNames :: Set.Set Name,
    dataDefsFields :: Map.Map Name (Map.Map Name (Arg, Int)),
    typeId :: Int -- TODO remove this. Needed for type id generation. Move to ModuleState?
} deriving (Show, Eq)
makeLenses ''Ctx

createGlobalContext :: LascaOpts -> [Expr] -> Ctx
createGlobalContext opts exprs = execState (loop exprs) (emptyCtx opts)
  where
    loop [] = return ()
    loop (e:exprs) = do
        names e
        loop exprs

    names :: Expr -> State Ctx ()
    names (Val _ name _) = globalVals %= Set.insert name
    names fun@(Function meta name tpe args _) = globalFunctions %= Map.insert name fun
    names (Data _ name tvars consts) = do
        id <- gets typeId
        let dataDef = DataDef id name consts
        let (funcs, vals) = foldl (\(funcs, vals) (DataConst n args) ->
                              if null args
                              then (funcs, n : vals)
                              else let dataTypeIdent = TypeIdent name
                                       tpe = (foldr (\(Arg _ tpe) acc -> tpe `TypeFunc` acc) dataTypeIdent args) -- FIXME forall
                                       meta = metaWithType tpe
                                       funDef = Function meta n dataTypeIdent args EmptyExpr
                                   in ((n, funDef) : funcs, vals)) ([], []) consts
        -- FIXME Merge with above, create State?
        let argsWithIds = foldl' (\acc (DataConst _ args) ->
                                          fst $ foldl' (\(acc, idx) arg@(Arg n t)  ->
                                              if n `Map.member` acc
                                              then error $ printf "Field %s already defined in data %s" (show n) (show name)
                                              else (Map.insert n (arg, idx) acc, idx + 1) -- field name -> (S.Arg, field index in constructor) mapping
                                          ) (acc, 0) args
                                      ) Map.empty consts

        modify (\s -> s {
            dataDefs =  dataDef : dataDefs s,
            dataDefsNames = Set.insert name (dataDefsNames s),
            dataDefsFields = Map.insert name argsWithIds (dataDefsFields s),
            typeId = id + 1
        })
        globalVals %= Set.union (Set.fromList vals)
        globalFunctions %= Map.union (Map.fromList funcs)
    names expr = error $ "Wat? Expected toplevel expression, but got " ++ show expr

emptyLascaOpts = LascaOpts {
    lascaFiles  = [],
    mode = "static",
    exec = False,
    verboseMode = False,
    printLLVMAsm = False,
    printAst = False,
    printTypes = False,
    optimization = 0
}
