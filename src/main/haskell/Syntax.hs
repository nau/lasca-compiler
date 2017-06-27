{-# LANGUAGE Strict #-}
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
import           Type

type Name = String

data LascaOpts = LascaOpts
  { lascaFiles   :: [String]
  , mode         :: String
  , exec         :: Bool
  , verboseMode  :: Bool
  , printLLVMAsm :: Bool
  , printAst     :: Bool
  , printTypes   :: Bool
  , optimization :: Int
  }

data Position = NoPosition | Position {sourceLine :: Word, sourceColumn :: Word} deriving (Eq, Ord)

data Meta = Meta {
  pos :: Position,
  symbolType :: Scheme
} deriving (Eq, Ord)

instance Show Meta where
--  show (Meta pos tpe) = "Meta{pos=" ++ show pos ++ ", tpe=" ++ show tpe ++ "}"
  show (Meta _ tpe) = show tpe

instance Show Position where
  show NoPosition = "<unknown>"
  show Position{sourceLine = sl, sourceColumn = sc} = show sl ++ ":" ++ show sc

emptyMeta = Meta { pos = NoPosition, symbolType = schemaAny }

withMetaPos line col = emptyMeta { pos = Position {sourceLine = line, sourceColumn = col} }

data Expr
  = Literal Meta Lit
  | Ident Meta Name
  | Val Meta Name Expr
  | Apply Meta Expr [Expr]
  | Lam Meta Arg Expr
  | Select Meta Expr Expr
  | Match Meta Expr [Case]
  | BoxFunc Name [Arg]   -- LLVM codegen only
  | Function Meta Name Type [Arg] Expr
  | Extern Name Type [Arg]
  | If Meta Expr Expr Expr
  | Let Meta Name Expr Expr
  | Array Meta [Expr]
  | Data Meta Name [DataConst]
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
            Data meta name constrs -> (meta, \ m -> Data m name constrs)
            _ -> error $ "Should not happen :) " ++ show expr

symbolTypeLens :: Lens.Lens' Meta Scheme
symbolTypeLens = Lens.lens symbolType (\meta st -> meta { symbolType = st })

getExprType expr = expr^.metaLens.symbolTypeLens

instance Eq Expr where
  (Literal _ l) == (Literal _ r) = l == r
  (Ident _ l) == (Ident _ r) = l == r
  (Val _ nl l) == (Val _ nr r) = nl == nr && l == r
  (Apply _ nl l) == (Apply _ nr r) = nl == nr && l == r
  (Lam _ nl l) == (Lam _ nr r) = nl == nr && l == r
  (Select _ nl l) == (Select _ nr r) = nl == nr && l == r
  (Match _ nl l) == (Match _ nr r) = nl == nr && l == r
  (BoxFunc nl l) == (BoxFunc nr r) = nl == nr && l == r
  (Function _ nl _ al l) == (Function _ nr _ ar r) = nl == nr && al == ar && l == r
  (Extern nl _ l) == (Extern nr _ r) = nl == nr && l == r
  (If _ nl al l) == (If _ nr ar r) = nl == nr && al == ar && l == r
  (Let _ nl al l) == (Let _ nr ar r) = nl == nr && al == ar && l == r
  (Array _ l) == (Array _ r) = l == r
  (Data _ nl l) == (Data _ nr r) = nl == nr && l == r

{-
instance Show Expr where
  show (Literal _ l) = show l
  show (Ident meta n) = printf "%s: %s" n (show meta)
  show (Val meta n e) = printf "val %s: %s = %s\n" n (show meta) (show e)
  show (Apply meta f e) = printf "%s(%s): %s" (show f) (intercalate "," $ map show e) (show meta)
  show (Lam _ a e) = printf "{ %s -> %s }\n" (show a) (show e)
  show (Select _ e f) = printf "%s.%s" (show e) (show f)
  show (Match _ e cs) = printf "match %s {\n%s}\n" (show e) (show cs)
  show (BoxFunc f args) = printf "BoxFunc %s($args)" (show f) (intercalate "," $ map show args)
  show (Function meta f t args b) = printf "def %s(%s): %s = %s\n" f (intercalate "," $ map show args) (show meta) (show b)
  show (Extern f t args) = printf "def %s(%s): %s\n" (show f) (intercalate "," $ map show args) (show t)
  show (If meta c t f) = printf "if %s then {\n%s \n} else {\n%s\n}: %s" (show c) (show t) (show f) (show meta)
  show (Let meta n e b) = printf "%s = %s;\n%s: %s" n (show e) (show b) (show meta)
  show (Array _ es) = printf "[%s]" (intercalate "," $ map show es)
  show (Data _ n cs) = printf "data %s = %s\n" n (intercalate "\n| " $ map show cs)
-}



data Case = Case Pattern Expr deriving (Eq, Ord, Show)
data Pattern
  = LitPattern Lit
  | ConstrPattern String [Pattern]
  | VarPattern String
  | WildcardPattern
  deriving (Eq, Ord, Show)

data DataConst = DataConst Name [Arg] deriving (Eq, Ord, Show)

data DataDef = DataDef Int String [DataConst]
  deriving (Show, Eq)

data Ctx = Context {
  _globalFunctions :: Map.Map String Scheme,
  _globalVals :: Set.Set String,
  dataDefs :: [DataDef],
  typeId :: Int -- TODO remove this. Needed for type id generation. Move to ModuleState?
} deriving (Show, Eq)

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

createGlobalContext :: [Expr] -> Ctx
createGlobalContext exprs = execState (loop exprs) emptyCtx
  where
      loop [] = return ()
      loop (e:exprs) = do
        names e
        loop exprs

      names :: Expr -> State Ctx ()
      names (Val _ name _) = globalVals %= Set.insert name
      names (Function meta name _ _ _) = globalFunctions %= Map.insert name (symbolType meta)
      names (Data _ name consts) = do
        id <- gets typeId
        let dataDef = DataDef id name consts
        let (funcs, vals) = foldl (\(funcs, vals) (DataConst n args) ->
                              if null args
                              then (funcs, n : vals)
                              else let dataTypeIdent = TypeIdent name
                                       tpe = Forall [] (foldr (\(Arg _ tpe) acc -> tpe `TypeFunc` acc) dataTypeIdent args)
                                   in ((n, tpe) : funcs, vals)) ([], []) consts
        modify (\s -> s { dataDefs =  dataDef : dataDefs s, typeId = id + 1 })
        globalVals %= Set.union (Set.fromList vals)
        globalFunctions %= Map.union (Map.fromList funcs)
      names (Extern name tpe args) = do
        let funcType = Forall [] (foldr (\(Arg _ t) acc -> TypeFunc t acc) tpe args)
        globalFunctions %= Map.insert name funcType
      names expr = error $ "Wat? Expected toplevel expression, but got " ++ show expr


globalFunctions :: Lens.Lens' Ctx (Map.Map String Scheme)
globalFunctions = Lens.lens _globalFunctions (\c e -> c { _globalFunctions = e } )

globalVals :: Lens.Lens' Ctx (Set.Set String)
globalVals = Lens.lens _globalVals (\c e -> c { _globalVals = e } )

emptyCtx = Context {
  _globalFunctions = Map.empty,
  _globalVals = Set.empty,
  dataDefs = [],
  typeId = 1000
}