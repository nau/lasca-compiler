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
  show (Meta pos tpe) = "Meta{pos=" ++ show pos ++ ", tpe=" ++ show tpe ++ "}"
  
instance Show Position where
  show NoPosition = "<unknown>"
  show Position{sourceLine = sl, sourceColumn = sc} = show sl ++ ":" ++ show sc

emptyMeta = Meta { pos = NoPosition, symbolType = schemaAny }

withMetaPos line col = emptyMeta { pos = Position {sourceLine = line, sourceColumn = col} }

data Expr
  = Literal Meta Lit
  | Ident Meta Name
  | Val Name Expr
  | Apply Meta Expr [Expr]
  | Lam String Expr
  | Select Meta Expr Expr
  | Match Expr [Case]
  | BoxFunc Name [Arg]   -- LLVM codegen only
  | Function Name Type [Arg] Expr
  | Extern Name Type [Arg]
  | If Expr Expr Expr
  | Let Name Expr Expr
  | Array [Expr]
  | Data Name [DataConst]
  deriving (Ord)

instance Eq Expr where
  (Literal _ l) == (Literal _ r) = l == r
  (Ident _ l) == (Ident _ r) = l == r
  (Val nl l) == (Val nr r) = nl == nr && l == r
  (Apply _ nl l) == (Apply _ nr r) = nl == nr && l == r
  (Lam nl l) == (Lam nr r) = nl == nr && l == r
  (Select _ nl l) == (Select _ nr r) = nl == nr && l == r
  (Match nl l) == (Match nr r) = nl == nr && l == r
  (BoxFunc nl l) == (BoxFunc nr r) = nl == nr && l == r
  (Function nl _ al l) == (Function nr _ ar r) = nl == nr && al == ar && l == r
  (Extern nl _ l) == (Extern nr _ r) = nl == nr && l == r
  (If nl al l) == (If nr ar r) = nl == nr && al == ar && l == r
  (Let nl al l) == (Let nr ar r) = nl == nr && al == ar && l == r
  (Array l) == (Array r) = l == r
  (Data nl l) == (Data nr r) = nl == nr && l == r

instance Show Expr where
  show (Literal _ l) = show l
  show (Ident meta n) = n
  show (Val n e) = printf "val %s = %s\n" n (show e)
  show (Apply _ f e) = printf "%s(%s)" (show f) (intercalate "," $ map show e)
  show (Lam a e) = printf "{ %s -> %s}\n" (show a) (show e)
  show (Select _ e f) = printf "%s.%s" (show e) (show f)
  show (Match e cs) = printf "match %s {\n%s}\n" (show e) (show cs)
  show (BoxFunc f args) = printf "BoxFunc %s($args)" (show f) (intercalate "," $ map show args)
  show (Function f t args b) = printf "def %s(%s): %s = %s\n" (show f) (intercalate "," $ map show args) (show t) (show b)
  show (Extern f t args) = printf "def %s(%s): %s\n" (show f) (intercalate "," $ map show args) (show t)
  show (If c t f) = printf "if %s then {\n%s \n} else {\n%s\n}" (show c) (show t) (show f)
  show (Let n e b) = printf "%s = %s;\n%s" n (show e) (show b)
  show (Array es) = printf "[%s]" (intercalate "," $ map show es)
  show (Data n cs) = printf "data %s = %s\n" n (intercalate "\n| " $ map show cs)



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
  _globalFunctions :: Set.Set String,
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
      names (Val name _) = globalVals %= Set.insert name
      names (Function name _ _ _) = globalFunctions %= Set.insert name
      names (Data name consts) = do
        id <- gets typeId
        let dataDef = DataDef id name consts
        let (funcs, vals) = foldl (\(funcs, vals) (DataConst n args) ->
                              if null args
                              then (funcs, n : vals)
                              else (n : funcs, vals)) ([], []) consts
        modify (\s -> s { dataDefs =  dataDef : dataDefs s, typeId = id + 1 })
        globalVals %= Set.union (Set.fromList vals)
        globalFunctions %= Set.union (Set.fromList funcs)
      names (Extern name _ _) = globalFunctions %= Set.insert name
      names expr = error $ "Wat? Expected toplevel expression, but got " ++ show expr


globalFunctions :: Lens.Lens' Ctx (Set.Set String)
globalFunctions = Lens.lens _globalFunctions (\c e -> c { _globalFunctions = e } )

globalVals :: Lens.Lens' Ctx (Set.Set String)
globalVals = Lens.lens _globalVals (\c e -> c { _globalVals = e } )

emptyCtx = Context {
  _globalFunctions = Set.empty,
  _globalVals = Set.empty,
  dataDefs = [],
  typeId = 1000
}