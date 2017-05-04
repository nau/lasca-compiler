{-# LANGUAGE Strict #-}
module Syntax where

--import           Data.Text
import           Data.List
import           Text.Printf
import qualified Text.Megaparsec as Megaparsec
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
  | Ident Name
  | Val Name Expr
  | Apply Meta Expr [Expr]
  | Lam String Expr
  | Select Meta Expr Expr
  | Match Expr [Case]
  | Fix Expr        -- typechecker only
  | BoxFunc Name [Arg]   -- LLVM codegen only
  | Function Name Type [Arg] Expr
  | Extern Name Type [Arg]
  | If Expr Expr Expr
  | Let Name Expr Expr
  | Array [Expr]
  | Data Name [DataConst]
  deriving (Eq, Ord)

instance Show Expr where
  show (Literal _ l) = show l
  show (Ident n) = n
  show (Val n e) = printf "val %s = %s\n" n (show e)
  show (Apply _ f e) = printf "%s(%s)" (show f) (intercalate "," $ map show e)
  show (Lam a e) = printf "{ %s -> %s}\n" (show a) (show e)
  show (Select _ e f) = printf "%s.%s" (show e) (show f)
  show (Match e cs) = printf "match %s {\n%s}\n" (show e) (show cs)
  show (Fix e) = printf "Fix %s" (show e)
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
