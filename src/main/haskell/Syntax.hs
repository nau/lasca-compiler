{-# LANGUAGE Strict #-}
module Syntax where

import           Data.Text
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

data Position = NoPosition | Position {sourceLine :: Word, sourceColumn :: Word} deriving (Eq, Ord, Show)

data Meta = Meta {
  pos :: Position,
  symbolType :: Scheme
} deriving (Eq, Ord, Show)

emptyMeta = Meta { pos = NoPosition, symbolType = schemaAny }

data Expr
  = Literal Lit Meta
  | Ident Name
  | Val Name Expr
  | Apply Meta Expr [Expr]
  | Lam String Expr
  | Select Meta Expr Expr
  | Fix Expr        -- typechecker only
  | BoxFunc Name [Arg]   -- LLVM codegen only
  | Function Name Type [Arg] Expr
  | Extern Name Type [Arg]
  | If Expr Expr Expr
  | Let Name Expr Expr
  | Array [Expr]
  | Data Name [DataConst]
  deriving (Eq, Ord, Show)

data DataConst = DataConst Name [Arg] deriving (Eq, Ord, Show)

data Lit = IntLit Int
  | FloatLit Double
  | BoolLit Bool
  | UnitLit
  | StringLit String
  deriving (Eq, Ord, Show)

data Arg = Arg Name Type deriving (Eq, Ord, Show)
