module Syntax where

import Data.Text
import Type

type Name = String

data LascaOpts = LascaOpts
  { lascaFiles :: [String]
  , mode :: String
  , exec :: Bool
  , printLLVMAsm :: Bool
  , optimization :: Int
  }

data Expr
  = Literal Lit
  | Var String
  | Apply Expr [Expr]
  | Lam String Expr
  | Fix Expr        -- typechecker only
  | BoxFunc Name [Arg]   -- LLVM codegen only
  | Function Name Type [Arg] Expr
  | Extern Name Type [Arg]
  | If Expr Expr Expr
  | Let Name Expr Expr
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
