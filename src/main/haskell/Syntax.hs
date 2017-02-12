module Syntax where

import Data.Text
import Type

type Name = String

data LascaOpts = LascaOpts
  { lascaFiles :: [String]
  , mode :: String
  , exec :: Bool
  , printLLVMAsm :: Bool
  }

data Expr
  = Literal Lit
  | Var String
  | Apply Expr [Expr]
  | Lam String Expr -- typechecker only
  | Fix Expr        -- typechecker only
  | Function Name Type [Arg] Expr
  | Extern Name Type [Arg]
  | If Expr Expr Expr
  | Let Name Expr Expr
  deriving (Eq, Ord, Show)

data Lit = IntLit Int
  | FloatLit Double
  | BoolLit Bool
  | StringLit String
  deriving (Eq, Ord, Show)

data Arg = Arg Name Type deriving (Eq, Ord, Show)
