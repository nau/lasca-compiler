module Syntax where

import Data.Text
import Type

type Name = String

data Expr
  = Literal Lit
  | Var String
  | Apply Expr [Expr]
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
