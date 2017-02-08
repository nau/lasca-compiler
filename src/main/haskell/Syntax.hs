module Syntax where

import Data.Text
import Type

type Name = String

data Expr
  = Literal Lit
  | Var String
  | Apply Expr [Expr]
  | Function Name SType [Arg] Expr
  | Extern Name SType [Arg]
  | If Expr Expr Expr
  | Let Name Expr Expr
  deriving (Eq, Ord, Show)

data SType = SType Name | UnitType | AnyType deriving (Eq, Ord, Show)
data Lit = IntLit Int
  | FloatLit Double
  | BoolLit Bool
  | StringLit String
  deriving (Eq, Ord, Show)

data Arg = Arg Name SType deriving (Eq, Ord, Show)
