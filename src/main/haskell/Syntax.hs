--------------------------------------------------------------------
-- |
-- Module    :  Syntax
-- Copyright :  (c) Stephen Diehl 2013
-- License   :  MIT
-- Maintainer:  stephen.m.diehl@gmail.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Syntax where

type Name = String

data Expr
  = Literal Lit
  | Var String
  | Apply Name [Expr]
  | Function Name [Name] Expr
  | Extern Name Type [Arg]
  | BinaryOp Name Expr Expr
  | UnaryOp Name Expr
  | If Expr Expr Expr
  | Let Name Expr Expr
  deriving (Eq, Ord, Show)

data Type = Type Name | UnitType | AnyType deriving (Eq, Ord, Show)
data Lit = IntLit Int | FloatLit Double | BoolLit Bool deriving (Eq, Ord, Show)
data Arg = Arg Name Type deriving (Eq, Ord, Show)

boolType = Type "Bool"
intType = Type "Int"
float64Type = Type "Float64"