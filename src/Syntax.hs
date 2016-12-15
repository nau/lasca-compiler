module Syntax where

data Expr =   Ident String
            | IntLit Integer
            | App Expr Expr
            | Extern String [String]
            | Def String [String] Expr
              deriving (Show)