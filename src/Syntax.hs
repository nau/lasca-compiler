module Syntax where

data Expr =   Ident String
            | IntLit Integer
            | App Expr Expr
            | Extern String [String]
            | Def String [String] Expr
              deriving (Show)

data Call = Call String [Expr]

flattenApp :: Expr -> Call
flattenApp app@(App _ _) = Call fn ars
    where
        ((Ident fn) : obj : ars) = args app []
        args :: Expr -> [Expr] -> [Expr]
        args (App l r) xs =  args l (r : xs)
        args e xs =  e : xs