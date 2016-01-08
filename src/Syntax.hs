module Syntax where

data NList a = Nil | NCons { hd :: a, tl :: NList a }

data Redir = Redir String String deriving (Show)

data Stmt = Seq [Stmt]
           | Fun String [String] Stmt
           | Assign String AExpr
           | Cmd String [CmdArg] -- [Redir]
             deriving (Show)

data CmdArg = StrArg String | Arg String deriving (Show)

data AExpr = Ident String
            | IntConst Integer
            | StrLit String
            | NList [AExpr]
            | Neg AExpr
            | ABinary ABinOp AExpr AExpr
              deriving (Show)

data ABinOp = Add
             | Subtract
             | Multiply
             | Divide
               deriving (Show)