{-# LANGUAGE Strict #-}
module Type where

import Data.List

newtype TVar = TV String
  deriving (Eq, Ord)

instance Show TVar where
  show (TV s) = s

data Type
  = TVar !TVar
  | TypeIdent !String
  | TypeFunc Type Type
  | TypeApply Type [Type]
  | Forall [TVar] Type
  deriving (Eq, Ord)

instance Show Type where
  show (TVar (TV n)) = n
  show (TypeIdent s) = s
  show (TypeFunc l r) = "(" ++ show l ++ " -> " ++ show r ++ ")"
  show (TypeApply t args) = "(" ++ show t ++ foldl (\acc a -> acc ++ " " ++ show a) "" args ++ ")"
  show (Forall targs t) = "∀(" ++ intercalate "," (map show targs) ++ ") => " ++ show t

infixr `TypeFunc`

typeInt :: Type
typeInt  = TypeIdent "Int"
typeFloat = TypeIdent "Float"
typeBool :: Type
typeBool = TypeIdent "Bool"

typeAny = TypeIdent "Any"
typeString = TypeIdent "String"
typeUnit = TypeIdent "Unit"
typeArray t = TypeApply (TypeIdent "Array") [t]
typeArrayInt = typeArray typeInt

schemaAny = Forall [] typeAny

isAny (TypeIdent "Any") = True
isAny _ = False