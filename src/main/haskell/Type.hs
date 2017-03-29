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
  deriving (Eq, Ord)

instance Show Type where
  show (TVar (TV n)) = n
  show (TypeIdent s) = s
  show (TypeFunc l r) = "(" ++ show l ++ " -> " ++ show r ++ ")"


infixr `TypeFunc`

data Scheme = Forall [TVar] Type
  deriving (Eq, Ord)

instance Show Scheme where
  show (Forall [] t) = show t
  show (Forall vars t) = "∀(" ++ intercalate "," typeVarNames ++ ") => " ++ show t
    where
      typeVarNames :: [String]
      typeVarNames = (map show vars)

typeInt :: Type
typeInt  = TypeIdent "Int"

typeBool :: Type
typeBool = TypeIdent "Bool"

typeAny = TypeIdent "Any"
typeString = TypeIdent "String"
typeUnit = TypeIdent "Unit"
typeArrayInt = TypeFunc (TypeIdent "Array") typeInt

isAny (TypeIdent "Any") = True
isAny _ = False