module Type where

import Data.List

newtype TVar = TV String
  deriving (Eq, Ord)

instance Show TVar where
  show (TV s) = s

data Type
  = TVar {-# UNPACK #-} !TVar
  | TCon {-# UNPACK #-} !String
  | TArr Type Type
  deriving (Eq, Ord)

instance Show Type where
  show (TVar (TV n)) = n
  show (TCon s) = s
  show (TArr l r) = "(" ++ show l ++ " -> " ++ show r ++ ")"


infixr `TArr`

data Scheme = Forall [TVar] Type
  deriving (Eq, Ord)

instance Show Scheme where
  show (Forall [] t) = show t
  show (Forall vars t) = "∀(" ++ intercalate "," typeVarNames ++ ") => " ++ show t
    where
      typeVarNames :: [String]
      typeVarNames = (map show vars)

typeInt :: Type
typeInt  = TCon "Int"

typeBool :: Type
typeBool = TCon "Bool"

typeAny = TCon "Any"
typeString = TCon "String"
typeUnit = TCon "Unit"

isAny (TCon "Any") = True
isAny _ = False