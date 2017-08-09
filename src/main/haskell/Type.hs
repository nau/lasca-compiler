{-# LANGUAGE Strict #-}
module Type where

import Data.List
import Data.String
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS

data Name = Name String | NS Name Name deriving (Eq, Ord)

instance IsString Name where
    fromString = Name

instance Show Name where
    show n = case n of
        Name s -> s
        NS prefix n -> show prefix ++ "_" ++ show n

qualify pkg name = if pkg == defaultPackageQName then name else NS pkg name

qnameToString n = show n

qname = Name

nameToSBS :: Name -> SBS.ShortByteString
nameToSBS = fromString . show

nameToBS :: Name -> BS.ByteString
nameToBS = fromString . show

defaultPackageName = "Main"
defaultPackageQName = Name defaultPackageName

newtype TVar = TV String
  deriving (Eq, Ord)

instance Show TVar where
  show (TV s) = s

data Type
  = TVar TVar
  | TypeIdent Name
  | TypeFunc Type Type
  | TypeApply Type [Type]
  | Forall [TVar] Type
  deriving (Eq, Ord)

instance Show Type where
  show (TVar (TV n)) = n
  show (TypeIdent s) = show s
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

isAny (TypeIdent "Any") = True
isAny _ = False

typeToList tpe = reverse $ go tpe []
  where go (TypeFunc a b) acc = go b (a : acc)
        go (Forall tvars tpe) acc = go tpe acc
        go a acc              = a : acc