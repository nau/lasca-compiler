module Lasca.Type where

import Data.List
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Encoding
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.Text.Prettyprint.Doc

data Name = Name Text | NS Name Name deriving (Eq, Ord)

instance IsString Name where
    fromString = Name . T.pack

instance Show Name where
    show n = case n of
        Name s -> T.unpack s
        NS prefix n -> show prefix ++ "_" ++ show n

nameToText n = case n of
    Name n -> n
    NS prefix n -> T.append (nameToText prefix) (T.cons '_' (nameToText n))

qualify mod name = if mod == defaultModuleQName then name else NS mod name

qnameToString n = show n

qname = Name

textToSBS :: Text -> SBS.ShortByteString
textToSBS = SBS.toShort . Encoding.encodeUtf8

nameToSBS :: Name -> SBS.ShortByteString
nameToSBS = textToSBS . nameToText

nameToBS :: Name -> BS.ByteString
nameToBS = Encoding.encodeUtf8 . nameToText

nameToList (Name n) = [n]
nameToList (NS prefix n) = nameToList prefix ++ nameToList n

defaultModuleName = "Main"
defaultModuleQName = Name defaultModuleName

newtype TVar = TV Text
  deriving (Eq, Ord)

instance Show TVar where
  show (TV s) = T.unpack s

data Type
  = TVar TVar
  | TypeIdent Name
  | TypeFunc Type Type
  | TypeApply Type [Type]
  | Forall [TVar] Type
  deriving (Eq, Ord)

instance Show Type where
  show (TVar (TV n)) = T.unpack n
  show (TypeIdent s) = show s
  show (TypeFunc l r) = "(" ++ show l ++ " -> " ++ show r ++ ")"
  show (TypeApply t args) = "(" ++ show t ++ foldl (\acc a -> acc ++ " " ++ show a) "" args ++ ")"
  show (Forall targs t) = "∀(" ++ intercalate "," (map show targs) ++ ") => " ++ show t

instance Pretty Name where
    pretty n = case n of
        Name s -> pretty s
        NS prefix n -> pretty prefix <+> "_" <+> pretty n

instance Pretty TVar where
  pretty (TV s) = pretty s

instance Pretty Type where
    pretty t = case t of
        (TVar (TV n)) -> pretty n
        (TypeIdent s) -> pretty s
        (TypeFunc l r) -> parens $ pretty l <+> "->" <+> pretty r
        (TypeApply t args) -> parens $pretty t <+> foldl (\acc a -> acc <+> pretty a) "" args
        (Forall targs t) -> "∀" <> parens (hsep (punctuate comma (map pretty targs))) <+> "=>" <+> pretty t


typeName tpe = case tpe of
    TypeIdent n -> n
    TypeApply t _ -> typeName t
    Forall _ t -> typeName t
    _ -> error $ "Should not happen. Type name can't be " ++ show tpe

infixr `TypeFunc`

pattern TypeByte     = TypeIdent "Byte"
pattern TypeInt      = TypeIdent "Int"
pattern TypeInt16    = TypeIdent "Int16"
pattern TypeInt32    = TypeIdent "Int32"
pattern TypeFloat    = TypeIdent "Float"
pattern TypeBool     = TypeIdent "Bool"
pattern TypeAny      = TypeIdent "Any"
pattern TypeString   = TypeIdent "String"
pattern TypeUnit     = TypeIdent "Unit"
pattern TypeArray t  = TypeApply (TypeIdent "Array") [t]
pattern TypeArrayInt = TypeArray TypeInt
pattern TypeRef a    = TypeApply (TypeIdent "Var") [a]

isIntegralType (TypeIdent t) | t `elem` ["Byte", "Int", "Int16", "Int32"] = True
isIntegralType _ = False


isAny (TypeIdent "Any") = True
isAny _ = False

typeToList tpe = reverse $ go tpe []
  where go (TypeFunc a b) acc = go b (a : acc)
        go (Forall tvars tpe) acc = go tpe acc
        go a acc              = a : acc

funcTypeArity this@(TypeFunc a b) = (length $ typeToList this) - 1
funcTypeArity _ = 0