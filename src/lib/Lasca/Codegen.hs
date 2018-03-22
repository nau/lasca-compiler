{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lasca.Codegen where

import Data.Word
import Data.String
import Data.List
import Data.Function
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
-- import qualified Data.Text as Text
import qualified Data.ByteString as ByteString
-- import qualified Data.Text.Encoding as Encoding
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text.Encoding as Encoding
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS

import Control.Monad.State
import Control.Applicative
import Control.Lens

import LLVM.AST
import LLVM.AST.Global
import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as T

import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FunctionAttribute as FA
import qualified LLVM.AST.IntegerPredicate as IPred
import qualified LLVM.AST.Linkage as Linkage

import Data.Digest.Murmur32

import qualified Debug.Trace as Debug

import qualified Lasca.Syntax as S
import qualified Lasca.Type as LT


-------------------------------------------------------------------------------
-- Module Level
-------------------------------------------------------------------------------

newtype LLVM a = LLVM { unLLVM :: State ModuleState a }
  deriving (Functor, Applicative, Monad, MonadState ModuleState )

data ModuleState = ModuleState {
    _llvmModule :: AST.Module,
    _globalValsInit :: [(LT.Name, S.Expr)],
    functions :: Map LT.Name Int,
    structs :: Map Int Int
} deriving (Show)


-- makeLenses ''ModuleState

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM modl s = result where
    state = execState (unLLVM s) (initModuleState modl)
    result = _llvmModule state

initModuleState modl = ModuleState {
    _llvmModule = modl,
    _globalValsInit = [],
    functions = Map.empty,
    structs = Map.empty
}

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = l, moduleSourceFileName = l }
  where l = fromString label

addDefn :: Definition -> LLVM ()
addDefn d = do
    modl <- gets _llvmModule
    let defs = moduleDefinitions modl
    if d `elem` defs
    then modify id
    else modify $ \s -> s { _llvmModule = (_llvmModule s) { moduleDefinitions = defs ++ [d] } }

defineGlobalVar name tpe body isConst = addDefn $
    AST.GlobalDefinition $ AST.globalVariableDefaults {
      LLVM.AST.Global.name        = AST.Name name
    , LLVM.AST.Global.isConstant  = isConst
    , LLVM.AST.Global.type' = tpe
    , LLVM.AST.Global.initializer = body
    }

defineGlobal name tpe body = defineGlobalVar name tpe body False

defineConst name tpe body = defineGlobalVar name tpe (Just body) True

defineFunction retty label link argtys blocks vararg funcAttrs = addDefn $
    GlobalDefinition $ functionDefaults {
      name        = Name label
    , linkage     = link
    , LLVM.AST.Global.functionAttributes = map Left funcAttrs
    , parameters  = ([Parameter ty (Name nm) [] | (nm, ty) <- argtys], vararg)
    , returnType  = retty
    , basicBlocks = blocks
    }

--define ::  Type -> SBS.ShortByteString -> [(SBS.ShortByteString, Type)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = defineFunction retty label Linkage.External argtys body False []

--external ::  Type -> SBS.ShortByteString -> [(SBS.ShortByteString, Type)] -> Bool -> [A.GroupID] -> LLVM ()
external retty label argtys vararg funcAttrs = defineFunction retty label Linkage.External argtys [] vararg funcAttrs

externalConst tpe nm = addDefn $ AST.GlobalDefinition $ globalVariableDefaults {
    LLVM.AST.Global.linkage = Linkage.External,
    LLVM.AST.Global.name = Name nm,
    LLVM.AST.Global.isConstant = True,
    LLVM.AST.Global.type' = tpe
}

---------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

intType :: Type
intType = T.i64

boolType :: Type
boolType = IntegerType 1

ptrType = T.ptr T.i8

ptrSize :: Int
ptrSize = 8 -- 64 bit architecture, TODO this is hardcode

-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

type Names = Map BS.ByteString Int

uniqueName :: BS.ByteString -> Names -> (BS.ByteString, Names)
uniqueName nm ns =
    case Map.lookup nm ns of
        Nothing -> (nm,  Map.insert nm 1 ns)
        Just ix -> (nm `mappend` (fromString $ show ix), Map.insert nm (ix+1) ns)

-------------------------------------------------------------------------------
-- Codegen State
-------------------------------------------------------------------------------

type SymbolTable = [(LT.Name, Operand)]

data CodegenState
    = CodegenState {
      currentBlock :: Name                     -- Name of the active block to append to
    , blocks       :: Map Name BlockState  -- Blocks for function
    , symtab       :: SymbolTable              -- Function scope symbol table
    , blockCount   :: Int                      -- Count of basic blocks
    , count        :: Word                     -- Count of unnamed instructions
    , names        :: Names                    -- Name Supply
    , moduleState  :: ModuleState
    , generatedStrings :: [String]
    } deriving Show

data BlockState
    = BlockState {
      idx   :: Int                            -- Block index
    , stack :: [Named Instruction]            -- Stack of instructions
    , term  :: Maybe (Named Terminator)       -- Block terminator
    } deriving Show

-------------------------------------------------------------------------------
-- Codegen Operations
-------------------------------------------------------------------------------

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, BlockState _ s t) = BasicBlock l s (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ show l

entryBlockName :: BS.ByteString
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: ModuleState -> CodegenState
emptyCodegen ms = CodegenState {
    currentBlock = Name (SBS.toShort entryBlockName),
    blocks = Map.empty,
    symtab = [],
    blockCount = 1,
    count = 0,
    names = Map.empty,
    moduleState = ms,
    generatedStrings = []
}

execCodegen :: [(LT.Name, Operand)] -> ModuleState -> Codegen a -> CodegenState
execCodegen vars modState m = execState transformations initialState
  where
    transformations = runCodegen m
    initialState = (emptyCodegen modState) { symtab = vars }

fresh :: Codegen Word
fresh = do
    i <- gets count
    modify $ \s -> s { count = 1 + i }
    return $ i + 1

current :: Codegen BlockState
current = do
    c <- gets currentBlock
    blks <- gets blocks
    case Map.lookup c blks of
        Just x -> return x
        Nothing -> error $ "No such block: " ++ show c

instr :: Instruction -> Codegen Operand
instr ins = instrTyped ptrType ins
{-# INLINE instr #-}

instrTyped tpe ins = do
    n <- fresh
    let ref = UnName n
    blk <- current
    let i = stack blk
    modifyBlock (blk { stack = i ++ [ref := ins] } )
    return $ LocalReference tpe ref
{-# INLINE instrTyped #-}

instrDo ins = do
    blk <- current
    let i = stack blk
    modifyBlock (blk { stack = i ++ [Do $ ins]} )
    return ()
{-# INLINE instrDo #-}

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
    blk <- current
    modifyBlock (blk { term = Just trm })
    return trm

-------------------------------------------------------------------------------
-- Block Stack
-------------------------------------------------------------------------------

entry :: Codegen Name
entry = gets currentBlock

addBlock :: BS.ByteString -> Codegen Name
addBlock bname = do
    bls <- gets blocks
    ix <- gets blockCount
    nms <- gets names
    let new = emptyBlock ix
        (qname', supply) = uniqueName bname nms
        qname = SBS.toShort qname'
    modify $ \s -> s { blocks = Map.insert (Name qname) new bls
                     , blockCount = ix + 1
                     , names = supply
                     }
    return (Name qname)

setBlock :: Name -> Codegen Name
setBlock bname = do
    modify $ \s -> s { currentBlock = bname }
    return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
    active <- gets currentBlock
    modify $ \s -> s { blocks = Map.insert active new (blocks s) }

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

assign :: LT.Name -> Operand -> Codegen ()
assign var x = do
    lcls <- gets symtab
    modify $ \s -> s { symtab = (var, x) : lcls }

getvar :: LT.Name -> Codegen Operand
getvar var = do
    syms <- gets symtab
    case lookup var syms of
        Just x  -> return x
        Nothing -> error $ "Local variable not in scope: " ++ show var

-------------------------------------------------------------------------------

-- References
local tpe name = LocalReference tpe (AST.Name name)

localPtr name = local ptrType name

global :: Type -> SBS.ShortByteString -> Operand
global tpe name = constOp (C.GlobalReference (T.ptr tpe) (AST.Name name))

constOp :: C.Constant -> Operand
constOp = ConstantOperand

constNull tpe = C.IntToPtr (constInt 0) (T.ptr tpe)
constNullPtr = constNull T.i8
constNullPtrOp = constOp constNullPtr

constInt :: Int -> C.Constant
constInt = constInt64
constInt32 i = C.Int 32 (toInteger i)
constInt64 i = C.Int 64 (toInteger i)

constIntOp :: Int -> Operand
constIntOp = constInt64Op
constInt32Op i = constOp (C.Int 32 (toInteger i))
constInt64Op i = constOp (C.Int 64 (toInteger i))

constFloat i = C.Float (F.Double i)
constFloatOp = constOp . constFloat

constByte b = constOp (C.Int 8 b)
constBool b = C.Int 1 (if b then 1 else 0)
constTrue = constOp (constBool True)
constFalse = constOp (constBool False)
constRef tpe name = let ptr = C.GlobalReference (T.ptr tpe) (AST.Name name) in C.BitCast ptr ptrType

fptoptr fp = do
    int <- bitcast fp T.i64
    inttoptr int

ptrtofp ptr = do
--    i64ptr <- bitcast ptr (T.ptr T.i64)
    int <- ptrtoint ptr T.i64
    bitcast int T.double

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))


bitcast op toTpe= instr (BitCast op toTpe [])
{-# INLINE bitcast #-}

ptrtoint op toTpe= instr (PtrToInt op toTpe [])
{-# INLINE ptrtoint #-}

inttoptr op = instr (IntToPtr op ptrType [])
{-# INLINE inttoptr #-}

-- Effects
add lhs rhs = instr $ Add False False lhs rhs []
sub lhs rhs = instr $ Sub False False lhs rhs []
mul lhs rhs = instr $ Mul False False lhs rhs []
div lhs rhs = instr $ SDiv True lhs rhs []
intCmp op lhs rhs = do
    bool <- instr $ ICmp op lhs rhs []
    instr $ ZExt bool intType []
intCmpBoxed op lhs rhs = do
    res <- intCmp op lhs rhs
    callFn (funcType ptrType [intType]) "boxBool" [res]
fadd lhs rhs = instr $ FAdd NoFastMathFlags lhs rhs []
fsub lhs rhs = instr $ FSub NoFastMathFlags lhs rhs []
fmul lhs rhs = instr $ FMul NoFastMathFlags lhs rhs []
fdiv lhs rhs = instr $ FDiv NoFastMathFlags lhs rhs []

call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []
{-# INLINE call #-}

callIns fn args = Call Nothing CC.C [] (Right fn) (toArgs args) [] []
callFnIns ftype name args = callIns (global ftype (fromString name)) args

callFn ftype name args = call (global ftype (fromString name)) args
{-# INLINE callFn #-}

alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []
{-# INLINE alloca #-}

allocaSize ty size = instr $ Alloca ty (Just size) 0 []

store :: Operand -> Operand -> Codegen ()
store ptr val = instrDo $ Store False ptr val Nothing 0 []
{-# INLINE store #-}

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []
{-# INLINE load #-}

-- Control Flow
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

phi :: Type -> [(Operand, Name)] -> Codegen Operand
phi ty incoming = instr $ Phi ty incoming []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

getelementptr addr indices = instr $ GetElementPtr False addr indices []
{-# INLINE getelementptr #-}

globalStringRef :: String -> C.Constant
globalStringRef name = C.GlobalReference (T.ptr $ stringStructType (length name + 1)) (AST.Name (getStringLitName name))

globalStringRefAsPtr name = C.BitCast (globalStringRef name) ptrType

one = constOp $ C.Float (F.Double 1.0)
zero = constOp $ C.Float (F.Double 0.0)
false = zero
true = one

toSig :: [S.Arg] -> [(SBS.ShortByteString, AST.Type)]
toSig = map (\(S.Arg name tpe) -> (fromString (show name), ptrType))

getStringLitName :: String -> SBS.ShortByteString
getStringLitName s = name
  where
    name = fromString $ take 15 s ++ "." ++ show hash
    hash = hash32 s

createStruct args = C.Struct Nothing False args

createCharString s = (C.Array T.i8 bytes, len)
  where
    bytestring = UTF8.fromString s
    constByte b = C.Int 8 (toInteger b)
    bytes = map constByte (ByteString.unpack bytestring ++ [fromInteger 0])
    len = ByteString.length bytestring + 1

createString s = (createStruct [constInt (len - 1), C.Array T.i8 bytes], len)
  where
    bytestring = UTF8.fromString s
    constByte b = C.Int 8 (toInteger b)
    bytes = map constByte (ByteString.unpack bytestring ++ [fromInteger 0])
    len = ByteString.length bytestring + 1

defineStringLit :: String -> LLVM ()
defineStringLit s = defineConst (getStringLitName s) (stringStructType len) string
  where (string, len) = createString s

--            Lasca Runtime Data Representation Types
funcType retTy args = T.FunctionType retTy args False

stringStructType len = T.StructureType False [intType, T.ArrayType (fromIntegral len) T.i8]

laTypeStructType = T.StructureType False [ptrType]

boxStructOfType boxedType = T.StructureType False [ptrType, boxedType]

boxStructType = boxStructOfType ptrType

arrayStructType elemType = T.StructureType False [intType, T.ArrayType 0 elemType]

positionStructType = T.StructureType False [intType, intType]

closureStructType = T.StructureType False [intType, intType, ptrType]

functionStructType = T.StructureType False [ptrType, ptrType, intType]

functionsStructType len = T.StructureType False [intType, arrayTpe len]
  where arrayTpe len = T.ArrayType len functionStructType

runtimeStructType = T.StructureType False [ptrType, ptrType, boolType]