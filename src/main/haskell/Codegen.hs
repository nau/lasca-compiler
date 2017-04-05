--------------------------------------------------------------------
-- |
-- Module    :  Codegen
-- Copyright :  (c) Stephen Diehl 2013
-- License   :  MIT
-- Maintainer:  stephen.m.diehl@gmail.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Codegen where

import Data.Word
import Data.String
import Data.List
import Data.Function
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
-- import qualified Data.Text as Text
import qualified Data.ByteString as ByteString
-- import qualified Data.Text.Encoding as Encoding
import qualified Data.ByteString.Char8 as Char8

import Control.Monad.State
import Control.Applicative
-- import Control.Lens

import LLVM.AST
import LLVM.AST.Global
import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as T

import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.Float as F

import qualified Debug.Trace as Debug

import qualified Syntax as S


-------------------------------------------------------------------------------
-- Module Level
-------------------------------------------------------------------------------

newtype LLVM a = LLVM { unLLVM :: State ModuleState a }
  deriving (Functor, Applicative, Monad, MonadState ModuleState )

data ModuleState = ModuleState {
  _llvmModule :: AST.Module,
  _locals :: Set.Set String,
  _outers :: Set.Set String,
  _usedVars :: Set.Set String,
  _syntacticAst :: [S.Expr],
  _globalValsInit :: [(S.Name, S.Expr)],
  _modNames :: Names,
  functions :: Map.Map String Int
} deriving (Show)

-- makeLenses ''ModuleState

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM modl s = result where
  state = execState (unLLVM s) (initModuleState modl)
  result = _llvmModule state

initModuleState modl = ModuleState {
  _llvmModule = modl,
  _locals = Set.empty,
  _outers = Set.empty,
  _usedVars = Set.empty,
  _syntacticAst = [],
  _globalValsInit = [],
  _modNames = Map.empty,
  functions = Map.empty
}

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn d = do
  modl <- gets _llvmModule
  let defs = moduleDefinitions modl
  if d `elem` defs
  then modify id
  else modify $ \s -> s { _llvmModule = (_llvmModule s) { moduleDefinitions = defs ++ [d] } }

defineGlobal name tpe body = addDefn $
  AST.GlobalDefinition $ AST.globalVariableDefaults {
    LLVM.AST.Global.name        = name
  , LLVM.AST.Global.isConstant  = False
  , LLVM.AST.Global.type' = tpe
  , LLVM.AST.Global.initializer = body
  }

defineConst name tpe body = addDefn $
  AST.GlobalDefinition $ AST.globalVariableDefaults {
    LLVM.AST.Global.name        = name
  , LLVM.AST.Global.isConstant  = True
  , LLVM.AST.Global.type' = tpe
  , LLVM.AST.Global.initializer = body
  }

define ::  Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = body
  }

external ::  Type -> String -> [(Type, Name)] -> Bool -> LLVM ()
external retty label argtys vararg = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], vararg)
  , returnType  = retty
  , basicBlocks = []
  }

---------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- IEEE 754 double
double :: Type
double = FloatingPointType 64 IEEE

intType :: Type
intType = IntegerType 32

boolType :: Type
boolType = IntegerType 1

typeInfoStructType = T.StructureType False [T.i32, ptrType]
ptrType = T.ptr T.i8

ptrSize :: Int
ptrSize = 8

-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

type Names = Map.Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm,  Map.insert nm 1 ns)
    Just ix -> (nm ++ show ix, Map.insert nm (ix+1) ns)

-------------------------------------------------------------------------------
-- Codegen State
-------------------------------------------------------------------------------

type SymbolTable = [(String, Operand)]

data CodegenState
  = CodegenState {
    currentBlock :: Name                     -- Name of the active block to append to
  , blocks       :: Map.Map Name BlockState  -- Blocks for function
  , symtab       :: SymbolTable              -- Function scope symbol table
  , blockCount   :: Int                      -- Count of basic blocks
  , count        :: Word                     -- Count of unnamed instructions
  , names        :: Names                    -- Name Supply
  , moduleState  :: ModuleState
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
makeBlock (l, (BlockState _ s t)) = BasicBlock l s (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ (show l)

entryBlockName :: String
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: ModuleState -> CodegenState
emptyCodegen = \ms -> CodegenState {
  currentBlock = (Name entryBlockName),
  blocks = Map.empty,
  symtab = [],
  blockCount = 1,
  count = 0,
  names = Map.empty,
  moduleState = ms
}

execCodegen :: [(String, Operand)] -> ModuleState -> Codegen a -> CodegenState
execCodegen vars modState m = execState (runCodegen m) (emptyCodegen modState) { symtab = vars }

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

instr :: Instruction -> Codegen (Operand)
{-# INLINE instr #-}
instr ins = do
  n <- fresh
  let ref = (UnName n)
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = i ++ [ref := ins] } )
  return $ local ref

instr2 :: Type -> Instruction -> Codegen (Operand)
instr2 tpe ins = do
  n <- fresh
  let ref = (UnName n)
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = i ++ [ref := ins] } )
  return $ LocalReference tpe ref

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  return trm

named :: String -> Codegen a -> Codegen Operand
named iname m = m >> do
  blk <- current
  let b = Name iname
      (_ := x) = last (stack blk)
  modifyBlock $ blk { stack = init (stack blk) ++ [b := x] }
  return $ local b

-- icmp :: Operand -> Operand -> Codegen (Operand)
-- icmp lhs rhs = instr2 T.i1 (ICmp)

-------------------------------------------------------------------------------
-- Block Stack
-------------------------------------------------------------------------------

entry :: Codegen Name
entry = gets currentBlock

addBlock :: String -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix <- gets blockCount
  nms <- gets names
  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms
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

assign :: String -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = [(var, x)] ++ lcls }

getvar :: String -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

-------------------------------------------------------------------------------

-- References
local ::  Name -> Operand
local = LocalReference ptrType

global :: Type -> Name -> Operand
global tpe name = constant (C.GlobalReference tpe name)



constant :: C.Constant -> Operand
constant = ConstantOperand

constNull = IntToPtr (constInt 0) ptrType []

constInt :: Int -> Operand
constInt i = constant (C.Int 32 (toInteger i))

constFloat i = constant (C.Float (F.Double i))

constByte b = constant (C.Int 8 b)
constTrue = constant (C.Int 1 1)
constFalse = constant (C.Int 1 0)
constRef name = let ptr = C.GlobalReference ptrType (AST.Name name) in C.BitCast ptr ptrType
constRefOperand name = constant (constRef name)

uitofp :: Type -> Operand -> Codegen Operand
uitofp ty a = instr $ UIToFP a ty []

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))


bitcast op toTpe= instr2 toTpe (BitCast op toTpe [])
ptrtoint op toTpe= instr2 toTpe (PtrToInt op toTpe [])
inttoptr op toTpe= instr2 toTpe (IntToPtr op toTpe [])

-- Effects
call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

allocaSize ty size = instr $ Alloca ty (Just size) 0 []

store :: Operand -> Operand -> Codegen Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []

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