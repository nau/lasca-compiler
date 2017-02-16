--------------------------------------------------------------------
-- |
-- Module    :  Emit
-- Copyright :  (c) Stephen Diehl 2013
-- License   :  MIT
-- Maintainer:  stephen.m.diehl@gmail.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Emit where

import LLVM.General.Module
import LLVM.General.Context
import LLVM.General.Analysis
import LLVM.General.PassManager

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Global
import qualified LLVM.General.AST.Type as T
import qualified LLVM.General.AST.Instruction as I
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.IntegerPredicate as IP
import qualified LLVM.General.AST.FloatingPointPredicate as FP

-- import qualified Data.Text as Text
import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as Encoding
import qualified Data.ByteString.UTF8 as UTF8

import LLVM.General.ExecutionEngine ( withMCJIT, withModuleInEngine, getFunction )

import qualified Data.Text
import qualified Data.ByteString
import qualified Data.Text.Encoding
import Data.Digest.Murmur32
import Data.Maybe
import Data.Word
import Data.Int
import Control.Monad.State
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Debug.Trace as Debug

import Codegen
import Type
import JIT (runJIT)
import qualified Syntax as S

-- data Ctx = Context
type Ctx = Set.Set String

one = constant $ C.Float (F.Double 1.0)
zero = constant $ C.Float (F.Double 0.0)
false = zero
true = one

externArgsToSig :: [S.Arg] -> [(AST.Type, AST.Name)]
externArgsToSig = map (\(S.Arg name tpe) -> (typeMapping tpe, AST.Name name))

toSig :: [S.Arg] -> [(AST.Type, AST.Name)]
toSig = map (\(S.Arg name tpe) -> (ptrType, AST.Name name))


stringStructType len = T.StructureType False [T.i32, T.ArrayType (fromIntegral len) T.i8]
applyStructType len = T.StructureType False [T.i32, T.ArrayType (fromIntegral len) T.i8]


getStringLitName s = AST.Name name
  where
    name = (take 15 s) ++ "." ++ (show hash)
    hash = hash32 s

defineStringLit :: String -> LLVM ()
defineStringLit s = do  addDefn $ AST.GlobalDefinition $ AST.globalVariableDefaults {
                          LLVM.General.AST.Global.name        = getStringLitName s
                        , LLVM.General.AST.Global.isConstant  = True
                        , LLVM.General.AST.Global.type' = stringStructType len
                        , LLVM.General.AST.Global.initializer = Just (C.Struct (Nothing) False [C.Int 32 (toInteger len), C.Array T.i8 bytes])
                        }
  where
    bytestring = UTF8.fromString s
    constByte b = C.Int 8 (toInteger b)
    bytes = map constByte (ByteString.unpack bytestring)
    len = ByteString.length bytestring

defineStringConstants :: S.Expr -> LLVM ()
defineStringConstants (S.Literal (S.StringLit s)) = defineStringLit s
defineStringConstants (S.If cond true false) = do
  defineStringConstants cond
  defineStringConstants true
  defineStringConstants false
  return ()
defineStringConstants (S.Apply _ exprs) = do
  mapM defineStringConstants exprs
  return ()
defineStringConstants (S.Let _ e body) = do
  defineStringConstants e
  defineStringConstants body
  return ()
defineStringConstants _ = return ()

-- codegenTop :: S.Expr -> LLVM ()
codegenTop ctx (S.Data name args) = return ()

codegenTop ctx (S.Function name tpe args body) = do
  defineStringConstants body
  define retType name largs bls
  where
    largs = toSig args
    retType = ptrType -- typeMapping tpe
    bls = createBlocks $ execCodegen [] $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM args $ \(S.Arg n t) -> do
--         var <- alloca (typeMapping t)   -- FIXME: this shouldn't be necessary
        var <- alloca ptrType
        store var (local (AST.Name n))
        assign n var
      cgen ctx body >>= ret

codegenTop _ (S.Extern name tpe args) = do
  external llvmType name fnargs
  where
    llvmType = typeMapping tpe
    fnargs = externArgsToSig args

codegenTop ctx exp = do
  define double "main" [] bls
  where
    bls = createBlocks $ execCodegen [] $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen ctx exp >>= ret

codegenInit = do
  define T.void "start" [] bls
  where
    bls = createBlocks $ execCodegen [] $ do
        entry <- addBlock entryBlockName
        setBlock entry
        call (global initLascaRuntimeFuncType (AST.Name "initLascaRuntime")) []
        call (global mainFuncType (AST.Name "main")) []
        terminator $ I.Do $ I.Ret (Nothing) []
        return ()


-- Static mode
typeMapping :: Type -> AST.Type
typeMapping (TCon "Any") = ptrType
typeMapping (TCon "Unit") = T.void
typeMapping (TCon "Bool") = T.i1
typeMapping (TCon "Int") = T.i32
typeMapping (TCon "Float64") = T.double
typeMapping (TCon "String") = ptrType

-- Dynamic mode
-- typeMapping _ = typeInfoType
-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

binops :: Map.Map String Integer
binops = Map.fromList [("+", 10), ("-", 11), ("*", 12), ("/", 13),
  ("==", 42), ("!=", 43), ("<", 44), ("<=", 45), (">=", 46), (">", 47)]

cgen :: Ctx -> S.Expr -> Codegen AST.Operand
cgen ctx (S.Let a b c) = do
  i <- alloca (T.ptr T.i8)
  val <- cgen ctx b
  store i val
  assign a i
  cgen ctx c
cgen ctx (S.Var x) = do
  syms <- gets symtab
  case lookup x syms of
    Just x -> Debug.trace ("Found " ++ show x) (load x)
    Nothing | x `Set.member` ctx -> do
                                      let tpe = funcType ptrType [ptrType]
                                      let a = Debug.trace ("Global ident " ++ x) x
                                      bitcast (global tpe (AST.Name x)) ptrType
            | otherwise -> error $ "Ooops, can't find symbol " ++ x
cgen ctx (S.Literal l) = box l
cgen ctx (S.Apply (S.Var "or") [lhs, rhs]) = cgen ctx (S.If lhs (S.Literal (S.BoolLit True)) rhs)
cgen ctx (S.Apply (S.Var "and") [lhs, rhs]) = cgen ctx (S.If lhs rhs (S.Literal (S.BoolLit False)))
cgen ctx (S.Apply (S.Var fn) [lhs, rhs]) | fn `Map.member` binops = do
  llhs <- cgen ctx lhs
  lrhs <- cgen ctx rhs
  let code = constInt (binops Map.! fn)
  call (global runtimeBinOpFuncType (AST.Name "runtimeBinOp")) [code, llhs, lrhs]
cgen ctx (S.Apply (S.Var fn) args) = do
  largs <- mapM (cgen ctx) args
  syms <- gets symtab
  case lookup fn syms of
    Just x  -> do
      let arglist = map (\x -> ptrType) args
      op <- bitcast (AST.LocalReference ptrType (AST.Name fn)) (T.ptr (funcType ptrType arglist))
      call (op) largs
    Nothing -> call (global ptrType (AST.Name fn)) largs
cgen ctx (S.If cond tr fl) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"

  -- %entry
  ------------------
  cond <- cgen ctx cond
  -- unbox Bool
  voidPtrCond <- call (global unboxFuncType (AST.Name "unbox")) [cond, constInt 0]
  bool <- ptrtoint voidPtrCond T.i1

  test <- instr2 T.i1 (I.ICmp IP.EQ bool constTrue [])
  cbr test ifthen ifelse -- Branch based on the condition

  -- if.then
  ------------------
  setBlock ifthen
  trval <- cgen ctx tr       -- Generate code for the true branch
  br ifexit              -- Branch to the merge block
  ifthen <- getBlock

  -- if.else
  ------------------
  setBlock ifelse
  flval <- cgen ctx fl       -- Generate code for the false branch
  br ifexit              -- Branch to the merge block
  ifelse <- getBlock

  -- if.exit
  ------------------
  setBlock ifexit
  phi ptrType [(trval, ifthen), (flval, ifelse)]

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------
funcType retTy args = T.FunctionType retTy args False

initLascaRuntimeFuncType = funcType T.void []
mainFuncType = funcType ptrType []
boxFuncType = funcType ptrType [T.i32]
runtimeBinOpFuncType = funcType ptrType [T.i32, ptrType, ptrType]
runtimeApplyFuncType = funcType ptrType [ptrType]
unboxFuncType = funcType ptrType [ptrType, T.i32]

box :: S.Lit -> Codegen AST.Operand
box (S.BoolLit b) = call (global boxFuncType (AST.Name "boxBool")) [constInt (boolToInt b)]
box (S.IntLit  n) = call (global boxFuncType (AST.Name "boxInt")) [constInt (toInteger n)]
box (S.StringLit s) = do
  let name = getStringLitName s
  let len = ByteString.length . UTF8.fromString $ s
  let ref = global (stringStructType len) name
  ref' <- bitcast ref ptrType
  call (global boxFuncType (AST.Name "box")) [constInt 3, ref']


boolToInt True = 1
boolToInt False = 0

codegen :: S.LascaOpts -> AST.Module -> [S.Expr] -> IO AST.Module
codegen opts modo fns = do
  let ast = codegenModule modo fns
  runJIT opts ast
  return ast

codegenModule :: AST.Module -> [S.Expr] -> AST.Module
codegenModule modo fns = ast2
    where
        ctx = createGlobalContext fns
        modn = mapM (codegenTop ctx) fns
        ast = runLLVM modo modn
        ast2 = runLLVM ast codegenInit

createGlobalContext :: [S.Expr] -> Set.Set String
createGlobalContext exprs = context
  where
      context = loop exprs Set.empty
      loop [] m = m
      loop (e:exprs) m = names e m `Set.union` loop exprs m
      names (S.Function name _ _ _) m = Set.insert name m
      names (S.Extern name _ _) m = Set.insert name m