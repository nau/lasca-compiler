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
import qualified LLVM.General.AST.Type as T
import qualified LLVM.General.AST.Instruction as I
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.IntegerPredicate as IP
import qualified LLVM.General.AST.FloatingPointPredicate as FP

import LLVM.General.ExecutionEngine ( withMCJIT, withModuleInEngine, getFunction )

import Data.Word
import Data.Int
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map

import Codegen
import JIT (runJIT)
import qualified Syntax as S

one = constant $ C.Float (F.Double 1.0)
zero = constant $ C.Float (F.Double 0.0)
false = zero
true = one

externArgsToSig :: [S.Arg] -> [(AST.Type, AST.Name)]
externArgsToSig = map (\(S.Arg name tpe) -> (typeMapping tpe, AST.Name name))

toSig :: [S.Arg] -> [(AST.Type, AST.Name)]
toSig = map (\(S.Arg name tpe) -> (ptrType, AST.Name name))


codegenTop :: S.Expr -> LLVM ()
codegenTop (S.Function name tpe args body) = do
  define retType name largs bls
  where
    largs = toSig args
    retType = typeMapping tpe
    bls = createBlocks $ execCodegen [] $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM args $ \(S.Arg n t) -> do
        var <- alloca (typeMapping t)   -- FIXME: this shouldn't be necessary
        store var (local (AST.Name n))
        assign n var
      cgen body >>= ret

codegenTop (S.Extern name tpe args) = do
  external llvmType name fnargs
  where
    llvmType = typeMapping tpe
    fnargs = externArgsToSig args

codegenTop exp = do
  define double "main" [] bls
  where
    bls = createBlocks $ execCodegen [] $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen exp >>= ret


-- Static mode
typeMapping :: S.Type -> AST.Type
typeMapping S.AnyType = ptrType
typeMapping (S.Type "Any") = ptrType
typeMapping (S.Type "Unit") = T.void
typeMapping (S.Type "Bool") = T.i1
typeMapping (S.Type "Int") = T.i32
typeMapping (S.Type "Float64") = T.double

-- Dynamic mode
-- typeMapping _ = typeInfoType
-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

cgen :: S.Expr -> Codegen AST.Operand
cgen (S.UnaryOp op a) = do
  cgen $ S.Apply ("unary" ++ op) [a]
cgen (S.Let a b c) = do
  i <- alloca (T.ptr T.i8)
  val <- cgen b
  store i val
  assign a i
  cgen c
cgen (S.BinaryOp op a b) = do
  lhs <- cgen a
  rhs <- cgen b
  let codeNum = case op of
              "==" -> 42
              "<" -> 44
              "+" -> 10
              "-" -> 11
              "*" -> 12
              "/" -> 13
  let code = constInt codeNum
  call (global runtimeBinOpFuncType (AST.Name "runtimeBinOp")) [code, lhs, rhs]
cgen (S.Var x) = getvar x >>= load
cgen (S.Literal l) = box l
cgen (S.Apply fn args) = do
  largs <- mapM cgen args
  call (global ptrType (AST.Name fn)) largs
cgen (S.If cond tr fl) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"

  -- %entry
  ------------------
  cond <- cgen cond
  -- unbox Bool
  voidPtrCond <- call (global unboxFuncType (AST.Name "unbox")) [cond, constInt 0]
  bool <- ptrtoint voidPtrCond T.i1

  test <- instr2 T.i1 (I.ICmp IP.EQ bool constTrue [])
--   let test = constTrue
  cbr test ifthen ifelse -- Branch based on the condition

  -- if.then
  ------------------
  setBlock ifthen
  trval <- cgen tr       -- Generate code for the true branch
  br ifexit              -- Branch to the merge block
  ifthen <- getBlock

  -- if.else
  ------------------
  setBlock ifelse
  flval <- cgen fl       -- Generate code for the false branch
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

boxFuncType = funcType ptrType [T.i32]
runtimeBinOpFuncType = funcType ptrType [T.i32, ptrType, ptrType]
unboxFuncType = funcType ptrType [ptrType, T.i32]

box :: S.Lit -> Codegen AST.Operand
box (S.BoolLit b) = call (global boxFuncType (AST.Name "boxBool")) [constInt (boolToInt b)]
box (S.IntLit  n) = call (global boxFuncType (AST.Name "boxInt")) [constInt (toInteger n)]

boolToInt True = 1
boolToInt False = 0

codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen modo fns = do
  let modn = mapM codegenTop fns
      ast = runLLVM modo modn
  runJIT ast
  return ast

codegenModule :: AST.Module -> [S.Expr] -> AST.Module
codegenModule modo fns = ast
    where
        modn = mapM codegenTop fns
        ast = runLLVM modo modn
