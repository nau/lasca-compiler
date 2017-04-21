--------------------------------------------------------------------
-- |
-- Module    :  JIT
-- Copyright :  (c) Stephen Diehl 2013
-- License   :  MIT
-- Maintainer:  stephen.m.diehl@gmail.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
{-# LANGUAGE Strict #-}
module JIT (
  jit,
  runJIT,
  getLLAsString
) where

import           Data.Int
import           Data.Word
import           Foreign.Ptr          (FunPtr, castFunPtr)
import           Syntax

import           Control.Monad.Except

import qualified LLVM.AST             as AST
import           LLVM.CodeModel
import           LLVM.Context
import           LLVM.Module          as Mod
import           LLVM.Target

import           LLVM.Analysis
import           LLVM.PassManager
import           LLVM.Transforms

import qualified LLVM.ExecutionEngine as EE

foreign import ccall "dynamic" haskFun :: FunPtr (IO Double) -> IO Double
foreign import ccall "dynamic" startFun :: FunPtr (IO ()) -> IO ()

run :: FunPtr a -> IO Double
run fn = haskFun (castFunPtr fn :: FunPtr (IO Double))

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 2  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

passes :: Int -> PassSetSpec
passes level = defaultCuratedPassSetSpec { optLevel = Just (fromIntegral level) }

runJIT :: LascaOpts -> AST.Module -> IO (Either String AST.Module)
runJIT opts mod = withContext $ \context ->
    jit context $ \executionEngine ->
      runExceptT $ withModuleFromAST context mod $ \m ->
        withPassManager (passes (optimization opts)) $ \pm -> do
          -- Optimization Pass
          runPassManager pm m
          optmod <- moduleAST m

          when (printLLVMAsm opts) $ do
            s <- moduleLLVMAssembly m
            putStrLn s

          EE.withModuleInEngine executionEngine m $ \ee -> do
            initLascaRuntime <- EE.getFunction ee (AST.Name "start")
--             mainfn <- EE.getFunction ee (AST.Name "main")
            case initLascaRuntime of
              Just fn -> startFun (castFunPtr fn :: FunPtr (IO ()))
              Nothing -> putStrLn "Couldn't find initLascaRuntime!"

          -- Return the optimized module
          return optmod

getLLAsString :: AST.Module -> IO (Maybe String)
getLLAsString mod = do
    eith <- withContext $ \context ->
        runExceptT $ withModuleFromAST context mod $ \m -> do
          putStrLn "Getting LLVM assembly..."
          moduleLLVMAssembly m
    case eith of
        Left err -> do
            putStrLn err
            return Nothing
        Right s -> return $ Just s

