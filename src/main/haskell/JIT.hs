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
  withOptimizedModule,
  getLLAsString
) where

import           Data.Int
import           Data.Word
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.IO as TIO
import System.IO
import           Foreign.Ptr          (FunPtr, Ptr, castFunPtr)
import           Foreign.C.String
import           Foreign.C.Types
import Foreign.Marshal.Array
import           Syntax
import Options

import           Control.Monad.Except

import qualified LLVM.AST             as AST
import           LLVM.CodeModel
import           LLVM.Context
import           LLVM.Module          as Mod
import           LLVM.Target

import           LLVM.Analysis
import           LLVM.PassManager
import           LLVM.Transforms
--import LLVM.Pretty (ppllvm)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8

import qualified LLVM.ExecutionEngine as EE

foreign import ccall "dynamic" startFun :: FunPtr (Int -> Ptr CString -> IO ()) -> Int -> Ptr CString -> IO ()

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 2  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

passes :: Int -> PassSetSpec
passes level = defaultCuratedPassSetSpec { optLevel = Just (fromIntegral level) }

runJIT :: LascaOpts -> AST.Module -> IO ()
runJIT opts mod = do
--    let llvmAst = ppllvm mod
--    TIO.putStrLn $ LT.toStrict llvmAst
    withOptimizedModule opts mod $ \context m -> do
        jit context $ \executionEngine -> do
            let args = lascaFiles opts
            let len = length args
            EE.withModuleInEngine executionEngine m $ \ee -> do
                startFunPtr <- EE.getFunction ee (AST.Name "start")
                case startFunPtr of
                    Just fn -> do
                        cargs <- mapM newCString args
                        array <- mallocArray len
                        pokeArray array cargs
                        startFun (castFunPtr fn :: FunPtr (Int -> Ptr CString -> IO ())) len array
                    Nothing -> putStrLn "Couldn't find function start!"
            return ()

withOptimizedModule opts mod f = withContext $ \context -> do
    withModuleFromAST context mod $ \m ->
        withPassManager (passes (optimization opts)) $ \pm -> do
            -- Optimization Pass
--                    linkModules m stdModule
            runPassManager pm m
            optmod <- moduleAST m
            when (printLLVMAsm opts) $ do
                s <- moduleLLVMAssembly m
                Char8.putStrLn s
            f context m

getLLAsString :: AST.Module -> IO String
getLLAsString mod = do
    s <- withContext $ \context ->
        withModuleFromAST context mod $ \m -> do
            putStrLn "Getting LLVM assembly..."
            moduleLLVMAssembly m
    return $ Char8.unpack s

