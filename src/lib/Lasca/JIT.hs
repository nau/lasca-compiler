module Lasca.JIT (
  runJIT,
  withOptimizedModule
) where

import           Data.Int
import           Data.Word
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.IO as TIO
import System.IO
import           Foreign.Ptr
import           Foreign.C.String
import           Foreign.C.Types
import Foreign.Marshal.Array
import           Lasca.Syntax
import Lasca.Options

import           Control.Monad.Except

import qualified LLVM.AST             as AST
import           LLVM.CodeModel
import           LLVM.Context
import           LLVM.Module          as Mod
import LLVM.Target hiding (withHostTargetMachine)

import           LLVM.Analysis
import           LLVM.PassManager
import           LLVM.Transforms
import           LLVM.OrcJIT
import           LLVM.OrcJIT.CompileLayer
import           LLVM.Linking (loadLibraryPermanently, getSymbolAddressInProcess)
import qualified LLVM.CodeGenOpt as CodeGenOpt
import qualified LLVM.CodeModel as CodeModel
import qualified LLVM.Relocation as Reloc
--import LLVM.Pretty (ppllvm)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8

foreign import ccall "dynamic" mainFun :: FunPtr (Int -> Ptr CString -> IO ()) -> Int -> Ptr CString -> IO ()

passes :: Int -> PassSetSpec
passes level = defaultCuratedPassSetSpec { optLevel = Just (fromIntegral level) }

withHostTargetMachine :: (TargetMachine -> IO a) -> IO a
withHostTargetMachine f = do
  initializeAllTargets
  triple <- getProcessTargetTriple
  cpu <- getHostCPUName
  features <- getHostCPUFeatures
  (target, _) <- lookupTarget Nothing triple
  withTargetOptions $ \options ->
    -- Make it PIC, otherwise it won't work with shared libraries
    withTargetMachine target triple cpu features options Reloc.PIC CodeModel.Default CodeGenOpt.Default f


resolver :: IRCompileLayer l -> SymbolResolver
resolver compileLayer =
  SymbolResolver
    (\s -> findSymbol compileLayer s True)
    (\s ->
       fmap (\a -> Right $ JITSymbol a (JITSymbolFlags False True False True)) (getSymbolAddressInProcess s)
    )

{-
  Read https://purelyfunctional.org/posts/2018-04-02-llvm-hs-jit-external-function.html
  for explanation.
-}
runJIT :: LascaOpts -> AST.Module -> IO ()
runJIT opts mod = do
--    putStrLn $ LT.unpack $ ppllvm mod
    b <- loadLibraryPermanently Nothing
    unless (not b) (error "Couldn’t load library")
    withOptimizedModule opts mod $ \context m ->
        withHostTargetMachine $ \tm ->
            withObjectLinkingLayer $ \linkingLayer ->
                withIRCompileLayer linkingLayer tm $ \compileLayer ->
                    withModule compileLayer m
                        (resolver compileLayer) $ \moduleHandle -> do
                            mainSymbol <- mangleSymbol compileLayer "main"
                            (Right (JITSymbol mainFn _)) <- findSymbol compileLayer mainSymbol True
                            let args = lascaFiles opts
                            let len = length args
                            cargs <- mapM newCString args
                            array <- mallocArray len
                            pokeArray array cargs
                            result <- mainFun (castPtrToFunPtr (wordPtrToPtr mainFn)) len array
                            return ()

withOptimizedModule opts mod f = withContext $ \context ->
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
