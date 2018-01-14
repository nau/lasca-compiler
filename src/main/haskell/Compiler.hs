{-# LANGUAGE Strict #-}
module Compiler where

import Parser
import Codegen
import Emit
import JIT
import Infer
import Type
import Syntax
import Options


import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import Text.Printf
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS

import System.IO
import System.Environment
import System.Exit
import System.Process
import System.Directory
import System.FilePath
import System.Console.Haskeline
import Data.List
import qualified Data.Set as Set
import Debug.Trace as Debug
import qualified Text.Megaparsec as Megaparsec
import Control.Lens.TH

import qualified LLVM.AST as AST
import qualified LLVM.Module as LLVM
import qualified LLVM.Target as LLVM

greet :: LascaOpts -> IO ()
greet opts | null (lascaFiles opts) = repl opts
           | otherwise = do
   let file = head (lascaFiles opts)
   processFile opts file
   return ()

initModule :: String -> AST.Module
initModule name = emptyModule name

process :: LascaOpts -> AST.Module -> String -> IO (Maybe AST.Module)
process opts modo source = do
  let res = parseToplevel source
  case res of
    Left err -> die (show err)
    Right ex -> do
      ast <- codegen opts modo ex
      return $ Just ast

codegen :: LascaOpts -> AST.Module -> [Expr] -> IO AST.Module
codegen opts modo fns = do
  ast <- codegenModule opts modo fns
  runJIT opts ast
  return ast

genModule :: LascaOpts -> AST.Module -> String -> IO AST.Module
genModule opts modo source = do
    let importPreludeAst = Import emptyMeta "Prelude"
    let filePath = Char8.unpack (SBS.fromShort $ AST.moduleSourceFileName modo)
    dir <- getCurrentDirectory
    let absoluteFilePath = dir </> filePath
    case parseToplevelFilename absoluteFilePath source of
      Left err -> do
          die $ Megaparsec.parseErrorPretty err
      Right exprs -> do
          let exprs' = (importPreludeAst : exprs)
          (imported, ex) <- loadImports Set.empty [] exprs'
--          Debug.traceM $ printf "AAA %s\n%s" (show  exprs') (show ex)
--          print exprs
          when (verboseMode opts) $ putStrLn ("Parsed OK, imported " ++ show imported)
          when (printAst opts) $ mapM_ print ex
          when (verboseMode opts) $ putStrLn("Compiler mode is " ++ mode opts)
          codegenModule opts modo ex

readMod :: LascaOpts -> String -> IO AST.Module
readMod opts fname = do
  file <- readFile fname
  genModule opts (initModule fname) file

processFile :: LascaOpts -> String -> IO ()
processFile opts fname = do
  mod <- readMod opts fname
  processModule opts mod fname

processModule :: LascaOpts -> AST.Module -> String -> IO ()
processModule opts mod fname = if exec opts then
  do when (verboseMode opts) $ putStrLn "Running JIT"
     runJIT opts mod
  else
  do withOptimizedModule opts mod $ (\context m -> do
          ll <- LLVM.moduleLLVMAssembly m
          let asm = Char8.unpack ll
          writeFile (fname ++ ".ll") asm
          LLVM.withHostTargetMachine $ \tm -> LLVM.writeObjectToFile tm (LLVM.File (fname ++ ".o")) m)
     let name = takeWhile (/= '.') fname
     let optLevel = optimization opts
     let optimizationOpts = ["-O" ++ show optLevel | optLevel > 0]
     callProcess "clang-5.0"
       (optimizationOpts ++
          ["-e", "_start", "-g", "-o", name, "-L.", "-llascart",
           fname ++ ".o"])
     return ()


repl :: LascaOpts -> IO ()
repl opts = runInputT defaultSettings (loop (initModule "Lasca JIT"))
  where
  loop :: AST.Module -> InputT IO ()
  loop mod = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> do
        modn <- lift $ process opts mod input
        case modn of
          Just modn -> loop modn
          Nothing -> loop mod

loadImport :: Set.Set Name -> [Name] -> Name -> IO (Set.Set Name, [Expr])
loadImport imported importPath name = do
--    Debug.traceM $ printf "loadImport %s %s %s" (show imported) (show importPath) (show name)
    when (name `elem` importPath) $ die (printf "Circular dependency in %s -> %s" (show importPath) (show name))
    if name `Set.member` imported
    then return (imported, [])
    else do
        let path = "src/main/lasca/" ++ show name ++ ".lasca"
        p <- readFile path
        case parseToplevel p of
            Left err -> die $ Megaparsec.parseErrorPretty err
            Right moduleExprs -> do
                (newImported, importedExprs) <- loadImports imported (name : importPath) moduleExprs
                return $ (newImported, importedExprs)


getImports exprs =
    foldl' (\imports expr -> case expr of
                          Import _ name -> name : imports
                          _ -> imports
                          ) [] exprs

loadImports :: Set.Set Name -> [Name] -> [Expr] -> IO (Set.Set Name, [Expr])
loadImports imported importPath exprs = do
    let imports = getImports exprs
    foldM (\acc@(imported, exprs) name -> do
        (newImported, newExprs) <- loadImport imported importPath name
        return $ (Set.union imported newImported, newExprs ++ exprs)
        ) (imported, exprs) imports

main :: IO ()
main = parseOptions >>= greet
