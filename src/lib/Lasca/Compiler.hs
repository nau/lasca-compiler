module Lasca.Compiler where

import Lasca.Parser
import Lasca.Namer
import Lasca.Desugar
import Lasca.Codegen
import Lasca.EmitCommon
import Lasca.Emit
import qualified Lasca.EmitStatic as EmitStatic
import qualified Lasca.EmitDynamic as EmitDynamic
import Lasca.JIT
import Lasca.Infer
import Lasca.Type
import Lasca.Syntax
import Lasca.Options


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
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace as Debug
import qualified Text.Megaparsec as Megaparsec
import Control.Lens.TH

import qualified LLVM.AST as AST
import qualified LLVM.Module as LLVM
import qualified LLVM.Target as LLVM

initModule :: String -> AST.Module
initModule name = emptyModule name

parsePhase opts filename = do
    file <- readFile filename
    let importPreludeAst = Import emptyMeta "Prelude"
    let filePath = filename
    dir <- getCurrentDirectory
    let absoluteFilePath = dir </> filePath
    case parseToplevelFilename absoluteFilePath file of
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
          return ex

runPhases opts filename = do
    exprs <- parsePhase opts filename
    let (named, state) = namerPhase opts exprs
    let ctx = _context state
    let mainPackage = _currentPackage state
    let mainFunctionName = NS mainPackage "main"
    let desugared = desugarExprs ctx desugarExpr named
    typed <- if mode opts == "static"
             then typerPhase opts ctx filename desugared
             else return desugared
    when (printAst opts) $ putStrLn $ intercalate "\n" (map printExprWithType exprs)
    let desugared2 = delambdafy ctx typed -- must be after typechecking
    let mod = codegenPhase opts ctx filename desugared2 mainFunctionName
    if exec opts then do
        when (verboseMode opts) $ putStrLn "Running JIT"
        runJIT opts mod
    else compileExecutable opts filename mod

typerPhase opts ctx filename exprs = do
    case typeCheck ctx exprs of
        Right (env, typedExprs) -> do
            when (verboseMode opts) $ putStrLn "typechecked OK"
            when (printTypes opts) $ putStrLn (showPretty env)
            return typedExprs
        Left e -> do
            dir <- getCurrentDirectory
            let source = dir </> filename
            die $ (source ++ ":" ++ showTypeError e)

codegenPhase opts ctx filename exprs mainFunctionName = do
    let modo = initModule filename
    let cgen = if mode opts == "static" then EmitStatic.cgen else EmitDynamic.cgen
    runLLVM modo $  do
        declareStdFuncs
        fmt <- genFunctionMap exprs
        let defs = reverse (_dataDefs ctx)
        tst <- genTypesStruct ctx defs
        genRuntime opts fmt tst
        forM_ exprs $ \expr -> do
            defineStringConstants expr
            codegenTop ctx cgen expr
        codegenStartFunc ctx cgen (show mainFunctionName)

processMainFile :: LascaOpts -> String -> IO ()
processMainFile opts filename = runPhases opts filename

compileExecutable opts fname mod = do
    withOptimizedModule opts mod $ (\context m -> do
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

loadImport :: Set Name -> [Name] -> Name -> IO (Set Name, [Expr])
loadImport imported importPath name = do
--    Debug.traceM $ printf "loadImport %s %s %s" (show imported) (show importPath) (show name)
    when (name `elem` importPath) $ die (printf "Circular dependency in %s -> %s" (show importPath) (show name))
    if name `Set.member` imported
    then return (imported, [])
    else do
        let path = "examples/" ++ show name ++ ".lasca"
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

loadImports :: Set Name -> [Name] -> [Expr] -> IO (Set Name, [Expr])
loadImports imported importPath exprs = do
    let imports = getImports exprs
    foldM (\acc@(imported, exprs) name -> do
        (newImported, newExprs) <- loadImport imported importPath name
        return $ (Set.union imported newImported, newExprs ++ exprs)
        ) (imported, exprs) imports

runLasca opts = do
    if null (lascaFiles opts)
    then die ("need file") -- TODO show help
    else do
        let file = head (lascaFiles opts)
        processMainFile opts file

main = do
    opts <- parseOptions
    runLasca opts
