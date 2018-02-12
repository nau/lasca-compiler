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

parsePhase opts filename = do
    exists <- doesFileExist filename
    if exists then do
        absoluteFilePath <- canonicalizePath filename
        file <- readFile absoluteFilePath
        case parseToplevelFilename absoluteFilePath file of
          Left err -> die $ Megaparsec.parseErrorPretty err
          Right exprs -> do
              exprs1 <- fixPackageAndImportPrelude filename exprs
              searchPaths <- moduleSearchPaths
              (imported, ex) <- loadImports searchPaths Set.empty [] exprs1
    --          Debug.traceM $ printf "AAA %s\n%s" (show  exprs1) (show ex)
              when (verboseMode opts) $ putStrLn ("Parsed OK, imported " ++ show imported)
              when (printAst opts) $ mapM_ print ex
              when (verboseMode opts) $ putStrLn ("Compiler mode is " ++ (show $ mode opts))
              return ex
    else error $ printf "Couldn't open file %s" (show filename)

fixPackageAndImportPrelude filename exprs = case exprs of
    (pkg@(Package _ name): exprs) -> do
        when (takeBaseName filename /= (last $ nameToList name)) $
          die $ printf "Wrong package name in file %s. Package name should match file name, but was %s)" (filename) (show name)
        return $ pkg : insertImportPrelude name exprs
    _ -> do
        let name = Name $ takeBaseName filename
        let pkg = Package emptyMeta name
        return $ pkg : insertImportPrelude name exprs

insertImportPrelude name exprs = if name == Name "Prelude" then exprs else Import emptyMeta "Prelude" : exprs

nameToList (Name n) = [n]
nameToList (NS prefix n) = nameToList prefix ++ nameToList n

moduleSearchPaths = do
    dir <- getCurrentDirectory
    lascaPathEnv <- lookupEnv "LASCAPATH"
    let lascaPaths = splitSearchPath $ fromMaybe "" lascaPathEnv
    absPaths <- mapM canonicalizePath lascaPaths
    existingPaths <- filterM doesDirectoryExist absPaths
    -- TODO add XDB paths
    return $ nub $ dir : existingPaths

findModulePath searchPaths name = do
    let relPath = (path name) <.> "lasca"
    result <- findFile searchPaths relPath
    case result of
        Just file -> return file
        Nothing -> error $ printf "Couldn't find module %s. Search path: %s" (show name) (show $ intercalate "," searchPaths)
  where
    path (Name n) = n
    path (NS prefix n) = path prefix </> path n

parseModule searchPaths name = loadImport searchPaths Set.empty [] name

loadImports :: [FilePath] -> Set Name -> [Name] -> [Expr] -> IO (Set Name, [Expr])
loadImports searchPaths imported importPath exprs = do
    let imports = getImports exprs
--    Debug.traceM $ printf "loadImports2 %s %s %s" (show imported) (show importPath) (show $ imports)
    foldM (\(imported, exprs) name -> do
        (newImported, newExprs) <- loadImport searchPaths imported importPath name
        return $ (Set.union imported newImported, newExprs ++ exprs)
        ) (imported, exprs) imports

loadImport searchPaths imported importPath name = do
--    Debug.traceM $ printf "loadImport %s %s %s" (show imported) (show importPath) (show name)
    when (name `elem` importPath) $ die (printf "Circular dependency in %s -> %s" (show importPath) (show name))
    if name `Set.member` imported
    then return (imported, [])
    else do
        absoluteFilePath <- findModulePath searchPaths name
        file <- readFile absoluteFilePath
        case parseToplevelFilename absoluteFilePath file of
          Left err -> die $ Megaparsec.parseErrorPretty err
          Right exprs -> do
              exprs1 <- fixPackageAndImportPrelude absoluteFilePath exprs
              (newImported, importedExprs) <- loadImports searchPaths imported (name : importPath) exprs1
              return $ (Set.insert name newImported, importedExprs)

getImports exprs =
    foldl' (\imports expr -> case expr of
              Import _ name -> name : imports
              _ -> imports
           ) [] exprs

runPhases opts filename = do
    exprs <- parsePhase opts filename
    let (named, state) = namerPhase opts exprs
    let ctx = _context state
    let mainPackage = _currentPackage state
    let mainFunctionName = NS mainPackage "main"
    let desugared = desugarExprs ctx desugarExpr named
    typed <- if mode opts == Static
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
    let modo = emptyModule filename
    let cgen = if mode opts == Static then EmitStatic.cgen else EmitDynamic.cgen
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
    callProcess "clang"
      (optimizationOpts ++
          ["-g", "-o", name, "-llascart", fname ++ ".o"])
    return ()

runLasca opts = do
    if null (lascaFiles opts)
    then die ("need file") -- TODO show help
    else do
        let file = head (lascaFiles opts)
        processMainFile opts file

main = do
    opts <- parseOptions
    runLasca opts
