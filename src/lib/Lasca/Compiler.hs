module Lasca.Compiler where

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
import Lasca.Modules


import Control.Monad
import Data.Maybe
import Text.Printf
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as Char8

import System.Info
import System.Environment
import System.Exit
import System.Process
import System.Directory
import System.FilePath
import Data.List
import qualified Data.Map.Strict as Map
import Debug.Trace as Debug
import Control.Applicative

import qualified LLVM.Module as LLVM
import qualified LLVM.Target as LLVM
import qualified LLVM.Relocation as Reloc
import qualified LLVM.Target.Options as TO
import qualified LLVM.CodeModel as CodeModel
import qualified LLVM.CodeGenOpt as CodeGenOpt


parsePhase opts filename = do
    exists <- doesFileExist filename
    if exists then do
        absoluteFilePath <- canonicalizePath filename
        searchPaths <- moduleSearchPaths
        (imported, mainModule) <- loadModule searchPaths Map.empty [] absoluteFilePath (Name $ T.pack filename)
        let linearized = linearizeIncludes mainModule
        let ex = foldr (\m exprs -> moduleExprs m ++ exprs) [] linearized
--          Debug.traceM $ printf "AAA %s\n%s" (show  exprs1) (show ex)
        when (verboseMode opts) $ putStrLn $ printf "Parsed OK, imported %s, linearized: %s" (show imported) (show linearized)
        when (printAst opts) $ mapM_ print ex
        when (verboseMode opts) $ putStrLn ("Compiler mode is " ++ show (mode opts))
        return ex
    else error $ printf "Couldn't open file %s" (show filename)

runPhases opts filename = do
    exprs <- parsePhase opts filename
    let (named, state) = namerPhase opts exprs
    let ctx = _context state
    let mainModule = _currentModule state
    let mainFunctionName = NS mainModule "main"
    let desugared = desugarPhase ctx named
    typed <- if mode opts == Static
             then typerPhase opts ctx filename desugared
             else return desugared
    let desugared2 = patmatPhase ctx typed
    let desugared3 = lambdaLiftPhase ctx desugared2 -- must be after typechecking
    let !desugared4 = delambdafyPhase ctx desugared3 -- must be after typechecking
    when (printAst opts) $ putStrLn $ intercalate "\n" (map printExprWithType desugared4)
    let mod = codegenPhase ctx filename desugared4 mainFunctionName
    if exec opts then do
        when (verboseMode opts) $ putStrLn "Running JIT"
        runJIT opts mod
    else compileExecutable opts filename mod

typerPhase opts ctx filename exprs = do
    result <- typeCheck ctx exprs
    case result of
        Right (env, typedExprs) -> do
            when (verboseMode opts) $ putStrLn "typechecked OK"
            when (printTypes opts) $ putStrLn (showPretty env)
            return typedExprs
        Left e -> do
            dir <- getCurrentDirectory
            let source = dir </> filename
            die (source ++ ":" ++ showTypeError e)

codegenPhase context filename exprs mainFunctionName = do
    let opts = _lascaOpts context
    let modo = emptyModule filename
    let cgen = if mode opts == Static then EmitStatic.cgen else EmitDynamic.cgen
    let ctx = collectGlobals context exprs
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

findCCompiler = do
    ccEnv <- lookupEnv "CC"
    cl5 <- findExecutable "clang-5"
    clang <- findExecutable "clang"
    gcc <- findExecutable "gcc"
    return $ ccEnv <|> cl5 <|> clang <|> gcc

withHostTargetMachine :: (LLVM.TargetMachine -> IO a) -> IO a
withHostTargetMachine f = do
    LLVM.initializeAllTargets
    triple <- LLVM.getProcessTargetTriple
    cpu <- LLVM.getHostCPUName
    features <- LLVM.getHostCPUFeatures
    (target, _) <- LLVM.lookupTarget Nothing triple
    LLVM.withTargetOptions $ \options ->
        LLVM.withTargetMachine target triple cpu features options Reloc.PIC CodeModel.Default CodeGenOpt.Default f


compileExecutable opts fname mod = do
    withOptimizedModule opts mod $ \context m -> do
        ll <- LLVM.moduleLLVMAssembly m
        let asm = Char8.unpack ll
        writeFile (fname ++ ".ll") asm
        withHostTargetMachine $ \tm -> LLVM.writeObjectToFile tm (LLVM.File (fname ++ ".o")) m
    let outputPath = case outputFile opts of
          [] -> dropExtension fname
          path -> path
    let optLevel = optimization opts
    let optimizationOpts = ["-O" ++ show optLevel | optLevel > 0]
    result <- findCCompiler
    lascaPathEnv <- lookupEnv "LASCAPATH"
    absLascaPathEnv <- mapM canonicalizePath lascaPathEnv
    let lascaPath = fromMaybe "." absLascaPathEnv
    let cc = fromMaybe (error "Did find C compiler. Install Clang or GCC, or define CC environment variable") result
        lascartStaticLink = if os == "darwin"
            then ["-llascartStatic"]
            else ["-Wl,--whole-archive", "-llascartStatic" , "-Wl,--no-whole-archive"]
        lascartDynamicLink = ["-llascart"]
        libLascaLink = ["-rdynamic"] 
            -- passes --export-dynamic to the linker. 
            -- Needed for OrcJit to to able to dynamicly load generated `main` function
            ++ lascartStaticLink
            -- ++ lascartDynamicLink
        libDirs = ["-L" ++ lascaPath]
        links = ["-lgc", "-lffi", "-lm", "-lpcre2-8"]
    let args = optimizationOpts ++ libDirs ++ ["-fPIC", "-g"] ++ libLascaLink ++ links ++ [ "-o", outputPath, fname ++ ".o"]
    when (verboseMode opts) $ putStrLn (intercalate " " $ cc : args)
    callProcess cc args
    return ()

runLasca :: LascaOpts -> IO ()
runLasca opts = do
    if null (lascaFiles opts)
    then die ("need file") -- TODO show help
    else do
        let file = head (lascaFiles opts)
        processMainFile opts file

main :: IO ()
main = do
    opts <- parseOptions
    runLasca opts
