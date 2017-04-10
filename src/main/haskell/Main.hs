--------------------------------------------------------------------
-- |
-- Module    :  Main
-- Copyright :  (c) Stephen Diehl 2013
-- License   :  MIT
-- Maintainer:  stephen.m.diehl@gmail.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
{-# LANGUAGE Strict #-}
module Main where

import Parser
import Codegen
import Emit
import JIT
import Infer
import Type
import Syntax


import Control.Monad
import Control.Monad.Trans

import System.IO
import System.Environment
import System.Process
import System.Console.Haskeline
import Options.Applicative
import Data.Semigroup ((<>))
import qualified Debug.Trace as Debug

import qualified LLVM.AST as AST

optimizeOpt :: Parser Int
optimizeOpt = option auto
            ( long "optimization-level"
           <> short 'g'
           <> value 0
           <> help "Optimization level for LLVM" )

lascaOpts :: Parser LascaOpts
lascaOpts = LascaOpts
  <$> many (argument str (metavar "FILES..."))
  <*> strOption
      ( long "mode"
      <> short 'm'
      <> value "static"
      <> help "Compiler mode. Options are [dynamic | static]. Static by default."
      )
  <*> switch
      ( long "exec"
     <> short 'e'
     <> help "Execute immediately" )
  <*> switch
      ( long "print-llvm"
      <> help "Print LLVM IR" )
  <*> switch
        ( long "print-ast"
        <> help "Print AST" )
  <*> switch
        ( long "print-types"
        <> help "Print infered types" )
  <*> optimizeOpt

greet :: LascaOpts -> IO ()
greet opts | null (lascaFiles opts) = repl opts
           | otherwise = do
   mapM (processFile opts) (lascaFiles opts)
   return ()

initModule :: String -> AST.Module
initModule name = emptyModule name

process :: LascaOpts -> AST.Module -> String -> IO (Maybe AST.Module)
process opts modo source = do
  let res = parseToplevel source
  case res of
    Left err -> print err >> return Nothing
    Right ex -> do
      ast <- codegen opts modo ex
      return $ Just ast

genModule :: LascaOpts -> AST.Module -> String -> IO (Maybe AST.Module)
genModule opts modo source = case parseToplevel source of
    Left err -> do
        putStrLn $ show err
        return Nothing
    Right ex -> do
        putStrLn "Parsed OK"
        if printAst opts then putStrLn (show ex) else return ()
        putStrLn("Compiler mode is " ++ (mode opts))
        if ((mode opts) == "static")
        then case typeCheck ex of
          Right env -> do
            putStrLn "typechecked OK"
            if printTypes opts then putStrLn $ show env else return ()
            return (Just (codegenModule modo ex))
          Left e -> do
            putStrLn $ show e
            return Nothing
        else return (Just (codegenModule modo ex))


prelude = readFile "src/main/lasca/Prelude.lasca"
-- prelude = return ""

readMod :: LascaOpts -> String -> IO (Maybe AST.Module)
readMod opts fname = do
  pre  <- prelude
  file <- readFile fname
  genModule opts (initModule fname) (pre ++ "\n" ++file)

processFile :: LascaOpts -> String -> IO ()
processFile opts fname = do
  mod <- readMod opts fname
  putStrLn "Read module OK"
  case mod of
    Just mod -> processModule opts mod fname
    Nothing -> do putStrLn "Couldn't compile a module"

processModule :: LascaOpts -> AST.Module -> String -> IO ()
processModule opts mod fname = do
  if (exec opts)
  then do
    putStrLn "Running JIT"
    res <- runJIT opts mod
    case res of
        Left err -> putStrLn err
        Right m -> return ()
    return ()
  else do
    Just(asm) <- getLLAsString mod
    writeFile (fname ++ ".ll") asm
    let name = takeWhile (/= '.') fname
    -- Dynamic linking
    let optLevel = optimization opts
    let optimizationOpts = if optLevel > 0 then ["-O" ++ show optLevel] else []
    callProcess "clang-4.0" (optimizationOpts ++ ["-e", "_start", "-g", "-o", name, "-L.", "-llascart", fname ++ ".ll"])
    -- Static linking
  --         callProcess "clang-3.5" ["-e", "_start", "-g", "-o", name, "-L.", {-"-llascart",-} fname ++ ".ll", "/usr/local/lib/libgc.a","liblascart.a"]
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

main :: IO ()
main = execParser opts >>= greet
  where
    opts = info (helper <*> lascaOpts)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "Lasca compiler" )


typeCheck :: [Expr] -> Either TypeError TypeEnv
typeCheck exprs = inferTop emptyTyenv ({-Debug.trace (show a)-} a)
  where
        a = map f exprs
        f e@(Fix (Function name _ _ _)) = (name, e)
        f e@(Val name _) = (name, e)
        f e@(Function name _ _ _) = (name, e)
        f e@(Extern name _ _) = (name, e)
        f e@(Data name _) = (name, e)
        f e = error ("What the fuck " ++ show e)
