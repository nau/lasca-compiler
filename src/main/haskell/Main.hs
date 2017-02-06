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

module Main where

import Parser
import Codegen
import Emit
import JIT

import Control.Monad
import Control.Monad.Trans

import System.IO
import System.Environment
import System.Process
import System.Console.Haskeline
import Options.Applicative

import qualified LLVM.General.AST as AST

data LascaOpts = LascaOpts
  { lascaFiles :: [String]
  , exec :: Bool
  , printLLVMAsm :: Bool
  }

sample :: Parser LascaOpts
sample = LascaOpts
  <$> some (argument str (metavar "FILES..."))
  <*> switch
      ( long "exec"
     <> help "Execute immediately" )
  <*> switch
      ( long "print-llvm"
      <> help "Execute immediately" )

greet :: LascaOpts -> IO ()
greet (LascaOpts [] _ _) = repl
greet (LascaOpts files exec printLlvm) = do
    if exec
    then mapM (execFile) files
    else mapM processFile files
    return ()

initModule :: String -> AST.Module
initModule name = emptyModule name

process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
  let res = parseToplevel source
  case res of
    Left err -> print err >> return Nothing
    Right ex -> do
      ast <- codegen modo ex
      return $ Just ast

genModule :: AST.Module -> String -> IO (Maybe AST.Module)
genModule modo source = case parseToplevel source of
    Left err -> do
        putStrLn $ show err
        return Nothing
    Right ex -> do
        putStrLn "Parsed OK"
        putStrLn (show ex)

        return (Just (codegenModule modo ex))


prelude = readFile "Prelude.lasca"

readMod :: String -> IO (Maybe AST.Module)
readMod fname = do
  pre  <- prelude
  file <- readFile fname
  genModule (initModule fname) (pre ++ file)

execFile :: String -> IO ()
execFile fname = do
  mod <- readMod fname
  putStrLn "Read module OK"
  case mod of
    Just mod -> do
        res <- runJIT mod
        case res of
          Left err -> putStrLn err
          Right m -> return ()
        return ()
    Nothing -> do
        putStrLn "Couldn't compile a module"

processFile :: String -> IO ()
processFile fname = do
  mod <- readMod fname

  putStrLn "Read module OK"
  case mod of
    Just mod -> do
        Just(asm) <- getLLAsString mod
        writeFile (fname ++ ".ll") asm
        let name = takeWhile (/= '.') fname
-- Dynamic linking
        callProcess "clang-3.5" ["-e", "_start", "-g", "-o", name, "-L.", "-llascart", fname ++ ".ll"]
-- Static linking
--         callProcess "clang-3.5" ["-e", "_start", "-g", "-o", name, "-L.", {-"-llascart",-} fname ++ ".ll", "/usr/local/lib/libgc.a","liblascart.a"]
        return ()
    Nothing -> do
        putStrLn "Couldn't compile a module"



repl :: IO ()
repl = runInputT defaultSettings (loop (initModule "Lasca JIT"))
  where
  loop :: AST.Module -> InputT IO ()
  loop mod = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> do
        modn <- lift $ process mod input
        case modn of
          Just modn -> loop modn
          Nothing -> loop mod

main :: IO ()
main = execParser opts >>= greet
  where
    opts = info (helper <*> sample)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

