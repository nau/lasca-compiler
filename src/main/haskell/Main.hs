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

import qualified LLVM.General.AST as AST

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
        return (Just (codegenModule modo ex))




readMod :: String -> IO (Maybe AST.Module)
readMod fname = do
  file <- readFile fname
  genModule (initModule fname) file

processFile :: String -> IO ()
processFile fname = do
  mod <- readMod fname
  putStrLn "Read module OK"
  case mod of
    Just mod -> do
        runJIT mod
--         Just(asm) <- getLLAsString mod
--         writeFile (fname ++ ".ll") asm
--         let name = dropWhile (/= '.') fname
--         callProcess "gcc" ["-o", name, fname ++ ".ll"]
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
main = do
  args <- getArgs
  case args of
    []      -> repl
    [fname] -> do
        processFile fname
