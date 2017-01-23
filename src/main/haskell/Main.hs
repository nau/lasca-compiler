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
import System.Console.Haskeline

import qualified LLVM.General.AST as AST

initModule :: AST.Module
initModule = emptyModule "my cool jit"

process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
  let res = parseToplevel source
  case res of
    Left err -> print err >> return Nothing
    Right ex -> do
      ast <- codegen modo ex
      return $ Just ast

genModule :: AST.Module -> String -> Maybe AST.Module
genModule modo source = case parseToplevel source of
    Left err -> Nothing
    Right ex -> Just (codegenModule modo ex)




readMod :: String -> IO (Maybe AST.Module)
readMod fname = do
  file <- readFile fname
  return $ genModule initModule file

processFile1 :: Maybe AST.Module -> IO (Maybe String)
processFile1 (Just mod) = getLLAsString mod


processFile :: String -> IO (Maybe String)
processFile fname = do
  mod <- readMod fname
  processFile1 mod

repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
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
        result <- processFile fname
        case result of
            Just (s) -> putStrLn(s)
            Nothing -> return ()
        return ()
