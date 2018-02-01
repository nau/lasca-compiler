module Lasca.Options (
    LascaOpts(..),
    parseOptions
) where

import Options.Applicative
import Data.Semigroup ((<>))

data LascaOpts = LascaOpts
    { lascaFiles   :: [String]
    , mode         :: String
    , exec         :: Bool
    , verboseMode  :: Bool
    , printLLVMAsm :: Bool
    , printAst     :: Bool
    , printTypes   :: Bool
    , optimization :: Int
    } deriving (Show, Eq)

optimizeOpt :: Parser Int
optimizeOpt = option auto
            ( long "optimization-level"
           <> short 'O'
           <> value 0
           <> help "Optimization level for LLVM" )

lascaOptsParser :: Parser LascaOpts
lascaOptsParser = LascaOpts
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
      ( long "verbose"
      <> help "Verbose mode" )
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


parseOptions = execParser opts
  where opts = info (helper <*> lascaOptsParser)
                ( fullDesc
               <> progDesc "Print a greeting for TARGET"
               <> header "Lasca compiler" )