module Lasca.Options (
    LascaOpts(..),
    TypingMode(..),
    parseOptions,
    emptyLascaOpts
) where

import Options.Applicative
import Data.Semigroup ((<>))

data TypingMode = Static | Dynamic deriving (Eq)
instance Show TypingMode where
    show Static = "static"
    show Dynamic = "dynamic"

instance Read TypingMode where
    readsPrec _ "static" =  [(Static, "")]
    readsPrec _ "dynamic" = [(Dynamic, "")]
    readsPrec _ _ = []

data LascaOpts = LascaOpts
    { lascaFiles   :: [String]
    , mode         :: TypingMode
    , outputFile   :: String
    , exec         :: Bool
    , verboseMode  :: Bool
    , printLLVMAsm :: Bool
    , printAst     :: Bool
    , printTypes   :: Bool
    , optimization :: Int
    } deriving (Show, Eq)

emptyLascaOpts = LascaOpts {
    lascaFiles  = [],
    mode = Static,
    outputFile = "",
    exec = False,
    verboseMode = False,
    printLLVMAsm = False,
    printAst = False,
    printTypes = False,
    optimization = 0
}

optimizeOpt :: Parser Int
optimizeOpt = option auto
            ( long "optimization-level"
           <> short 'O'
           <> value 0
           <> help "Optimization level for LLVM" )

lascaOptsParser :: Parser LascaOpts
lascaOptsParser = LascaOpts
  <$> some (argument str (metavar "FILES..."))
  <*> option auto
      ( long "mode"
      <> short 'm'
      <> value Static
      <> help "Compiler mode. Options are [dynamic | static]. Static by default."
      )
  <*> strOption
      ( short 'o'
      <> value ""
      <> help "Write output to FILE"
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
        <> help "Print inferred types" )
  <*> optimizeOpt


parseOptions = execParser opts
  where opts = info (helper <*> lascaOptsParser)
                ( fullDesc
               <> progDesc "Lasca Compiler 0.1"
               <> header "Lasca Compiler 0.1" )