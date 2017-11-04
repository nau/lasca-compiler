{-# LANGUAGE Strict #-}
module Emit (codegenModule) where

import qualified LLVM.Module
import qualified LLVM.Context
import qualified LLVM.Analysis
import qualified LLVM.PassManager

import qualified LLVM.AST as AST
import qualified LLVM.AST.Global
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Instruction as I
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.FunctionAttribute as FA

-- import qualified Data.Text as Text
import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as Encoding
import Text.Printf
import qualified Data.ByteString.UTF8 as UTF8
import Data.String
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS

import LLVM.ExecutionEngine ( withMCJIT, withModuleInEngine, getFunction )

import qualified Data.Text
import qualified Data.ByteString
import qualified Data.Text.Encoding
import Data.Digest.Murmur32
import Data.Maybe
import qualified Data.List as List
import Data.Word
import Data.Int
import Control.Monad.State
import Control.Monad.Except
import Control.Applicative
import qualified Control.Lens as Lens
import Control.Lens.Operators
import Control.Lens.TH
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Debug.Trace as Debug
import System.Exit
import System.Directory
import System.FilePath

import Codegen
import Type
import Infer
import EmitCommon
import Desugar
import qualified EmitDynamic as EmitDynamic
import qualified EmitStatic as EmitStatic
import qualified Syntax as S
import Syntax (Ctx, createGlobalContext)
import qualified Options as Opts

codegenModule :: Opts.LascaOpts -> AST.Module -> [S.Expr] -> IO AST.Module
codegenModule opts modo exprs = do
    let desugared = desugarExprs ctx desugarExpr exprs
    (exprs, cgen) <- typecheck ctx modo desugared -- TODO remove modo
    when (Opts.printAst opts) $ putStrLn $ List.intercalate "\n" (map S.printExprWithType exprs)
    let desugared = delambdafy ctx exprs -- must be after typechecking
    return $ modul cgen desugared
  where
    ctx = createGlobalContext opts exprs
    modul cgen exprs = runLLVM modo $ genModule cgen exprs
    genModule cgen exprs = do
        declareStdFuncs
        fmt <- genFunctionMap exprs
        let defs = reverse (S.dataDefs ctx)
        tst <- genTypesStruct ctx defs
        genRuntime opts fmt tst
        forM_ exprs $ \expr -> do
            defineStringConstants expr
            codegenTop ctx cgen expr
        codegenStartFunc ctx cgen


typecheck ctx modo exprs = do
    let opts = ctx ^. S.lascaOpts
    if Opts.mode opts == "static"
    then case typeCheck ctx exprs of
             Right (env, typedExprs) -> do
                 when (Opts.verboseMode opts) $ putStrLn "typechecked OK"
                 when (Opts.printTypes opts) $ print env
                 return (typedExprs, EmitStatic.cgen)
             Left e -> do
                 let sourceSBS = AST.moduleSourceFileName modo
                 dir <- getCurrentDirectory
                 let source = dir </> Char8.unpack (SBS.fromShort sourceSBS)
                 die $ (source ++ ":" ++ showTypeError e)
    else return (exprs, EmitDynamic.cgen)

codegenTop ctx cgen topExpr = case topExpr of
    this@(S.Let meta name expr _) -> do
        modify (\s -> s { _globalValsInit = _globalValsInit s ++ [(name, expr)] })
        let valType = llvmTypeOf this
    --    Debug.traceM $ printf "Cons %s: %s" (show name) (show valType)
        defineGlobal (nameToSBS name) valType (Just $ defaultValueForType valType)

    f@(S.Function meta name tpe args body) ->
        if meta ^. S.isExternal then do
            let (S.Literal _ (S.StringLit externName)) = body
            external (externalTypeMapping tpe) (fromString externName) (externArgsToSig args) False []
        else do
            modState <- get
            let codeGenResult = codeGen modState
            let blocks = createBlocks codeGenResult
            mapM_ defineStringLit (generatedStrings codeGenResult)
            let retType = mappedReturnType args funcType
            define retType (nameToSBS name) largs blocks
      where
        funcType = S.typeOf f
        largs = map (\(n, t) -> (nameToSBS n, t)) argsWithTypes

        funcTypeToLlvm (S.Arg name _) (TypeFunc a b, acc) = (b, (name, typeMapping a) : acc)
        funcTypeToLlvm arg t = error $ "AAA3" ++ show arg ++ show t

        argsWithTypes = do
    --        Debug.traceM $ printf "codegenTop %s(%s): %s" (show name) (show args) (show funcType)
            reverse $ snd $ foldr funcTypeToLlvm (funcType, []) (reverse args)

        codeGen modState = execCodegen [] modState $ do
      --      Debug.traceM $ printf "argsWithTypes %s" (show argsWithTypes)
            entry <- addBlock entryBlockName
            setBlock entry
            forM_ argsWithTypes $ \(n, t) -> do
                var <- alloca t
                store var (local t (nameToSBS n))
        --        Debug.traceM $ printf "assign %s: %s = %s" n (show t) (show var)
                assign n var
            cgen ctx body >>= ret

    (S.Data _ name tvars constructors) -> return ()
    S.Package{} -> return ()
    S.Import{} -> return ()
    _ -> error $ printf "Expression of this kind should not get to codegenTop. It's a bug. %s at %s"
            (show topExpr) (show $ S.exprPosition topExpr)