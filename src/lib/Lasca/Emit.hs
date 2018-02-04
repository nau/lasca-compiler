module Lasca.Emit (codegenTop) where

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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Debug.Trace as Debug
import System.Exit
import System.Directory
import System.FilePath

import Lasca.Codegen
import Lasca.Type
import Lasca.Infer
import Lasca.EmitCommon
import Lasca.Desugar
import Lasca.Namer
import qualified Lasca.EmitDynamic as EmitDynamic
import qualified Lasca.EmitStatic as EmitStatic
import Lasca.Syntax
import qualified Lasca.Options as Opts

codegenTop ctx cgen topExpr = case topExpr of
    this@(Let meta name expr _) -> do
        modify (\s -> s { _globalValsInit = _globalValsInit s ++ [(name, expr)] })
        let valType = llvmTypeOf this
    --    Debug.traceM $ printf "Cons %s: %s" (show name) (show valType)
        defineGlobal (nameToSBS name) valType (Just $ defaultValueForType valType)

    f@(Function meta name tpe args body) ->
        if meta ^. isExternal then do
            let (Literal _ (StringLit externName)) = body
            external (externalTypeMapping tpe) (fromString externName) (externArgsToSig args) False []
        else do
            modState <- get
            let codeGenResult = codeGen modState
            let blocks = createBlocks codeGenResult
            mapM_ defineStringLit (generatedStrings codeGenResult)
            let retType = mappedReturnType args funcType
            define retType (nameToSBS name) largs blocks
      where
        funcType = typeOf f
        largs = map (\(n, t) -> (nameToSBS n, t)) argsWithTypes

        funcTypeToLlvm (Arg name _) (TypeFunc a b, acc) = (b, (name, typeMapping a) : acc)
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

    (Data _ name tvars constructors) -> return ()
    Package{} -> return ()
    Import{} -> return ()
    _ -> error $ printf "Expression of this kind should not get to codegenTop. It's a bug. %s at %s"
            (show topExpr) (show $ exprPosition topExpr)