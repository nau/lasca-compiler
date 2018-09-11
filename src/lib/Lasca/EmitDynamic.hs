module Lasca.EmitDynamic where

import LLVM.Module
import LLVM.Context
import LLVM.Analysis
import LLVM.PassManager

import qualified LLVM.AST as AST
import qualified LLVM.AST.Global
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Instruction as I
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.FunctionAttribute as FA
import qualified LLVM.AST.IntegerPredicate as IPred

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
import Lasca.EmitCommon
import Lasca.Infer
import qualified Lasca.Syntax as S
import Lasca.Syntax (Ctx)
import qualified Lasca.Options as Opts

cgen :: Ctx -> S.Expr -> Codegen AST.Operand
cgen ctx (S.Let False meta a _ b c) = do
    i <- alloca $ llvmTypeOf b
    val <- cgen ctx b
    store i val
    assign a i
    cgen ctx c
cgen ctx (S.Ident meta name) = do
    syms <- gets symtab
    modState <- gets moduleState
    let mapping = functions modState
    case lookup name syms of
      Just x ->
  --       Debug.trace ("Local " ++ show name)
          load x
      Nothing | name `Map.member` S._globalFunctions ctx -> boxClosure name mapping []
              | name `Map.member` S._globalVals ctx -> load (globalOp ptrType (nameToSBS name))
              | otherwise -> boxError (nameToText name)
cgen ctx (S.Literal meta l) = do
--  Debug.traceM $ "Generating literal " ++ show l ++ " on " ++ show (S.pos meta)
    boxLit l meta
cgen ctx this@(S.Array meta exprs) = do
    vs <- sequence [cgen ctx e | e <- exprs]
    boxArray vs
cgen ctx this@(S.Select meta tree expr) = cgenSelect ctx this

cgen ctx this@(S.Apply meta (S.Ident _ "unary-") [expr]) = cgenApplyUnOp ctx this
cgen ctx this@(S.Apply meta (S.Ident _ fn) [lhs, rhs]) | fn `Map.member` binops = cgenApplyBinOp ctx this
cgen ctx (S.Apply meta expr args) = cgenApply ctx meta expr args
cgen ctx (S.Closure _ funcName enclosedVars) = do
    modState <- gets moduleState
    let mapping = functions modState
    boxClosure funcName mapping enclosedVars
cgen ctx m@S.Match{} =
    error $ printf "Match expressions should be already desugared! %s at: %s" (show m) (show $ S.exprPosition m)
cgen ctx (S.If meta cond tr fl) = cgenIfDynamic ctx meta cond tr fl
cgen ctx e = error ("cgen shit " ++ show e)

cgenIfDynamic ctx meta cond tr fl = do
    let resultType = llvmTypeOf tr
    let test = do
            cond <- cgen ctx cond
            -- unbox Bool
            bool <- unboxBoolDynamically cond
            instr (I.ICmp IP.EQ bool constTrue [])
    cgenIf resultType test (cgen ctx tr) (cgen ctx fl)

cgenSelect ctx this@(S.Select meta tree expr) = do
    tree <- cgen ctx tree
    e <- cgen ctx expr
    let pos = createPosition $ S.pos meta
    callBuiltin "runtimeSelect" [tree, e, constOp pos]
cgenSelect ctx e = error ("cgenSelect should only be called on Select, but called on" ++ show e)

cgenApplyUnOp ctx this@(S.Apply meta op@(S.Ident _ "unary-") [expr]) = do
    lexpr <- cgen ctx expr
    callBuiltin "runtimeUnaryOp" [constIntOp 1, lexpr]
cgenApplyUnOp ctx e = error ("cgenApplyUnOp should only be called on Apply, but called on" ++ show e)

cgenApplyBinOp ctx (S.Apply meta (S.Ident _ fn) [lhs, rhs]) = do
    llhs <- cgen ctx lhs
    lrhs <- cgen ctx rhs
    let code = fromMaybe (error ("Couldn't find binop " ++ show fn)) (Map.lookup fn binops)
    let codeOp = constIntOp code
    callBuiltin "runtimeBinOp" [codeOp, llhs, lrhs]
cgenApplyBinOp ctx e = error ("cgenApplyBinOp should only be called on Apply, but called on" ++ show e)

cgenApply ctx meta expr args = do
    syms <- gets symtab
    let symMap = Map.fromList syms
    let isGlobal fn = (fn `Map.member` S._globalFunctions ctx) && not (fn `Map.member` symMap)
    case expr of
       -- TODO Here are BUGZZZZ!!!! :)
       -- TODO check arguments!
       -- this is done to speed-up calls if you `a global function
        S.Ident _ fn | isGlobal fn -> do
            let f = S._globalFunctions ctx Map.! fn
--            Debug.traceM $ printf "Calling %s" fn
            largs <- forM args $ \arg -> cgen ctx arg
            callFn (funcLLvmType f) (show fn) largs

        expr -> do
            modState <- gets moduleState
            e <- cgen ctx expr
            largs <- mapM (cgen ctx) args
            let argc = constIntOp (length largs)
            sargsPtr <- allocaSize ptrType argc
            let asdf (idx, arg) = do
                  p <- getelementptr sargsPtr [idx]
                  store p arg
            sargs <- bitcast sargsPtr ptrType -- runtimeApply accepts i8*, so need to bitcast. Remove when possible
            -- cdecl calling convension, arguments passed right to left
            sequence_ [asdf (constIntOp i, a) | (i, a) <- zip [0..] largs]
            let pos = createPosition $ S.pos meta
            callBuiltin "runtimeApply" [e, argc, sargs, constOp pos]
