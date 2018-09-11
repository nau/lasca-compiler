module Lasca.EmitStatic where

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

import Lasca.Codegen as Codegen
import Lasca.Type
import Lasca.EmitCommon
import Lasca.Infer
import qualified Lasca.Syntax as S
import Lasca.Syntax (Ctx)
import qualified Lasca.Options as Opts

dataTypeHasField ctx typeName fieldName =
    typeName `Set.member` (S._dataDefsNames ctx) && fieldName `Map.member` (S._dataDefsFields ctx Map.! typeName)

isDataType ctx tpe = case tpe of
    TypeIdent name | name `Set.member` (S._dataDefsNames ctx) -> True
    _ -> False

isFuncType (TypeFunc _ _) = True
isFuncType _ = False

lascaPrimitiveTypes = Set.fromList [TypeInt, TypeFloat, TypeBool, TypeString, TypeAny, TypeUnit]
anyTypeVar = TVar $ TV "a"

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
-- TODO Either extend it to other types and generalize or make builtin functions inlineable
cgen ctx this@(S.Apply meta (S.Ident _ (NS "Bits" fn)) [lhs, rhs]) | fn `elem` ["intAnd", "intOr", "intXor", "intShiftL", "intShiftR"] = do
    llhs <- cgen ctx lhs
    lrhs <- cgen ctx rhs
    a <- unboxInt llhs
    b <-  unboxInt lrhs
    case fn of
        (Name "intAnd") -> instrTyped intType (I.And a b []) >>= boxInt
        (Name "intOr")  -> instrTyped intType (I.Or a b []) >>= boxInt
        (Name "intXor") -> instrTyped intType (I.Xor a b []) >>= boxInt
        (Name "intShiftL") -> instrTyped intType (I.Shl False False a b []) >>= boxInt
        (Name "intShiftR") -> instrTyped intType (I.AShr False a b []) >>= boxInt
        _ -> error $ printf "Unsupported builtin operation %s" (show $ S.exprPosition this)
        
cgen ctx (S.Apply meta expr args) = cgenApply ctx meta expr args
cgen ctx (S.Closure _ funcName enclosedVars) = do
    modState <- gets moduleState
    let mapping = functions modState
    boxClosure funcName mapping enclosedVars
cgen ctx m@S.Match{} =
    error $ printf "Match expressions should be already desugared! %s at: %s" (show m) (show $ S.exprPosition m)
cgen ctx (S.If meta cond tr fl) = cgenIfStatic ctx meta cond tr fl

cgen ctx e = error ("cgen shit " ++ show e)

cgenIfStatic ctx meta cond tr fl = do
    let resultType = llvmTypeOf tr
    let test = do
            cond <- cgen ctx cond
            -- unbox Bool
            bool <- unboxBool cond
            instr (I.ICmp IP.EQ bool constTrue [])
    cgenIf resultType test (cgen ctx tr) (cgen ctx fl)

cgenSelect ctx this@(S.Select meta tree expr) = do
    --    Debug.traceM $ printf "Selecting! %s" (show this)
    let (treeType, tpeName) = case S.typeOf tree of
                                  treeType@(TypeIdent tpeName) -> (treeType, tpeName)
                                  treeType@(TypeApply (TypeIdent tpeName) _) -> (treeType, tpeName)
                                  treeType -> error $ printf "Unsupported type for selection %s" (show treeType)
    let identType = S.typeOf expr
    let expectedReturnType = S.typeOf this
    --    Debug.traceM $ printf "Selecting %s: %s" (show treeType) (show identType)
    case expr of
       (S.Ident _ fieldName) | dataTypeHasField ctx tpeName fieldName -> do
           let pos = createPosition $ S.pos meta
           tree <- cgen ctx tree
           let (S.Ident _ fieldName) = expr
           let fieldsWithIndex = (S._dataDefsFields ctx) Map.! tpeName
    --            Debug.traceM $ printf "fieldsWithIndex %s" (show fieldsWithIndex)
           let (S.Arg n declaredFieldType, idx) = fromMaybe (error $ printf "No such field %s in %s" (show fieldName) (show tpeName)) (Map.lookup fieldName fieldsWithIndex)
           let len = length fieldsWithIndex
           dataStruct <- bitcast tree (T.ptr (dataValueStructType len))
           array <- getelementptr dataStruct [constIntOp 0, constInt32Op 2]
           valueAddr <- getelementptr array [constIntOp 0, constIntOp idx]
           load valueAddr
    --            traceM $ printf "AAAA %s: %s" (show array) (show value)
    --            resultValue <- castBoxedValue declaredFieldType value
    --            Debug.traceM $ printf "Selecting %s: %s" (show tree) (show resultValue)
    --            return $ constFloatOp 1234.5
    --            resolveBoxing declaredFieldType expectedReturnType resultValue
    --            return resultValue
        --    return value
       (S.Ident _ name) | isFuncType identType -> do
    --      traceM $ printf "Method call %s: %s" name (show identType)
           cgen ctx (S.Apply meta expr [tree])
       _ -> error $ printf "Unsupported select: %s at %s" (show this) (show $ S.pos meta)
cgenSelect ctx e = error ("cgenSelect should only be called on Select, but called on" ++ show e)

cgenApplyUnOp ctx this@(S.Apply meta op@(S.Ident _ "unary-") [expr]) = do
    lexpr' <- cgen ctx expr
    let (TypeFunc realExprType _) = S.typeOf op
    lexpr <- resolveBoxing anyTypeVar realExprType lexpr'
    let returnType = S.typeOf this
    let tpe = S.typeOf expr
    let minus = fromMaybe (error ("Only Byte | Int | Int16 | Int32 | Float supported but given " ++ show tpe)) (getArithOp 11 tpe)
    let llvmType = externalTypeMapping tpe
    let zero = case tpe of
            TypeByte -> C.Int 8 0
            TypeInt16 -> C.Int 16 0
            TypeInt32 -> C.Int 32 0
            TypeInt -> C.Int 64 0
            TypeFloat -> C.Float (F.Double 0.0)
            _ -> error $ printf "%s: Unexpected type in unaryOp - %s" (S.showPosition meta) (show tpe)
    r <- instrTyped llvmType $ (constOp zero) `minus` lexpr
    resolveBoxing returnType anyTypeVar r
cgenApplyUnOp ctx e = error ("cgenApplyUnOp should only be called on Apply, but called on" ++ show e)

getArithOp code tpe = case (code, tpe) of
    (10, _) | isIntegralType tpe -> Just $ \lhs rhs -> I.Add False False lhs rhs []
    (10, TypeFloat) -> Just $ \lhs rhs -> I.FAdd I.NoFastMathFlags lhs rhs []
    (11, _) | isIntegralType tpe -> Just $ \lhs rhs -> I.Sub False False lhs rhs []
    (11, TypeFloat) -> Just $ \lhs rhs -> I.FSub I.NoFastMathFlags lhs rhs []
    (12, _) | isIntegralType tpe -> Just $ \lhs rhs -> I.Mul False False lhs rhs []
    (12, TypeFloat) -> Just $ \lhs rhs -> I.FMul I.NoFastMathFlags lhs rhs []
    (13, _) | isIntegralType tpe -> Just $ \lhs rhs -> I.SDiv True lhs rhs []
    (13, TypeFloat) -> Just $ \lhs rhs -> I.FDiv I.NoFastMathFlags lhs rhs []
    _ -> Nothing

getCmpOp code tpe = case (code, tpe) of
    (42, _) | isIntegralType tpe -> Just $ \lhs rhs -> I.ICmp IPred.EQ lhs rhs []
    (42, TypeFloat) -> Just $ \lhs rhs -> I.FCmp FP.OEQ lhs rhs []
    (43, _) | isIntegralType tpe -> Just $ \lhs rhs -> I.ICmp IPred.NE lhs rhs []
    (43, TypeFloat) -> Just $ \lhs rhs -> I.FCmp FP.ONE lhs rhs []
    (44, _) | isIntegralType tpe -> Just $ \lhs rhs -> I.ICmp IPred.SLT lhs rhs []
    (44, TypeFloat) -> Just $ \lhs rhs -> I.FCmp FP.OLT lhs rhs []
    (45, _) | isIntegralType tpe -> Just $ \lhs rhs -> I.ICmp IPred.SLE lhs rhs []
    (45, TypeFloat) -> Just $ \lhs rhs -> I.FCmp FP.OLE lhs rhs []
    (46, _) | isIntegralType tpe -> Just $ \lhs rhs -> I.ICmp IPred.SGE lhs rhs []
    (46, TypeFloat) -> Just $ \lhs rhs -> I.FCmp FP.OGE lhs rhs []
    (47, _) | isIntegralType tpe -> Just $ \lhs rhs -> I.ICmp IPred.SGT lhs rhs []
    (47, TypeFloat) -> Just $ \lhs rhs -> I.FCmp FP.OGT lhs rhs []
    _ -> Nothing

cgenApplyBinOp ctx this@(S.Apply meta op@(S.Ident _ fn) [lhs, rhs]) = do
    llhs' <- cgen ctx lhs
    lrhs' <- cgen ctx rhs
    let lhsType = S.typeOf lhs
    let rhsType = S.typeOf rhs
    let returnType = S.typeOf this
--    Debug.traceM $ printf "Doing binop %s with type %s" (show this) (show $ S.typeOf op)
    let (realLhsType, realRhsType) = case S.typeOf op of
                TypeFunc realLhsType (TypeFunc realRhsType _) -> (realLhsType, realRhsType)
                _ -> error ("cgenApplyBinOp: Should not happen: " ++ show this ++ show (S.typeOf op))
    llhs <- resolveBoxing anyTypeVar realLhsType llhs'
    lrhs <- resolveBoxing anyTypeVar realRhsType lrhs'
    let code = fromMaybe (error ("Couldn't find binop " ++ show fn)) (Map.lookup fn binops)
--    Debug.traceM $ printf "%s: %s <==> %s: %s, code %s" (show lhsType) (show realLhsType) (show rhsType) (show realRhsType) (show code)
    let llvmType = externalTypeMapping realLhsType
    res <- case code of
        _ | code >= 10 && code <= 13 -> do
            let op = fromMaybe (error $ printf "cgenApplyBinOp not defined operation code %d for type %s" code (show realLhsType)) (getArithOp code realLhsType)
            instrTyped llvmType (llhs `op` lrhs)
        _ | code >= 42 && code <= 47 -> do
            let op = fromMaybe (error $ printf "cgenApplyBinOp not defined operation code %d for type %s" code (show realLhsType)) (getCmpOp code realLhsType)
            r <- instrTyped llvmType (llhs `op` lrhs)
            instrTyped boolType $ I.ZExt r boolType []
        c  -> error $ printf "%s: Unsupported binary operation %s, code %s, type %s" (show $ S.exprPosition this) (S.printExprWithType this) (show c) (show realLhsType)
    resolveBoxing returnType anyTypeVar res
cgenApplyBinOp ctx e = error ("cgenApplyBinOp should only be called on Apply, but called on" ++ show e)

cgenApply ctx meta expr args = do
    syms <- gets symtab
    let symMap = Map.fromList syms
    let isGlobal fn = (fn `Map.member` S._globalFunctions ctx) && not (fn `Map.member` symMap)
    case expr of
         -- FIXME Here are BUGZZZZ!!!! :)
        this@(S.Ident meta (NS "Array" "getIndex")) -> do
            let [arrayExpr, indexExpr] = args
            array <- cgen ctx arrayExpr -- should be a pointer to either boxed or unboxed array
            boxedIdx <- cgen ctx indexExpr
            idx <- unboxInt boxedIdx
--            callFn (funcType ptrType [ptrType, intType]) "arrayGetIndex" [array, idx]
            cgenArrayApply array idx
                    
        S.Ident _ fn | isGlobal fn -> do
--            Debug.traceM $ printf "Calling %s" fn
            let f = S._globalFunctions ctx Map.! fn
            largs <- forM args $ \arg -> cgen ctx arg
            callFn (funcLLvmType f) (show fn) largs
        expr -> do
            -- closures
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

cgenArrayApply array idx = do
    arrayStructPtr <- bitcast array (T.ptr $ arrayStructType ptrType) -- &Array(type, len, &data[])
    -- TODO check idx is in bounds, eliminatable
    ptr <- getelementptr arrayStructPtr [constIntOp 0, constInt32Op 2, idx]
    load ptr

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

funcPtrFromClosure closure = do
    modState <- gets moduleState
    let mapping = functions modState
    let len = Map.size mapping
    closureTyped <- bitcast closure (T.ptr closureStructType)
    idxPtr <- getelementptr closureTyped [constIntOp 0, constInt32Op 1]
    idx <- instrTyped intType $ I.Load False idxPtr Nothing 0 []
--    callFn "putInt" [idx]
    let fst = functionsStructType (fromIntegral len)
    let fnsAddr = (globalOp fst (nameToSBS "Functions"))
    fns <- instrTyped (T.ptr fst) $ I.Load False fnsAddr Nothing 0 []
--    sizeAddr <- getelementptr fnsAddr [constIntOp 0, constIntOp 0]
--    size <- instrTyped intType $ I.Load False sizeAddr Nothing 0 []
--     Functions[idx].funcPtr
    fnPtr <- getelementptr fnsAddr [constIntOp 0, constInt32Op 1, idx, constInt32Op 1]
    load fnPtr

resolveBoxing declaredType instantiatedType expr =
    case (declaredType, instantiatedType) of
        _ | declaredType == instantiatedType -> return expr
        (TypeByte, TVar _) -> boxByte expr
        (TypeBool, TVar _) -> boxBool expr
        (TypeInt, TVar _) -> boxInt expr
        (TypeInt16, TVar _) -> boxInt16 expr
        (TypeInt32, TVar _) -> boxInt32 expr
        (TypeFloat, TVar _) -> boxFloat64 expr
        (TVar _, TypeByte) -> unboxByte expr
        (TVar _, TypeBool) -> unboxBool expr
        (TVar _, TypeInt) -> unboxInt expr
        (TVar _, TypeInt16) -> unboxInt16 expr
        (TVar _, TypeInt32) -> unboxInt32 expr
        (TVar _, TypeFloat) -> unboxFloat64 expr
        (TVar _, TVar _) -> return expr
        (l, r) -> do
--            Debug.traceM $ printf "resolveBoxing crap %s %s" (show l) (show r)
            return expr
{-# INLINE resolveBoxing #-}


