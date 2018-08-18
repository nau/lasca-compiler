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
lascaUnboxedTypes =  Set.fromList [TypeInt, TypeFloat]
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
                | name `Map.member` S._globalVals ctx -> load (global ptrType (nameToSBS name))
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
            voidPtrCond <- unboxDirect cond
            bool <- ptrtoint voidPtrCond T.i1
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
           let arrayType = T.ArrayType (fromIntegral len) ptrType
           let tpe = T.StructureType False [intType, arrayType] -- DataValue: {tag, values: []}

           boxedTree <- bitcast tree (T.ptr boxStructType)

           unboxedAddr <- getelementptr boxedTree [constIntOp 0, constInt32Op 1]
           unboxed <- load unboxedAddr

           dataStruct <- bitcast unboxed (T.ptr tpe)
           array <- getelementptr dataStruct [constIntOp 0, constInt32Op 1]
           valueAddr <- getelementptr array [constIntOp 0, constIntOp idx]
           value <- load valueAddr
    --            traceM $ printf "AAAA %s: %s" (show array) (show value)
    --            resultValue <- castBoxedValue declaredFieldType value
    --            Debug.traceM $ printf "Selecting %s: %s" (show tree) (show resultValue)
    --            return $ constFloatOp 1234.5
    --            resolveBoxing declaredFieldType expectedReturnType resultValue
    --            return resultValue
           return value
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
    res <- case S.typeOf expr of
        TypeInt   -> sub (constIntOp 0) lexpr >>= resolveBoxing returnType anyTypeVar
        TypeFloat -> fsub (constFloatOp 0.0) lexpr >>= resolveBoxing returnType anyTypeVar
        t -> error ("Only TypeInt | TypeFloat supported but given " ++ show t)
--    Debug.traceM $ printf "Doing unary- %s with type %s" (show this) (show $ S.typeOf op)
    return res
cgenApplyUnOp ctx e = error ("cgenApplyUnOp should only be called on Apply, but called on" ++ show e)


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
--    let realLhsType = TypeInt
--    let realRhsType = TypeInt
    llhs <- resolveBoxing anyTypeVar realLhsType llhs'
    lrhs <- resolveBoxing anyTypeVar realRhsType lrhs'
    let code = fromMaybe (error ("Couldn't find binop " ++ show fn)) (Map.lookup fn binops)
--    Debug.traceM $ printf "%s: %s <==> %s: %s, code %s" (show lhsType) (show realLhsType) (show rhsType) (show realRhsType) (show code)
    res <- case (code, realLhsType) of
        (10, TypeInt) -> add llhs lrhs >>= resolveBoxing returnType anyTypeVar
        (11, TypeInt) -> sub llhs lrhs >>= resolveBoxing returnType anyTypeVar
        (12, TypeInt) -> mul llhs lrhs >>= resolveBoxing returnType anyTypeVar
        (13, TypeInt) -> Codegen.div llhs lrhs >>= resolveBoxing returnType anyTypeVar
        (42, TypeInt) -> intCmpBoxed IPred.EQ llhs lrhs
        (43, TypeInt) -> intCmpBoxed IPred.NE llhs lrhs
        (44, TypeInt) -> intCmpBoxed IPred.SLT llhs lrhs
        (45, TypeInt) -> intCmpBoxed IPred.SLE llhs lrhs
        (46, TypeInt) -> intCmpBoxed IPred.SGE llhs lrhs
        (47, TypeInt) -> intCmpBoxed IPred.SGT llhs lrhs
        (10, TypeFloat) -> fadd llhs lrhs >>= resolveBoxing returnType anyTypeVar
        (11, TypeFloat) -> fsub llhs lrhs >>= resolveBoxing returnType anyTypeVar
        (12, TypeFloat) -> fmul llhs lrhs >>= resolveBoxing returnType anyTypeVar
        (13, TypeFloat) -> fdiv llhs lrhs >>= resolveBoxing returnType anyTypeVar
        (c, t)  -> error $ printf "%s: Unsupported binary operation %s, code %s, type %s" (show $ S.exprPosition this) (S.printExprWithType this) (show c) (show t)
    return res
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
    boxedArrayPtr <- bitcast array (T.ptr $ boxStructOfType (T.ptr $ arrayStructType ptrType)) -- Box(type, &Array(len, &data[])
    arrayStructAddr <- getelementptr boxedArrayPtr [constIntOp 0, constInt32Op 1]
    arrayStructPtr <- load arrayStructAddr
    -- TODO check idx is in bounds, eliminatable
    ptr <- getelementptr arrayStructPtr [constIntOp 0, constInt32Op 1, idx]
    load ptr

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

funcPtrFromClosure closure = do
    modState <- gets moduleState
    let mapping = functions modState
    let len = Map.size mapping
    closureTyped <- bitcast closure (T.ptr closureStructType)
    idxPtr <- getelementptr closureTyped [constIntOp 0, constInt32Op 0]
    idx <- instrTyped intType $ I.Load False idxPtr Nothing 0 []
--    callFn "putInt" [idx]
    let fst = functionsStructType (fromIntegral len)
    let fnsAddr = (global fst (nameToSBS "Functions"))
    fns <- instrTyped (T.ptr fst) $ I.Load False fnsAddr Nothing 0 []
--    sizeAddr <- getelementptr fnsAddr [constIntOp 0, constIntOp 0]
--    size <- instrTyped intType $ I.Load False sizeAddr Nothing 0 []
--     Functions[idx].funcPtr
    fnPtr <- getelementptr fnsAddr [constIntOp 0, constInt32Op 1, idx, constInt32Op 1]
    load fnPtr

castBoxedValue declaredType value = case declaredType of
    TypeFloat -> ptrtofp value
    TypeInt   -> ptrtoint value intType
    TypeByte  -> ptrtoint value T.i8
    _                 -> return value
{-# INLINE castBoxedValue #-}

unboxDirect expr = do
    boxed <- bitcast expr (T.ptr boxStructType)
    unboxedAddr <- getelementptr boxed [constIntOp 0, constInt32Op 1]
    load unboxedAddr
{-# INLINE unboxDirect #-}

unboxByte expr = do
    unboxed <- unboxDirect expr
    castBoxedValue TypeByte unboxed

unboxBool expr = do
    unboxed <- unboxDirect expr
    castBoxedValue TypeInt unboxed


unboxInt expr = do
    unboxed <- unboxDirect expr
    castBoxedValue TypeInt unboxed
{-# INLINE unboxInt #-}

unboxFloat64 expr = do
    unboxed <- unboxDirect expr
    castBoxedValue TypeFloat unboxed
{-# INLINE unboxFloat64 #-}

resolveBoxing declaredType instantiatedType expr =
    case (declaredType, instantiatedType) of
        _ | declaredType == instantiatedType -> return expr
        (TypeByte, TVar _) -> boxByte expr
        (TypeBool, TVar _) -> boxBool expr
        (TypeInt, TVar _) -> boxInt expr
        (TypeFloat, TVar _) -> boxFloat64 expr
        (TVar _, TypeByte) -> unboxByte expr
        (TVar _, TypeBool) -> unboxBool expr
        (TVar _, TypeInt) -> unboxInt expr
        (TVar _, TypeFloat) -> unboxFloat64 expr
        (TVar _, TVar _) -> return expr
        (l, r) -> do
--            Debug.traceM $ printf "resolveBoxing crap %s %s" (show l) (show r)
            return expr
{-# INLINE resolveBoxing #-}


