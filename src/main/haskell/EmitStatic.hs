{-# LANGUAGE Strict #-}

module EmitStatic (
  codegenStaticModule
) where

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
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Debug.Trace as Debug

import Codegen
import Type
import Emit
import qualified Syntax as S
import Syntax (Ctx, createGlobalContext)


staticArgsToSig :: [S.Arg] -> [(SBS.ShortByteString, AST.Type)]
staticArgsToSig = map (\(S.Arg name tpe) -> (fromString name, typeMapping tpe))

argToPtr (S.Arg n t) = case t of
    TypeIdent "Float" -> fptoptr $ local T.double (fromString n)
    TypeIdent "Int"   -> inttoptr $ local T.i32 (fromString n)
    _                 -> return $ localPtr $ fromString n

-- codegenTop :: S.Expr -> LLVM ()
codegenTop ctx this@(S.Val meta name expr) = do
    modify (\s -> s { _globalValsInit = _globalValsInit s ++ [(name, expr)] })
    let valType = llvmTypeOf this
    defineGlobal (fromString name) valType (Just $ defaultValueForType valType)

codegenTop ctx f@(S.Function meta name tpe args body) =
    if meta ^. S.isExternal then
        external (typeMapping tpe) (fromString name ) (externArgsToSig args) False []
    else do
        modState <- get
        let codeGenResult = codeGen modState
        let blocks = createBlocks codeGenResult
        mapM_ defineStringLit (generatedStrings codeGenResult)
        let retType = mappedReturnType args funcType
        define retType (fromString name) largs blocks
  where
    funcType = S.typeOf f
    largs = map (\(n, t) -> (fromString n, t)) argsWithTypes

    funcTypeToLlvm (S.Arg name _) (TypeFunc a b, acc) = (b, (name, typeMapping a) : acc)
    funcTypeToLlvm arg t = error $ "AAA3" ++ show arg ++ show t

    argsWithTypes = do
--        Debug.traceM $ printf "codegenTop %s(%s): %s" (name) (show args) (show funcType)
        reverse $ snd $ foldr funcTypeToLlvm (funcType, []) (reverse args)
    codeGen modState = execCodegen [] modState $ do
  --      Debug.traceM $ printf "argsWithTypes %s" (show argsWithTypes)
        entry <- addBlock entryBlockName
        setBlock entry
        forM_ argsWithTypes $ \(n, t) -> do
            var <- alloca t
            store var (local t (fromString n))
    --        Debug.traceM $ printf "assign %s: %s = %s" n (show t) (show var)
            assign n var
        cgen ctx body >>= ret

codegenTop ctx (S.Data _ name tvars constructors) = return ()

codegenTop ctx expr =
    error $ printf "Expression of this kind should not get to codegenTop. It's a bug. %s at %s"
        (show expr) (show $ S.exprPosition expr)

typeMapping :: Type -> AST.Type
typeMapping t = case t of
                  TypeIdent "Int" -> T.i32
                  TypeIdent "Float" -> T.double
--                  TypeIdent "Unit" -> T.void
                  _                -> ptrType

llvmTypeOf = typeMapping . S.typeOf
mappedReturnType args t = typeMapping $ declaredReturnType args t
declaredReturnType args t = foldr f t args
  where f arg (TypeFunc a b) = b
        f arg t = t
-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

dataTypeHasField ctx typeName fieldName =
    typeName `Set.member` (S.dataDefsNames ctx) && fieldName `Map.member` (S.dataDefsFields ctx Map.! typeName)

isDataType ctx tpe = case tpe of
    TypeIdent name | name `Set.member` (S.dataDefsNames ctx) -> True
    _ -> False

isFuncType (TypeFunc _ _) = True
isFuncType _ = False

lascaPrimitiveTypes = Set.fromList [TypeIdent "Int", TypeIdent "Float", TypeIdent "Bool", TypeIdent "String", TypeIdent "Any", TypeIdent "Unit"]

cgen :: Ctx -> S.Expr -> Codegen AST.Operand
cgen ctx (S.Let meta a b c) = do
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
        Nothing | name `Map.member` S._globalFunctions ctx -> boxFunc name mapping
                | name `Set.member` S._globalVals ctx -> load (global ptrType (fromString name))
                | otherwise -> boxError name
cgen ctx (S.Literal meta l) = do
--  Debug.traceM $ "Generating literal " ++ show l ++ " on " ++ show (S.pos meta)
    case l of
        S.IntLit i -> return $ constIntOp i
        S.FloatLit i -> return $ constFloatOp i
        _        -> box l meta
cgen ctx this@(S.Array meta exprs) = do
    vs <- sequence [cgen ctx e | e <- exprs]
    case S.typeOf this of
       TypeApply (TypeIdent "Array") [TypeIdent "Float"] -> do
           let len = length vs
           let arrayType = T.StructureType False [T.i32, T.ArrayType (fromIntegral len) T.double]
           (ptr, arrayPtr) <- gcMallocType arrayType
           sizePtr <- getelementptr arrayPtr [constIntOp 0, constIntOp 0]
           store sizePtr (constIntOp len)
           forM_ (zip vs [0..]) $ \(v, i) -> do
               p <- getelementptr arrayPtr [constIntOp 0, constIntOp 1, constIntOp i] -- [dereference, ith element]
               store p v
           return ptr
       _ -> boxArray vs
cgen ctx this@(S.Select meta tree expr) = do
--    Debug.traceM $ printf "Selecting! %s" (show this)
    let (treeType, tpeName) = case S.typeOf tree of
                                   treeType@(TypeIdent tpeName) -> (treeType, tpeName)
                                   treeType@(TypeApply (TypeIdent tpeName) _) -> (treeType, tpeName)
                                   treeType -> error $ printf "Unsupported type for selection %s" (show treeType)
    let identType = S.typeOf expr
--    Debug.traceM $ printf "Selecting %s: %s" (show treeType) (show identType)
    case expr of
        (S.Ident _ fieldName) | dataTypeHasField ctx tpeName fieldName -> do
            let pos = createPosition $ S.pos meta
            tree <- cgen ctx tree
            let (S.Ident _ fieldName) = expr
            let fieldsWithIndex = (S.dataDefsFields ctx) Map.! tpeName
--            Debug.traceM $ printf "fieldsWithIndex %s" (show fieldsWithIndex)
            let (S.Arg n t, idx) = fromMaybe (error $ printf "No such field %s in %s" fieldName tpeName) (Map.lookup fieldName fieldsWithIndex)
            let len = length fieldsWithIndex
            let arrayType = T.ArrayType (fromIntegral len) ptrType
            let tpe = T.StructureType False [T.i32, arrayType] -- DataValue: {tag, values: []}

            boxedTree <- bitcast tree (T.ptr boxStructType)
            
            unboxedAddr <- getelementptr boxedTree [constIntOp 0, constIntOp 1]
            unboxed <- load unboxedAddr

            dataStruct <- bitcast unboxed (T.ptr tpe)
            array <- getelementptr dataStruct [constIntOp 0, constIntOp 1]
            valueAddr <- getelementptr array [constIntOp 0, constIntOp idx]
            value <- load valueAddr
--            traceM $ printf "AAAA %s: %s" (show array) (show value)
            resultValue <- case t of
                TypeIdent "Float" -> ptrtofp value
                TypeIdent "Int"   -> ptrtoint value T.i32
                _                 -> return value
--            Debug.traceM $ printf "Selecting %s: %s" (show tree) (show resultValue)
--            return $ constFloatOp 1234.5
            return resultValue
        (S.Ident _ name) | isFuncType identType -> do
    --      traceM $ printf "Method call %s: %s" name (show identType)
            cgen ctx (S.Apply meta expr [tree])
        _ -> error $ printf "Unsupported select: %s at %s" (show this) (show $ S.pos meta)

cgen ctx (S.Apply meta (S.Ident _ "or") [lhs, rhs]) = cgen ctx (S.If meta lhs (S.Literal S.emptyMeta (S.BoolLit True)) rhs)
cgen ctx (S.Apply meta (S.Ident _ "and") [lhs, rhs]) = cgen ctx (S.If meta lhs rhs (S.Literal S.emptyMeta (S.BoolLit False)))
cgen ctx (S.Apply meta (S.Ident _ fn) [lhs, rhs]) | fn `Map.member` binops = do
    llhs <- cgen ctx lhs
    lrhs <- cgen ctx rhs
    let lhsType = S.typeOf lhs
    let code = fromMaybe (error ("Couldn't find binop " ++ fn)) (Map.lookup fn binops)
    case (code, lhsType) of
        (10, TypeIdent "Int") -> add llhs lrhs
        (11, TypeIdent "Int") -> sub llhs lrhs
        (12, TypeIdent "Int") -> mul llhs lrhs
        (13, TypeIdent "Int") -> Codegen.div llhs lrhs
        (42, TypeIdent "Int") -> do
          bool <- intEq llhs lrhs
          callFn "boxBool" [bool]
        (44, TypeIdent "Int") -> do
          bool <- intLt llhs lrhs
          callFn "boxBool" [bool]
        (47, TypeIdent "Int") -> do
          bool <- intGt llhs lrhs
          callFn "boxBool" [bool]
        (10, TypeIdent "Float") -> fadd llhs lrhs
        (11, TypeIdent "Float") -> fsub llhs lrhs
        (12, TypeIdent "Float") -> fmul llhs lrhs
        (13, TypeIdent "Float") -> fdiv llhs lrhs
        _  -> do
                let codeOp = constIntOp code
                callFn "runtimeBinOp" [codeOp, llhs, lrhs]
cgen ctx (S.Apply meta expr args) = do
    syms <- gets symtab
    let symMap = Map.fromList syms
    let isGlobal fn = (fn `Map.member` S._globalFunctions ctx) && not (fn `Map.member` symMap)
    let isArray expr = case S.typeOf expr of
                         TypeApply (TypeIdent "Array") _ -> True
                         _ -> False
    let isIntType expr = case S.typeOf expr of
                            TypeIdent "Int" -> True
                            _ -> False
    case expr of
         -- FIXME Here are BUGZZZZ!!!! :)
        this@(S.Ident meta "arrayApply") -> do
            let [arrayExpr, indexExpr] = args
            array <- cgen ctx arrayExpr -- should be a pointer to either boxed or unboxed array
            idx <- cgen ctx indexExpr -- must be T.i32. TODO should be platform-dependent
            case S.typeOf arrayExpr of
                TypeApply (TypeIdent "Array") [TypeIdent "Float"] -> do
                    arrayStructPtr <- bitcast array (T.ptr (arrayStructType T.double))
                    arrayPtr <- getelementptr arrayStructPtr [constIntOp 0, constIntOp 1]
                    arrayPtr1 <- bitcast arrayPtr (T.ptr T.double)
                    ptr <- getelementptr arrayPtr1 [idx]
                    load ptr
                _ -> do
                    boxedArrayPtr <- bitcast array (T.ptr $ boxStructOfType (T.ptr $ arrayStructType ptrType)) -- Box(type, &Array(len, &data[])
                    arrayStructAddr <- getelementptr boxedArrayPtr [constInt64Op 0, constIntOp 1]
                    arrayStructPtr <- load arrayStructAddr
                    arraysize <- getelementptr arrayStructPtr [constInt64Op 0, constIntOp 0]
                    size <- load arraysize
                    -- TODO check idx is in bounds, eliminatable
                    arrayDataAddr <- getelementptr arrayStructPtr [constInt64Op 0, constIntOp 1]
                    arraDataPtr <- load arrayDataAddr
                    arrayDataArray <- bitcast arraDataPtr (T.ptr (T.ArrayType 0 ptrType))
                    ptr' <- getelementptr arrayDataArray [constInt64Op 0, idx]
                    load ptr'
        this@(S.Ident meta fn) | isGlobal fn -> do
            let funDecl = (ctx ^. S.globalFunctions) Map.! fn
            let fnType = S.typeOf funDecl
            let declaredTypes = typeToList fnType
            let paramTypes = init declaredTypes
            let declaredReturnType = last declaredTypes
            let expectedTypes = typeToList $ S.typeOf this
            let expectedReturnType = last expectedTypes
            Debug.traceM $ printf "Calling %s: %s, expected %s" fn (show declaredReturnType) (show expectedReturnType)
            largs <- forM (zip args paramTypes) $ \(arg, paramType) -> do
                a <- cgen ctx arg
                let argType = S.typeOf arg
                case (paramType, argType) of
                    (TypeIdent l, TypeIdent r) | l == r -> return a
                    (TVar _, TypeIdent "Int") -> do
--                        Debug.traceM ("boxing " ++ show a)
                        callFn "boxInt" [a]
                    (TVar _, TypeIdent "Float") -> do
--                        Debug.traceM ("boxing " ++ show a)
                        callFn "boxFloat64" [a]
                    _ -> return a
            case (expectedReturnType, declaredReturnType) of
                _ | expectedReturnType == declaredReturnType -> callFn fn largs
                (TypeIdent "Int", TVar _) -> do
                    result <- callFn fn largs
                    callFn "unboxInt" [result]
                (TypeIdent "Float", TVar _) -> do
                    result <- callFn fn largs
                    callFn "unboxFloat64" [result]
                _ -> callFn fn largs
        expr -> do
            modState <- gets moduleState
            e <- cgen ctx expr
            largs <- mapM (cgen ctx) args
            let funcs = functions modState
            let argc = constIntOp (length largs)
            let len = Map.size funcs
            sargsPtr <- allocaSize ptrType argc
            let asdf (idx, arg) = do
                  p <- getelementptr sargsPtr [idx]
                  store p arg
            sargs <- bitcast sargsPtr ptrType -- runtimeApply accepts i8*, so need to bitcast. Remove when possible
            -- cdecl calling convension, arguments passed right to left
            sequence_ [asdf (constIntOp i, a) | (i, a) <- zip [0 .. len] largs]
            let pos = createPosition $ S.pos meta
            callFn "runtimeApply" [e, argc, sargs, constOp pos]
cgen ctx (S.BoxFunc _ funcName enclosedVars) = do
    modState <- gets moduleState
    let mapping = functions modState
    if null enclosedVars then boxFunc funcName mapping
    else boxClosure funcName mapping enclosedVars
cgen ctx m@S.Match{} =
    error $ printf "Match expressions should be already desugared! %s at: %s" (show m) (show $ S.exprPosition m)
cgen ctx (S.If meta cond tr fl) = do
    let resultType = llvmTypeOf tr
    ifthen <- addBlock "if.then"
    ifelse <- addBlock "if.else"
    ifexit <- addBlock "if.exit"
    -- %entry
    ------------------
    cond <- cgen ctx cond
    -- unbox Bool
    voidPtrCond <- callFn "unbox" [constIntOp 1, cond]
    bool <- ptrtoint voidPtrCond T.i1

    test <- instr (I.ICmp IP.EQ bool constTrue [])
    cbr test ifthen ifelse -- Branch based on the condition

    -- if.then
    ------------------
    setBlock ifthen
    trval <- cgen ctx tr       -- Generate code for the true branch
    br ifexit              -- Branch to the merge block
    ifthen <- getBlock

    -- if.else
    ------------------
    setBlock ifelse
    flval <- cgen ctx fl       -- Generate code for the false branch
    br ifexit              -- Branch to the merge block
    ifelse <- getBlock

    -- if.exit
    ------------------
    setBlock ifexit
    phi resultType [(trval, ifthen), (flval, ifelse)]

cgen ctx e = error ("cgen shit " ++ show e)


-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

codegenStaticModule :: S.LascaOpts -> AST.Module -> [S.Expr] -> AST.Module
codegenStaticModule opts modo exprs = modul
  where
    ctx = createGlobalContext opts exprs
    desugared = desugarExprs ctx exprs
    modul = runLLVM modo genModule
    genModule = do
        declareStdFuncs
        genFunctionMap desugared
        let defs = reverse (S.dataDefs ctx)
        genTypesStruct ctx defs
        genRuntime opts
        forM_ desugared $ \expr -> do
            defineStringConstants expr
            codegenTop ctx expr
        codegenStartFunc ctx cgen

genTypesStruct ctx defs = do
    types <- genData ctx defs staticArgsToSig argToPtr
    let array = C.Array ptrType types
    defineConst "Types" structType (struct array)
  where len = length defs
        struct a = createStruct [constInt len, a]
        structType = T.StructureType False [T.i32, T.ArrayType (fromIntegral len) ptrType]
