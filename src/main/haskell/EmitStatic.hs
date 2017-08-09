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
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Debug.Trace as Debug
import System.Exit

import Codegen
import Type
import Emit
import Infer
import qualified Syntax as S
import Syntax (Ctx, createGlobalContext)


staticArgsToSig :: [S.Arg] -> [(SBS.ShortByteString, AST.Type)]
staticArgsToSig = map (\(S.Arg name tpe) -> (nameToSBS name, typeMapping tpe))

argToPtr (S.Arg n t) = case t of
--    TypeIdent "Float" -> fptoptr $ local T.double (nameToSBS n)
--    TypeIdent "Int"   -> inttoptr $ local T.i32 (nameToSBS n)
    _                 -> return $ localPtr $ nameToSBS n

-- codegenTop :: S.Expr -> LLVM ()
codegenTop ctx this@(S.Val meta name expr) = do
    modify (\s -> s { _globalValsInit = _globalValsInit s ++ [(name, expr)] })
    let valType = llvmTypeOf this
    defineGlobal (nameToSBS name) valType (Just $ defaultValueForType valType)

codegenTop ctx f@(S.Function meta name tpe args body) =
    if meta ^. S.isExternal then
        external (externalTypeMapping tpe) (nameToSBS name ) (externArgsToSig args) False []
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
--        Debug.traceM $ printf "codegenTop %s(%s): %s" (name) (show args) (show funcType)
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

codegenTop ctx (S.Data _ name tvars constructors) = return ()

codegenTop ctx expr =
    error $ printf "Expression of this kind should not get to codegenTop. It's a bug. %s at %s"
        (show expr) (show $ S.exprPosition expr)

typeMapping :: Type -> AST.Type
typeMapping t = case t of
--                  TypeIdent "Int" -> T.i32
--                  TypeIdent "Float" -> T.double
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
lascaUnboxedTypes =  Set.fromList [TypeIdent "Int", TypeIdent "Float"]
anyTypeVar = TVar $ TV "a"

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
                | name `Set.member` S._globalVals ctx -> load (global ptrType (nameToSBS name))
                | otherwise -> boxError (show name)
cgen ctx (S.Literal meta l) = do
--  Debug.traceM $ "Generating literal " ++ show l ++ " on " ++ show (S.pos meta)
    box l meta
cgen ctx this@(S.Array meta exprs) = do
    vs <- sequence [cgen ctx e | e <- exprs]
    boxArray vs
cgen ctx this@(S.Select meta tree expr) = do
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
            let fieldsWithIndex = (S.dataDefsFields ctx) Map.! tpeName
--            Debug.traceM $ printf "fieldsWithIndex %s" (show fieldsWithIndex)
            let (S.Arg n declaredFieldType, idx) = fromMaybe (error $ printf "No such field %s in %s" (show fieldName) (show tpeName)) (Map.lookup fieldName fieldsWithIndex)
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

cgen ctx (S.Apply meta (S.Ident _ "or") [lhs, rhs]) = cgen ctx (S.If meta lhs (S.Literal S.emptyMeta (S.BoolLit True)) rhs)
cgen ctx (S.Apply meta (S.Ident _ "and") [lhs, rhs]) = cgen ctx (S.If meta lhs rhs (S.Literal S.emptyMeta (S.BoolLit False)))
cgen ctx this@(S.Apply meta op@(S.Ident _ "unary-") [expr]) = do
    lexpr' <- cgen ctx expr
    let (TypeFunc realExprType _) = S.typeOf op
    lexpr <- resolveBoxing anyTypeVar realExprType lexpr'
    let returnType = S.typeOf this
    res <- case S.typeOf expr of
        TypeIdent "Int"   -> sub (constIntOp 0) lexpr >>= resolveBoxing returnType anyTypeVar
        TypeIdent "Float" -> fsub (constFloatOp 0.0) lexpr >>= resolveBoxing returnType anyTypeVar
--    Debug.traceM $ printf "Doing unary- %s with type %s" (show this) (show $ S.typeOf op)
    return res
cgen ctx this@(S.Apply meta op@(S.Ident _ fn) [lhs, rhs]) | fn `Map.member` binops = do
    llhs' <- cgen ctx lhs
    lrhs' <- cgen ctx rhs
    let lhsType = S.typeOf lhs
    let rhsType = S.typeOf rhs
    let returnType = S.typeOf this
--    Debug.traceM $ printf "Doing binop %s with type %s" (show this) (show $ S.typeOf op)
    let (TypeFunc realLhsType (TypeFunc realRhsType _)) = S.typeOf op
--    let realLhsType = TypeIdent "Int"
--    let realRhsType = TypeIdent "Int"
    llhs <- resolveBoxing anyTypeVar realLhsType llhs'
    lrhs <- resolveBoxing anyTypeVar realRhsType lrhs'
--    Debug.traceM $ printf "%s: %s <==> %s: %s" (show lhsType) (show realLhsType) (show rhsType) (show realRhsType)
    let code = fromMaybe (error ("Couldn't find binop " ++ show fn)) (Map.lookup fn binops)
    res <- case (code, realLhsType) of
        (10, TypeIdent "Int") -> add llhs lrhs >>= resolveBoxing returnType anyTypeVar
        (11, TypeIdent "Int") -> sub llhs lrhs >>= resolveBoxing returnType anyTypeVar
        (12, TypeIdent "Int") -> mul llhs lrhs >>= resolveBoxing returnType anyTypeVar
        (13, TypeIdent "Int") -> Codegen.div llhs lrhs >>= resolveBoxing returnType anyTypeVar
        (42, TypeIdent "Int") -> intCmpBoxed IPred.EQ llhs lrhs
        (43, TypeIdent "Int") -> intCmpBoxed IPred.NE llhs lrhs
        (44, TypeIdent "Int") -> intCmpBoxed IPred.SLT llhs lrhs
        (45, TypeIdent "Int") -> intCmpBoxed IPred.SLE llhs lrhs
        (46, TypeIdent "Int") -> intCmpBoxed IPred.SGE llhs lrhs
        (47, TypeIdent "Int") -> intCmpBoxed IPred.SGT llhs lrhs
        (10, TypeIdent "Float") -> fadd llhs lrhs >>= resolveBoxing returnType anyTypeVar
        (11, TypeIdent "Float") -> fsub llhs lrhs >>= resolveBoxing returnType anyTypeVar
        (12, TypeIdent "Float") -> fmul llhs lrhs >>= resolveBoxing returnType anyTypeVar
        (13, TypeIdent "Float") -> fdiv llhs lrhs >>= resolveBoxing returnType anyTypeVar
        _  -> error $ printf "%s: Unsupported binary operation %s" (show $ S.exprPosition this) (S.printExprWithType this)
    return res
cgen ctx (S.Apply meta expr args) = cgenApply ctx meta expr args
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

cgenApply ctx meta expr args = do
    syms <- gets symtab
    let symMap = Map.fromList syms
    let isGlobal fn = (fn `Map.member` S._globalFunctions ctx) && not (fn `Map.member` symMap)
    let funDecl fn = (ctx ^. S.globalFunctions) Map.! fn
    let isExtern fn = isGlobal fn && (funDecl fn ^. S.metaLens.S.isExternal)
    case expr of
         -- FIXME Here are BUGZZZZ!!!! :)
        this@(S.Ident meta "arrayApply") -> do
            let [arrayExpr, indexExpr] = args
            array <- cgen ctx arrayExpr -- should be a pointer to either boxed or unboxed array
            boxedIdx <- cgen ctx indexExpr -- must be T.i32. TODO should be platform-dependent
            idx <- unboxInt boxedIdx
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
                    
        S.Ident _ fn | isExtern fn -> do
            let (S.Function _ _ returnType externArgs _) = S._globalFunctions ctx Map.! fn
            let argTypes = map (\(S.Arg n t) -> t) externArgs
--            Debug.traceM $ printf "Calling external %s(%s): %s" fn (show argTypes) (show returnType)
            largs <- forM (zip args argTypes) $ \(arg, tpe) -> do
                a <- cgen ctx arg
                resolveBoxing anyTypeVar tpe a
            res <- callFn (show fn) largs
            resolveBoxing returnType anyTypeVar res

        S.Ident _ fn | isGlobal fn -> do
--            Debug.traceM $ printf "Calling %s" fn
            largs <- forM args $ \arg -> cgen ctx arg
            callFn (show fn) largs
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
            callFn "runtimeApply" [e, argc, sargs, constOp pos]

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------
castBoxedValue declaredType value = case declaredType of
    TypeIdent "Float" -> ptrtofp value
    TypeIdent "Int"   -> ptrtoint value T.i32
    _                 -> return value
{-# INLINE castBoxedValue #-}

unboxInt expr = do
    boxed <- bitcast expr (T.ptr boxStructType)
    unboxedAddr <- getelementptr boxed [constIntOp 0, constIntOp 1]
    unboxed <- load unboxedAddr
    castBoxedValue (TypeIdent "Int") unboxed
{-# INLINE unboxInt #-}

unboxFloat64 expr = do
    boxed <- bitcast expr (T.ptr boxStructType)
    unboxedAddr <- getelementptr boxed [constIntOp 0, constIntOp 1]
    unboxed <- load unboxedAddr
    castBoxedValue (TypeIdent "Float") unboxed

resolveBoxing declaredType instantiatedType expr = do
    case (declaredType, instantiatedType) of
        _ | declaredType == instantiatedType -> return expr
        (TypeIdent "Int", TVar _) -> callFn "boxInt" [expr]
        (TypeIdent "Float", TVar _) -> callFn "boxFloat64" [expr]
        (TVar _, TypeIdent "Int") -> unboxInt expr
        (TVar _, TypeIdent "Float") -> unboxFloat64 expr
        (TVar _, TVar _) -> return expr
        (l, r) -> do
--            Debug.traceM $ printf "resolveBoxing crap %s %s" (show l) (show r)
            return expr
{-# INLINE resolveBoxing #-}

codegenStaticModule :: S.LascaOpts -> AST.Module -> [S.Expr] -> IO AST.Module
codegenStaticModule opts modo exprs = do
    let desugared = desugarExprs ctx desugarExpr exprs
    case typeCheck ctx desugared of
        Right (env, typedExprs) -> do
            when (S.verboseMode opts) $ putStrLn "typechecked OK"
            when (S.printTypes opts) $ print env
            let desugared = delambdafy ctx typedExprs
            when (S.printAst opts) $ putStrLn $ List.intercalate "\n" (map S.printExprWithType desugared)
            return $ modul desugared
        Left e -> die $ printTypeError e
  where
    ctx = createGlobalContext opts exprs
    modul exprs = runLLVM modo $ genModule exprs
    genModule exprs = do
        declareStdFuncs
        genFunctionMap exprs
        let defs = reverse (S.dataDefs ctx)
        genTypesStruct ctx defs
        genRuntime opts
        forM_ exprs $ \expr -> do
            defineStringConstants expr
            codegenTop ctx expr
        codegenStartFunc ctx cgen

genTypesStruct ctx defs = do
    types <- genData ctx defs toSig argToPtr
    let array = C.Array ptrType types
    defineConst "Types" structType (struct array)
  where len = length defs
        struct a = createStruct [constInt len, a]
        structType = T.StructureType False [T.i32, T.ArrayType (fromIntegral len) ptrType]
