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
import Debug.Trace as Debug

import Codegen
import Type
import Emit
import qualified Syntax as S
import Syntax (Ctx, createGlobalContext)


externArgsToSig :: [S.Arg] -> [(SBS.ShortByteString, AST.Type)]
externArgsToSig = map (\(S.Arg name tpe) -> (fromString name, typeMapping tpe))

uncurryLambda expr = go expr ([], expr) where
    go (S.Lam _ name e) result = let (args, body) = go e result in (name : args, body)
    go e (args, _) = (args, e)


defaultValueForType tpe =
    case tpe of
        T.FloatingPointType T.DoubleFP -> constFloat 0.0
        _ -> constNull T.i8



-- codegenTop :: S.Expr -> LLVM ()
codegenTop ctx this@(S.Val meta name expr) = do
    modify (\s -> s { _globalValsInit = _globalValsInit s ++ [(name, expr)] })
    let valType = llvmTypeOf this
    defineGlobal (AST.Name $ fromString name) valType (Just $ defaultValueForType valType)

codegenTop ctx f@(S.Function meta name tpe args body) =
    if meta ^. S.isExternal then
        external (typeMapping tpe) (fromString name ) (externArgsToSig args) False []
    else do
    --    Debug.traceM $ printf "codegenTop %s: %s" (name) (show funcType)
        r1 <- defineStringConstants body
      --   Debug.traceM ("Generating function1 " ++ name ++ (show r1))
      --   defineClosures ctx name body
      --   Debug.traceM ("Generating function2 " ++ name ++ (show r1))
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
            store var (localT (fromString n) t)
    --        Debug.traceM $ printf "assign %s: %s = %s" n (show t) (show var)
            assign n var
        cgen ctx body >>= ret

codegenTop ctx (S.Data _ name constructors) = return ()

codegenTop ctx exp = do
    modState <- get
    define T.double "main" [] (bls modState)
  where
    bls modState = createBlocks $ execCodegen [] modState $ do
        entry <- addBlock entryBlockName
        setBlock entry
        cgen ctx exp >>= ret

codegenStartFunc ctx = do
    modState <- get
    define T.void "start" [("argc", intType), ("argv", ptrType)] (bls modState)
  where
    bls modState = createBlocks $ execCodegen [] modState $ do
        entry <- addBlock entryBlockName
        setBlock entry
        callFn initLascaRuntimeFuncType "initLascaRuntime" [constRefOperand "Runtime"]
        callFn (funcType T.void [intType, ptrType]) "initEnvironment" [local "argc", local "argv"]
        initGlobals
        callFn mainFuncType "main" []
        terminator $ I.Do $ I.Ret Nothing []
        return ()

    initGlobals = do
        modState <- gets moduleState
        let globalValsInit = _globalValsInit modState
        mapM gen globalValsInit

    gen (name, expr) = do
        v <- cgen ctx expr
        let t = llvmTypeOf expr
  --      traceM $ "global type " ++ show t
        store (global t (fromString name)) v
        return v


typeMapping :: Type -> AST.Type
typeMapping t = case t of
                  TypeIdent "Int" -> T.i32
                  TypeIdent "Float" -> T.double
--                  TypeIdent "Unit" -> T.void
                  _                -> ptrType

llvmTypeOf = typeMapping . S.typeOf
mappedReturnType args t = typeMapping $ returnType args t
returnType args t = foldr f t args
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
    vs <- values
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
  where values = sequence [cgen ctx e | e <- exprs]
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
--            tagAddr <- getelementptr2 T.i32 dataStruct [constIntOp 0, constIntOp 0]
--            tag <- load2 T.i32 tagAddr
--            callFn ptrType "putInt" [tag]
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
        (10, TypeIdent "Int") -> add T.i32 llhs lrhs
        (11, TypeIdent "Int") -> sub T.i32 llhs lrhs
        (12, TypeIdent "Int") -> mul T.i32 llhs lrhs
        (13, TypeIdent "Int") -> Codegen.div T.i32 llhs lrhs
        (42, TypeIdent "Int") -> do
          bool <- intEq llhs lrhs
          callFn boxFuncType "boxBool" [bool]
        (47, TypeIdent "Int") -> do
          bool <- intGt llhs lrhs
          callFn boxFuncType "boxBool" [bool]
        (10, TypeIdent "Float") -> fadd llhs lrhs
        (11, TypeIdent "Float") -> fsub llhs lrhs
        (12, TypeIdent "Float") -> fmul llhs lrhs
        (13, TypeIdent "Float") -> fdiv llhs lrhs
        _  -> do
                let codeOp = constIntOp code
                callFn runtimeBinOpFuncType "runtimeBinOp" [codeOp, llhs, lrhs]
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
        this@(S.Ident meta fn) | isGlobal fn -> do
            let funDecl = (ctx ^. S.globalFunctions) Map.! fn
            let fnType = S.typeOf funDecl
            let fff t acc = case t of
                              TypeFunc a b -> fff b (a : acc)
                              a -> a : acc
            let types = reverse $ fff fnType []
            let paramTypes = init types
            let returnType = last types
--            Debug.traceM $ printf "Calling %s: %s from %s, return type %s" fn (show types) (show fnType) (show returnType)
            largs <- forM (zip args paramTypes) $ \(arg, paramType) -> do
                a <- cgen ctx arg
                let argType = S.typeOf arg
                case (paramType, argType) of
                    (TypeIdent l, TypeIdent r) | l == r -> return a
                    (TVar _, TypeIdent "Int") -> do
--                        Debug.traceM ("boxing " ++ show a)
                        callFn boxFuncType "boxInt" [a]
                    (TVar _, TypeIdent "Float") -> do
--                        Debug.traceM ("boxing " ++ show a)
                        callFn boxFuncType "boxFloat64" [a]
                    _ -> return a
            case returnType of
                TypeIdent "Int" -> do
                    res <- callFnType ptrType T.i32 (fromString fn) largs
--                    Debug.traceM ("res = " ++ show res)
                    return res
          --          callFn boxFuncType "boxInt"  [res]
                TypeIdent "Float" -> do
                    res <- callFnType ptrType T.double (fromString fn) largs
--                    Debug.traceM ("res = " ++ show res)
                    return res
          --          callFn boxFuncType "boxFloat64"  [res]
                _ -> callFn ptrType fn largs
        this | isArray this && length args == 1 && isIntType (head args) -> do
            -- this must be arrayApply
            idx <- cgen ctx (head args) -- must be T.i32. TODO should be platform-dependent
            array <- cgen ctx this -- should be a pointer to either boxed or unboxed array
            case S.typeOf this of
                TypeApply (TypeIdent "Array") [TypeIdent "Float"] -> do
                    arrayStructPtr <- bitcast array (T.ptr (T.StructureType False [T.i32, T.ptr T.double]))
                    arrayPtr <- getelementptr arrayStructPtr [constIntOp 0, constIntOp 1]
                    arrayPtr1 <- bitcast arrayPtr (T.ptr T.double)
                    ptr <- getelementptr arrayPtr1 [idx]
                    load ptr
                _ -> do
                    boxedArrayPtr <- bitcast array (T.ptr $ T.StructureType False [T.i32, T.ptr $ T.StructureType False [T.i32, ptrType]]) -- Box(type, &Array(len, &data[])
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
            callFn runtimeApplyFuncType "runtimeApply" [e, argc, sargs, constOp pos]
cgen ctx (S.BoxFunc _ funcName enclosedVars) = do
    modState <- gets moduleState
    let mapping = functions modState
    if null enclosedVars then boxFunc funcName mapping
    else boxClosure funcName mapping enclosedVars
cgen ctx m@S.Match{} = do
    let result = genMatch ctx m
  --  Debug.traceM $ "Generated " ++ show result
    cgen ctx result
cgen ctx (S.If meta cond tr fl) = do
    let resultType = llvmTypeOf tr
    ifthen <- addBlock "if.then"
    ifelse <- addBlock "if.else"
    ifexit <- addBlock "if.exit"
    -- %entry
    ------------------
    cond <- cgen ctx cond
    -- unbox Bool
    voidPtrCond <- callFn unboxFuncType "unbox" [constIntOp 1, cond]
    bool <- ptrtoint voidPtrCond T.i1

    test <- instr2 T.i1 (I.ICmp IP.EQ bool constTrue [])
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

genMatch :: Ctx -> S.Expr -> S.Expr
genMatch ctx m@(S.Match meta expr []) = error $ "Should be at least on case in match expression: " ++ show m
genMatch ctx (S.Match meta expr cases) =
    let body = foldr (\(S.Case p e) acc -> genPattern ctx (S.Ident (expr ^. S.metaLens) "$match") p e acc) genFail cases
    in  S.Let meta "$match" expr body  -- FIXME hack. Gen unique names

genFail = S.Apply S.emptyMeta (S.Ident S.emptyMeta "die") [S.Literal S.emptyMeta $ S.StringLit "Match error!"]

genPattern ctx lhs S.WildcardPattern rhs = const rhs
genPattern ctx lhs (S.VarPattern name) rhs = const (S.Let S.emptyMeta name lhs rhs)
genPattern ctx lhs (S.LitPattern literal) rhs = S.If S.emptyMeta (S.Apply S.emptyMeta (S.Ident S.emptyMeta "==") [lhs, S.Literal S.emptyMeta literal]) rhs
genPattern ctx lhs (S.ConstrPattern name args) rhs = cond
  where cond fail = S.If S.emptyMeta constrCheck (checkArgs name fail) fail
        constrCheck = S.Apply S.emptyMeta (S.Ident S.emptyMeta "runtimeIsConstr") [lhs, S.Literal S.emptyMeta $ S.StringLit name]
        constrMap = let cs = foldr (\ (S.DataDef _ _ constrs) acc -> constrs ++ acc) [] (S.dataDefs ctx)
                        tuples = fmap (\c@(S.DataConst n args) -> (n, args)) cs
                    in  Map.fromList tuples
        checkArgs nm fail =  case Map.lookup nm constrMap of
                            Nothing -> fail
                            Just constrArgs | length args == length constrArgs -> do
                                let argParam = zip args constrArgs
                                foldr (\(a, S.Arg n _) acc -> genPattern ctx (S.Select S.emptyMeta lhs (S.Ident S.emptyMeta n)) a acc fail) rhs argParam
                            Just constrArgs -> error (printf "Constructor %s has %d parameters, but %d given" nm (length constrArgs) (length args)) -- TODO box this error
-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

codegenStaticModule :: S.LascaOpts -> AST.Module -> [S.Expr] -> AST.Module
codegenStaticModule opts modo exprs = modul
  where
    ctx = createGlobalContext opts exprs
    modul = runLLVM modo genModule
    genModule = do
        fns' <- transform extractLambda exprs
        syn <- gets _syntacticAst
        let fns'' = fns' ++ syn
  --           Debug.traceM ("Rewritten exprs: " ++ show fns'')
  --           Debug.traceM ("Rewritten exprs: " ++ show st')
  --           Debug.traceM ("Rewritten exprs: " ++ show st')
        declareStdFuncs
        genFunctionMap fns''
        let defs = reverse (S.dataDefs ctx)
        genTypesStruct ctx defs
        genRuntime opts
        defineStringLit "Match error!" -- TODO remove this hack
        mapM_ (codegenTop ctx) fns''
        codegenStartFunc ctx

genTypesStruct ctx defs = do
    types <- genData ctx defs
    let array = C.Array ptrType types
    defineConst "Types" structType (Just (struct array))
  where len = length defs
        struct a = createStruct [constInt len, a]
        structType = T.StructureType False [T.i32, T.ArrayType (fromIntegral len) ptrType]

genData :: Ctx -> [S.DataDef] -> LLVM ([C.Constant])
genData ctx defs = sequence [genDataStruct d | d <- defs]
  where genDataStruct dd@(S.DataDef tid name constrs) = do
            defineStringLit name
            let literalName = fromString $ "Data." ++ name
            let numConstructors = length constrs
            constructors <- genConstructors ctx dd
            let arrayOfConstructors = C.Array ptrType constructors
            let struct = createStruct [constInt tid, globalStringRefAsPtr name, constInt numConstructors, arrayOfConstructors] -- struct Data
            defineConst literalName (T.StructureType False [T.i32, ptrType, T.i32, T.ArrayType (fromIntegral numConstructors) ptrType]) (Just struct)
            return (constRef literalName)

genConstructors ctx (S.DataDef tid name constrs) = do
    forM (zip constrs [0..]) $ \ ((S.DataConst n args), tag) ->
        defineConstructor ctx (fromString name) n tid tag args

boxStructType = T.StructureType False [T.i32, ptrType]

defineConstructor ctx typeName name tid tag args  = do
  -- TODO optimize for zero args
    modState <- get
    let codeGenResult = codeGen modState
    let blocks = createBlocks codeGenResult

    if null args
    then do
        let singletonName = name ++ ".Singleton"
        let dataValue = createStruct [constInt tag, C.Array ptrType []]
        defineConst (fromString singletonName) tpe (Just dataValue)

        let boxed = createStruct [constInt tid, constRef (fromString singletonName)]
        defineConst (fromString $ singletonName ++ ".Boxed") boxStructType (Just boxed)

        let boxedRef = C.GlobalReference boxStructType (AST.Name (fromString $ singletonName ++ ".Boxed"))
        let ptrRef = C.BitCast boxedRef ptrType
        defineConst (fromString name) ptrType (Just ptrRef)
    else do
        define ptrType (fromString name) fargs blocks -- define constructor function
        forM_ args $ \ (S.Arg name _) -> defineStringLit name -- define fields names as strings

    let structType = T.StructureType False [T.i32, ptrType, T.i32, T.ArrayType (fromIntegral len) ptrType]
    let struct =  createStruct [C.Int 32 (fromIntegral tid), globalStringRefAsPtr name, constInt len, fieldsArray]
    let literalName = fromString $ typeName ++ "." ++ name
    defineConst literalName structType (Just struct)

    return $ constRef literalName

  where
    len = length args
    arrayType = T.ArrayType (fromIntegral len) ptrType
    tpe = T.StructureType False [T.i32, arrayType] -- DataValue: {tag, values: []}
    fargs = externArgsToSig args
    fieldsArray = C.Array ptrType fields
    fields = map (\(S.Arg n _) -> globalStringRefAsPtr n) args

    codeGen modState = execCodegen [] modState $ do
        entry <- addBlock entryBlockName
        setBlock entry
        (ptr, structPtr) <- gcMallocType tpe
        tagAddr <- getelementptr structPtr [constIntOp 0, constIntOp 0] -- [dereference, 1st field] {tag, [arg1, arg2 ...]}
        store tagAddr (constIntOp tag)
        let argsWithId = zip args [0..]
        forM_ argsWithId $ \(S.Arg n t, i) -> do
            p <- getelementptr structPtr [constIntOp 0, constIntOp 1, constIntOp i] -- [dereference, 2nd field, ith element] {tag, [arg1, arg2 ...]}
            ref <- case t of
                  TypeIdent "Float" -> fptoptr $ localT (fromString n) T.double
                  TypeIdent "Int"   -> inttoptr $ localT (fromString n) T.i32
                  _                 -> return $ local $ fromString n
            store p (ref)
        -- remove boxing after removing runtimeIsConstr from genPattern, do a proper tag check instead
        boxed <- callFn ptrType "box" [constIntOp tid, ptr]
        ret boxed
--        ret ptr