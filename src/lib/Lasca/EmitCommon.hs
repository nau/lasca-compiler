{-# LANGUAGE TemplateHaskell #-}

module Lasca.EmitCommon where

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
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Debug.Trace as Debug

import Lasca.Codegen
import Lasca.Type
import qualified Lasca.Syntax as S
import Lasca.Syntax (Ctx)
import qualified Lasca.Options as Opts


externalTypeMapping :: Type -> AST.Type
externalTypeMapping tpe = case tpe of
    TypeByte  -> T.i8
    TypeBool  -> boolType
    TypeInt   -> intType
    TypeInt16 -> T.i16
    TypeInt32 -> T.i32
    TypeFloat -> T.double
    _         -> ptrType -- Dynamic mode

autoBoxedTypes = Set.fromList [TypeBool, TypeByte, TypeInt, TypeInt16, TypeInt32, TypeFloat]

isPrimitiveType tpe = tpe `Set.member` autoBoxedTypes

typeToLaTypeConstantName :: Type -> SBS.ShortByteString
typeToLaTypeConstantName tpe = case tpe of
    TypeIdent name -> textToSBS $ nameToText name `T.append` "_LaType"
    TypeFunc from to -> "Closure_LaType"
    TypeApply t _ -> typeToLaTypeConstantName t
    TVar _ -> error "typeToLaTypeConstantName on TVar"
    Forall _ t -> typeToLaTypeConstantName t

typeToLaTypeRef :: Type -> C.Constant
typeToLaTypeRef tpe = constRef ptrType $ typeToLaTypeConstantName tpe

externArgsToSig :: [S.Arg] -> [(SBS.ShortByteString, AST.Type)]
externArgsToSig = map (\(S.Arg name tpe) -> ((nameToSBS name), externalTypeMapping tpe))

externArgsToLlvmTypes args = map snd (externArgsToSig args)

externFuncLLvmType (S.Let True _ _ tpe lam _) = do
    let (args, _) = S.uncurryLambda lam
    funcType (externalTypeMapping tpe) (externArgsToLlvmTypes args)
externFuncLLvmType e = error ("externFuncLLvmType should only be called on Function, but called on" ++ show e)

funcLLvmType (S.Let True _ _ tpe lam _) = do
    let (args, _) = S.uncurryLambda lam
    funcType ptrType (map (const ptrType) args)
funcLLvmType e = error ("funcLLvmType should only be called on Function, but called on" ++ show e)

staticArgsToSig :: [S.Arg] -> [(SBS.ShortByteString, AST.Type)]
staticArgsToSig = map (\(S.Arg name tpe) -> (nameToSBS name, typeMapping tpe))

argToPtr (S.Arg n t) = case t of
--    TypeFloat -> fptoptr $ local T.double (nameToSBS n)
--    TypeInt   -> inttoptr $ local T.i32 (nameToSBS n)
    _                 -> return $ localPtr $ nameToSBS n

typeMapping :: Type -> AST.Type
typeMapping t = ptrType

llvmTypeOf = typeMapping . S.typeOf
mappedReturnType args t = typeMapping $ declaredReturnType args t
declaredReturnType args t = foldr f t args
  where f arg (TypeFunc a b) = b
        f arg t = t



defaultValueForType tpe =
    case tpe of
        T.FloatingPointType T.DoubleFP -> constFloat 0.0
        _ -> constNull T.i8

defineStringConstants :: S.Expr -> LLVM ()
defineStringConstants expr = case expr of
    S.Literal _ (S.StringLit s) -> defineStringLit s
    S.Literal _ _ -> return ()
    S.Ident{} -> return ()
    S.If _ cond true false -> do
        defineStringConstants cond
        defineStringConstants true
        defineStringConstants false
        return ()
    S.Select _ lhs rhs -> do
        defineStringConstants lhs
        defineStringConstants rhs
        return ()
    S.Array _ exprs -> mapM_ defineStringConstants exprs
    S.Apply meta tree exprs -> do
        defineStringConstants tree
        mapM_ defineStringConstants exprs
    S.Let _ _ _ _ e body -> do
        defineStringConstants e
        defineStringConstants body
        return ()
    S.Match _ e cases -> do
         defineStringConstants e
         mapM_ (\(S.Case p e) -> defineStringConstants e) cases
         return ()
    S.Lam _ _ e -> do
        defineStringConstants e
        return ()
    S.Module{} -> return ()
    S.Import{} -> return ()
    S.EmptyExpr -> return ()
    S.Closure{} -> return ()
    S.Data{} -> return ()

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

binops :: Map Name Int
binops = Map.fromList [("+", 10), ("-", 11), ("*", 12), ("/", 13),
    ("==", 42), ("!=", 43), ("<", 44), ("<=", 45), (">=", 46), (">", 47)]

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------
sizeOfType tpe = do
    nullptr <- getelementptr (constOp (constNull tpe)) [constIntOp 1]
    sizeof <- ptrtoint nullptr intType
    return sizeof

gcMalloc size = callBuiltin "gcMalloc" [size]

gcMallocType tpe = do
    size <- sizeOfType tpe
    ptr <- gcMalloc size
    casted <- bitcast ptr (T.ptr tpe)
    return (ptr, casted)

-- takes second field of boxed Int, Byte, Bool, Float, i.e. its value
unboxDirect expr boxedType = do
    boxed <- bitcast expr (T.ptr boxedType)
    unboxedAddr <- getelementptr boxed [constIntOp 0, constInt32Op 1]
    load unboxedAddr
{-# INLINE unboxDirect #-}

unboxByte expr = unboxDirect expr boxedByteType

unboxBool expr = unboxDirect expr boxedBoolType

unboxInt expr = unboxDirect expr boxedIntType
{-# INLINE unboxInt #-}

unboxInt16 :: AST.Operand -> Codegen AST.Operand
unboxInt16 expr = unboxDirect expr boxedInt16Type

unboxInt32 :: AST.Operand -> Codegen AST.Operand
unboxInt32 expr = unboxDirect expr boxedInt32Type

unboxFloat64 expr = unboxDirect expr boxedFloatType
{-# INLINE unboxFloat64 #-}

boxByte v = callBuiltin "boxByte" [v]
boxBool v = callBuiltin "boxBool" [v] -- todo change to i1
boxInt v = callBuiltin "boxInt" [v]
boxInt16 v = callBuiltin "boxInt16" [v]
boxInt32 v = callBuiltin "boxInt32" [v]
boxFloat64 v = callBuiltin "boxFloat64" [v]
unbox t v = callBuiltin "unbox" [t, v]
unboxBoolDynamically v = do
    let ref = typeToLaTypeRef TypeBool
    unbox (constOp ref) v -- checks types
    unboxBool v



boxLit (S.BoolLit b) meta = boxBool (constOp $ constByte (boolToInt b))
boxLit (S.IntLit  n) meta = boxInt (constIntOp n)
boxLit (S.FloatLit  n) meta = boxFloat64 (constFloatOp n)
boxLit S.UnitLit meta = return $ constOp $ constRef ptrType "UNIT_SINGLETON"
boxLit (S.StringLit s) meta = return $ constOp $ globalStringRefAsPtr s

createPosition S.NoPosition = createStruct [constInt 0, constInt 0] -- Postion (0, 0) means No Position. Why not.
createPosition S.Position{S.sourceLine, S.sourceColumn} = createStruct [constInt sourceLine, constInt sourceColumn]

boxArray values = callBuiltin "boxArray" (constIntOp (length values) : values)

boxError :: Text -> Codegen AST.Operand
boxError name = do
    let ref = constOp . globalStringRefAsPtr $ name
    callBuiltin "boxError" [ref]

showSyms = show . map fst


boxClosure :: Name -> Map Name Int -> [S.Arg] -> Codegen AST.Operand
boxClosure name mapping enclosedVars = do
    syms <- gets symtab
    let idx = fromMaybe (error $ printf "No such function %s in mapping %s" (show name) (show mapping)) (Map.lookup name mapping)
    let argc = length enclosedVars
    let findArg n = fromMaybe (error ("Couldn't find " ++ show n ++ " variable in symbols " ++ showSyms syms)) (lookup n syms)
    let args = map (\(S.Arg n _) -> findArg n) enclosedVars
    sargsPtr <- gcMalloc (constIntOp $ ptrSize * argc)
    sargsPtr1 <- bitcast sargsPtr (T.ptr ptrType)
    let asdf (idx, arg) = do
            p <- getelementptr sargsPtr1 [idx]
            bc1 <- bitcast p (T.ptr ptrType)
            bc <- load arg
            store bc1 bc

    let sargs = sargsPtr
    sequence_ [asdf (constIntOp i, a) | (i, a) <- zip [0 .. argc] args]
    callBuiltin "boxClosure" [constIntOp idx, constIntOp argc, sargsPtr]


boolToInt True = 1
boolToInt False = 0

builtinConsts =
    [ "UNIT_SINGLETON"
    , typeToLaTypeConstantName TypeUnit
    , typeToLaTypeConstantName TypeBool
    , typeToLaTypeConstantName TypeByte
    , typeToLaTypeConstantName TypeInt
    , typeToLaTypeConstantName TypeInt16
    , typeToLaTypeConstantName TypeInt32
    , typeToLaTypeConstantName TypeFloat
    , typeToLaTypeConstantName TypeString
    , typeToLaTypeConstantName (TypeFunc TypeUnit TypeUnit) -- resolves to Closure type
    , typeToLaTypeConstantName (TypeArray TypeUnit)
    , typeToLaTypeConstantName (TypeByteArray TypeUnit)
    ]

builtinFuncs = do
  let external resType name params vararg attrs = (name, (resType, params, vararg, attrs))
  Map.fromList $
    [ external T.void  "initLascaRuntime" [("runtime", ptrType)] False []
    , external ptrType "gcMalloc" [("size", intType)] False []
    , external ptrType "unbox" [("t", ptrType), ("ptr", ptrType)] False [FA.GroupID 0]
    , external ptrType "boxError" [("n", ptrType)] False [FA.GroupID 0]
    , external ptrType "boxByte" [("d", T.i8)] False [FA.GroupID 0]
    , external ptrType "boxInt" [("d", intType)] False [FA.GroupID 0]
    , external ptrType "boxInt16" [("d", T.i16)] False [FA.GroupID 0]
    , external ptrType "boxInt32" [("d", T.i32)] False [FA.GroupID 0]
    , external ptrType "boxBool" [("d", boolType)] False [FA.GroupID 0]
    , external ptrType "boxClosure" [("id", intType), ("argc", intType), ("argv", ptrType)] False []
    , external ptrType "boxFloat64" [("d", T.double)] False [FA.GroupID 0]
    , external ptrType "boxArray" [("size", intType)] True [FA.GroupID 0]
    , external ptrType "runtimeBinOp"  [("code",  intType), ("lhs",  ptrType), ("rhs", ptrType)] False [FA.GroupID 0]
    , external ptrType "runtimeUnaryOp"  [("code",  intType), ("expr",  ptrType)] False [FA.GroupID 0]
    , external ptrType "runtimeApply"  [("func", ptrType), ("argc", intType), ("argv", ptrType), ("pos", positionStructType)] False []
    , external ptrType "runtimeSelect" [("tree", ptrType), ("expr", ptrType), ("pos", positionStructType)] False [FA.GroupID 0]
    , external T.void  "initEnvironment" [("argc", intType), ("argv", ptrType)] False []
    ]

declareStdFuncs = do
    forM_ builtinConsts (externalConst ptrType) -- declare constants
    forM (Map.toList builtinFuncs) $ \(name, args) -> do
        let (restype, params, vararg, attrs) = args
        external restype name params vararg attrs
  --  external ptrType "runtimeIsConstr" [("value", ptrType), ("name", ptrType)] False [FA.GroupID 0]
    addDefn $ AST.FunctionAttributes (FA.GroupID 0) [FA.ReadOnly]

callBuiltin :: SBS.ShortByteString -> [AST.Operand] -> Codegen AST.Operand
callBuiltin name args = do
    let (restype, params, vararg, attrs) = builtinFuncs Map.! name
        ps = map snd params
        ftype = T.FunctionType restype ps vararg
    call ftype (globalOp ftype name) args
{-# INLINE callBuiltin #-}


myshow funcsWithArities = do
    (n, t, a) <- funcsWithArities
    return $ show n ++ " " ++ take 50 (show t) ++ " " ++ show a

genFunctionMap :: [S.Expr] -> LLVM T.Type
genFunctionMap fns = do
    defineNames
    defineConst "Functions" funcMapType struct1
--    Debug.traceM (List.intercalate "\n" $ map show fns)
--    Debug.traceM (List.intercalate "\n" $ myshow funcsWithArities)
--    Debug.traceM $ printf "mapping %d, funcsWithArities %d" (Map.size mapping) (length funcsWithArities)
--    Debug.traceM (show mapping)
--    Debug.traceM (show array)
    modify (\s -> s { functions = mapping })
    return funcMapType
  where
    funcMapType = functionsStructType (fromIntegral len)
    defineNames = mapM (\name -> defineStringLit (nameToText name)) entriesWithQNames

    len :: Int
    len = length funcsWithArities

    array = C.Array functionStructType (fmap snd entriesWithCNames)

    mapping :: Map Name Int
    mapping = do
        let entriesWithIdx = zip entriesWithQNames [0..]
        let insert k new old = error $ "Function "
              ++ (show k) ++ " already defined! In entries "
              ++ (List.intercalate "\n" $ map show entriesWithQNames)
        List.foldl' (\m (name, idx) -> Map.insertWithKey insert name idx m) Map.empty entriesWithIdx



    entriesWithCNames = fmap (\(name, cname, llvmType, arity) -> (cname, struct cname llvmType arity)) funcsWithArities
    entriesWithQNames = fmap (\(name, cname, llvmType, arity) -> name) funcsWithArities

    struct1 = createStruct [constInt (len), array]

    struct name tpe arity = createStruct
                            [globalStringRefAsPtr nm, constRef tpe sbsName, constInt arity]
                        where nm = nameToText name
                              sbsName = textToSBS nm

    funcsWithArities = List.foldl' go [] fns where
        go s (S.Data _ name tvars consts) =
            -- Add data constructors as global functions
            let constrFuncType args = funcType (ptrType) (map (const ptrType) args)
                addConstr n args = if null args then [] else [(n, n, (constrFuncType args), length args)]
                m = foldl (\acc (S.DataConst n args) -> addConstr n args ++ acc) [] consts
            in m ++ s
        go s f@(S.Let True meta name tpe lam _) = do
            let (args, _) = S.uncurryLambda lam
            (name, name, (funcLLvmType f), length args) : s
        go s _ = s

genRuntime opts fmt tst = defineConst "Runtime" runtimeStructType runtime
  where
    runtime = createStruct [constRef fmt "Functions", constRef tst "Types", constBool $ Opts.verboseMode opts]

genTypeStruct :: Name -> LLVM C.Constant
genTypeStruct name = do
    let nm = nameToText name
    let sbsName = textToSBS nm
    let literalName = nm `T.append` ".Literal"
    let sbsLiteralName = textToSBS literalName
    let typeName = nm `T.append` "_LaType"
    let sbsTypeName = textToSBS typeName
    let (charArray, len) = createCString nm
    let laTypeStruct = createStruct [constRef (T.ArrayType (fromIntegral len) T.i8) sbsLiteralName]
    defineConst sbsLiteralName (T.ArrayType (fromIntegral len) T.i8) charArray
    defineConst sbsTypeName laTypeStructType laTypeStruct
    return $ constRef laTypeStructType sbsTypeName

--genData :: Ctx -> [S.Expr] -> ([S.Arg] -> [(SBS.ShortByteString, AST.Type)]) -> LLVM ([C.Constant])
genData ctx defs argsToSig argToPtr = sequence [genDataStruct d | d <- defs]
  where genDataStruct dd@(S.Data meta name tvars constrs) = do
            typePtr <- genTypeStruct name
            defineStringLit (nameToText name)
            let literalName = fromString $ "Data." ++ (show name)
            let numConstructors = length constrs
            constructors <- genConstructors ctx typePtr dd
            let arrayOfConstructors = C.Array ptrType constructors
            let struct = createStruct [typePtr,
                                       globalStringRefAsPtr (nameToText name),
                                       constInt numConstructors,
                                       arrayOfConstructors] -- struct Data
            let dataStructType numConstructors = T.StructureType False [
                      ptrType, ptrType, intType, T.ArrayType (fromIntegral numConstructors) ptrType
                    ]
            defineConst literalName (dataStructType numConstructors) struct
            return (constRef (dataStructType numConstructors) literalName)
        genDataStruct e = error ("genDataStruct should only be called on Data, but called on" ++ show e)

        genConstructors ctx typePtr (S.Data meta name tvars constrs) = do
            forM (zip constrs [0..]) $ \ ((S.DataConst n args), tag) ->
                defineConstructor ctx typePtr name n tag args
        genConstructors ctx typePtr e = error ("genConstructors should only be called on Data, but called on" ++ show e)

        defineConstructor ctx typePtr typeName name tag args  = do
          -- TODO optimize for zero args
            modState <- get
            let codeGenResult = codeGen typePtr modState
            let blocks = createBlocks codeGenResult
                txtName = nameToText name
                sbsName = textToSBS txtName
            if null args
            then do
                let singletonName = show name ++ ".Singleton"
                let dataValue = createStruct [typePtr, constInt tag, C.Array ptrType []]
                defineConst (fromString singletonName) (dataValueStructType len) dataValue
                let ptrRef = constRef (dataValueStructType len) (fromString singletonName)
                defineStringLit txtName
                defineConst sbsName ptrType ptrRef
            else do
                define ptrType sbsName fargs blocks -- define constructor function
                forM_ args $ \ (S.Arg name _) -> defineStringLit (nameToText name) -- define fields names as strings

            let structType = T.StructureType False [ptrType, ptrType, intType, T.ArrayType (fromIntegral len) ptrType]
            let struct =  createStruct [typePtr, globalStringRefAsPtr (nameToText name), constInt len, fieldsArray]
            let literalName = fromString $ (show typeName) ++ "." ++ show name
            defineConst literalName structType struct

            return $ constRef structType literalName

          where
            len = length args
            fargs = argsToSig args
            fieldsArray = C.Array ptrType fields
            fields = map (\(S.Arg n _) -> globalStringRefAsPtr (nameToText n)) args

            codeGen typePtr modState = execCodegen [] modState $ do
                entry <- addBlock entryBlockName
                setBlock entry
                (ptr, structPtr) <- gcMallocType (dataValueStructType len)
                typeAddr <- getelementptr structPtr [constIntOp 0, constInt32Op 0] -- [dereference, 1st field: type] {LaType*, tag, [arg1, arg2 ...]}
                store typeAddr (constOp typePtr)
                tagAddr <- getelementptr structPtr [constIntOp 0, constInt32Op 1] -- [dereference, 2nd field: tag] {LaType*, tag, [arg1, arg2 ...]}
                store tagAddr (constIntOp tag)
                let argsWithId = zip args [0..]
                forM_ argsWithId $ \(arg, i) -> do
                    p <- getelementptr structPtr [constIntOp 0, constInt32Op 2, constIntOp i] -- [dereference, 3rd field, ith element] {LaType*, tag, [arg1, arg2 ...]}
                    ref <- argToPtr arg
                    store p ref
                ret ptr

codegenStartFunc ctx cgen mainName = do
    modState <- get
    define T.void "main" [("argc", intType), ("argv", ptrType)] (bls modState)
  where
    bls modState = createBlocks $ execCodegen [] modState $ do
        entry <- addBlock entryBlockName
        setBlock entry
        instrDo $ callFnIns (funcType T.void [ptrType]) "initLascaRuntime" [constOp $ constRef runtimeStructType "Runtime"]
        instrDo $ callFnIns (funcType T.void [intType, ptrType]) "initEnvironment" [local intType "argc", localPtr "argv"]
        initGlobals
        callFn (funcType ptrType []) mainName []
        terminator $ I.Do $ I.Ret Nothing []
        return ()

    initGlobals = do
        modState <- gets moduleState
        let globalValsInit = _globalValsInit modState
        mapM gen globalValsInit

    gen (name, expr) = do
        v <- cgen ctx expr
        store (globalOp ptrType (fromString (show name))) v
        return v

cgenIf resultType cond tr fl = do
    ifthen <- addBlock "if.then"
    ifelse <- addBlock "if.else"
    ifexit <- addBlock "if.exit"
    -- %entry
    ------------------
    test <- cond
    cbr test ifthen ifelse -- Branch based on the condition

    -- if.then
    ------------------
    setBlock ifthen
    trval <- tr       -- Generate code for the true branch
    br ifexit              -- Branch to the merge block
    ifthen <- getBlock

    -- if.else
    ------------------
    setBlock ifelse
    flval <- fl       -- Generate code for the false branch
    br ifexit              -- Branch to the merge block
    ifelse <- getBlock

    -- if.exit
    ------------------
    setBlock ifexit
    phi resultType [(trval, ifthen), (flval, ifelse)]

genTypesStruct ctx defs = do
    types <- genData ctx defs toSig argToPtr
--    Debug.traceM $ printf "genTypesStruct %s" (show types)
    let array = C.Array ptrType types
    defineConst "Types" structType (struct array)
    return structType
  where len = length defs
        struct a = createStruct [constInt len, a]
        structType = T.StructureType False [intType, T.ArrayType (fromIntegral len) ptrType]