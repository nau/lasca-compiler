{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}

module EmitCommon where

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

import Codegen
import Type
import qualified Syntax as S
import Syntax (Ctx, createGlobalContext)
import qualified Options as Opts


externalTypeMapping :: Type -> AST.Type
-- FIXME currently we assume every function returns a result and can't be Unit/void
--externalTypeMapping (TypeIdent "Unit") = T.void
externalTypeMapping TypeInt = intType
externalTypeMapping TypeFloat = T.double
externalTypeMapping _ = ptrType-- Dynamic mode

externArgsToSig :: [S.Arg] -> [(SBS.ShortByteString, AST.Type)]
externArgsToSig = map (\(S.Arg name tpe) -> (fromString (show name), externalTypeMapping tpe))

externArgsToLlvmTypes args = map snd (externArgsToSig args)

externFuncLLvmType (S.Function _ _ tpe args _) = funcType (externalTypeMapping tpe) (externArgsToLlvmTypes args)
funcLLvmType (S.Function _ _ tpe args _) = funcType (ptrType) (map (const ptrType) args)

staticArgsToSig :: [S.Arg] -> [(SBS.ShortByteString, AST.Type)]
staticArgsToSig = map (\(S.Arg name tpe) -> (nameToSBS name, typeMapping tpe))

argToPtr (S.Arg n t) = case t of
--    TypeFloat -> fptoptr $ local T.double (nameToSBS n)
--    TypeInt   -> inttoptr $ local T.i32 (nameToSBS n)
    _                 -> return $ localPtr $ nameToSBS n

typeMapping :: Type -> AST.Type
typeMapping t = case t of
--                  TypeInt -> T.i32
--                  TypeFloat -> T.double
--                  TypeIdent "Unit" -> T.void
                  _                -> ptrType

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
    S.Let _ _ e body -> do
        defineStringConstants e
        defineStringConstants body
        return ()
    S.Match _ e cases -> do
         defineStringConstants e
         mapM_ (\(S.Case p e) -> defineStringConstants e) cases
         return ()
    S.Lam{} -> error $ printf "defineStringConstants should be called after lambda lift! %s" (show expr)
    S.Function meta name retType args expr -> do
        defineStringConstants expr
        return ()
    S.Package{} -> return ()
    S.Import{} -> return ()
    S.EmptyExpr -> return ()
    S.BoxFunc{} -> return ()
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

gcMalloc size = callFn (funcType ptrType [intType]) "gcMalloc" [size]

gcMallocType tpe = do
    size <- sizeOfType tpe
    ptr <- gcMalloc size
    casted <- bitcast ptr (T.ptr tpe)
    return (ptr, casted)

unitTypePtrOp = constOp $ constRef ptrType "_UNIT"
boolTypePtrOp = constOp $ constRef ptrType "_BOOL"
intTypePtrOp = constOp $ constRef ptrType  "_INT"
doubleTypePtrOp = constOp $ constRef ptrType "_DOUBLE"
closureTypePtrOp = constOp $ constRef ptrType "_CLOSURE"
arrayTypePtrOp = constOp $ constRef ptrType "_ARRAY"

stringTypePtr = constRef ptrType "_STRING"
stringTypePtrOp = constOp $ stringTypePtr

box t v = callFn (funcType ptrType [ptrType, ptrType]) "box" [t, v]
boxBool v = callFn (funcType ptrType [intType]) "boxBool" [v] -- todo change to i1
boxInt v = callFn (funcType ptrType [intType]) "boxInt" [v]
boxFloat64 v = callFn (funcType ptrType [T.double]) "boxFloat64" [v]
unbox t v = callFn (funcType ptrType [ptrType, ptrType]) "unbox" [t, v]


boxLit (S.BoolLit b) meta = boxBool (constIntOp (boolToInt b))
boxLit (S.IntLit  n) meta = boxInt (constIntOp n)
boxLit (S.FloatLit  n) meta = boxFloat64 (constFloatOp n)
boxLit S.UnitLit meta = return $ constOp $ constRef ptrType "UNIT_SINGLETON"
boxLit (S.StringLit s) meta = do
    let name = getStringLitName s
    let len = 1 + (ByteString.length . UTF8.fromString $ s)
    let ref = global (stringStructType len) name
    ref' <- bitcast ref ptrType
    box stringTypePtrOp ref'

createPosition S.NoPosition = createStruct [constInt 0, constInt 0] -- Postion (0, 0) means No Position. Why not.
createPosition S.Position{S.sourceLine, S.sourceColumn} = createStruct [constInt sourceLine, constInt sourceColumn]

boxArray values = callFn (T.FunctionType ptrType [intType] True) "boxArray" (constIntOp (length values) : values)

boxError name = do
    modify (\s -> s { generatedStrings = name : generatedStrings s })
    let strLitName = getStringLitName name
    let len = length name + 1
    let ref = global (stringStructType len) strLitName
    ref <- bitcast ref ptrType
    callFn (funcType ptrType [ptrType]) "boxError" [ref]

showSyms = show . map fst


boxClosure :: Name -> Map Name Int -> [S.Arg] -> Codegen AST.Operand
boxClosure name mapping enclosedVars = do
    syms <- gets symtab
    let idx = fromMaybe (error $ printf "No such function %s in mapping %s" (show name) (show mapping)) (Map.lookup name mapping)
    let argc = length enclosedVars
    let findArg n = fromMaybe (error ("Couldn't find " ++ show n ++ " variable in symbols " ++ showSyms syms)) (lookup n syms)
    args <- forM enclosedVars $  \(S.Arg n _) -> load (findArg n)

    sargsPtr <- if null enclosedVars then return constNullPtrOp else boxArray args
    callFn (funcType ptrType [intType, ptrType]) "boxClosure" [constIntOp idx, sargsPtr]

boolToInt True = 1
boolToInt False = 0

declareStdFuncs = do
    externalConst ptrType "UNIT_SINGLETON"
    externalConst ptrType "UNIT"
    externalConst ptrType "_UNIT"
    externalConst ptrType "BOOL"
    externalConst ptrType "_BOOL"
    externalConst ptrType "INT"
    externalConst ptrType "_INT"
    externalConst ptrType "DOUBLE"
    externalConst ptrType "_DOUBLE"
    externalConst ptrType "CLOSURE"
    externalConst ptrType "_CLOSURE"
    externalConst ptrType "ARRAY"
    externalConst ptrType "_ARRAY"
    externalConst ptrType "_STRING"
    external T.void  "initLascaRuntime" [("runtime", ptrType)] False []
    external ptrType "gcMalloc" [("size", intType)] False []
    external ptrType "box" [("t", ptrType), ("ptr", ptrType)] False [FA.GroupID 0]
    external ptrType "unbox" [("t", ptrType), ("ptr", ptrType)] False [FA.GroupID 0]
    external intType "unboxInt" [("ptr", ptrType)] False [FA.GroupID 0]
    external T.double "unboxFloat64" [("ptr", ptrType)] False [FA.GroupID 0]
    external ptrType "boxError" [("n", ptrType)] False [FA.GroupID 0]
    external ptrType "boxInt" [("d", intType)] False [FA.GroupID 0]
    external ptrType "boxBool" [("d", intType)] False [FA.GroupID 0]
    external ptrType "boxClosure" [("id", intType), ("argv", ptrType)] False []
    external ptrType "boxFloat64" [("d", T.double)] False [FA.GroupID 0]
    external ptrType "boxArray" [("size", intType)] True [FA.GroupID 0]
    external ptrType "runtimeBinOp"  [("code",  intType), ("lhs",  ptrType), ("rhs", ptrType)] False [FA.GroupID 0]
    external ptrType "runtimeUnaryOp"  [("code",  intType), ("expr",  ptrType)] False [FA.GroupID 0]
    external ptrType "runtimeApply"  [("func", ptrType), ("argc", intType), ("argv", ptrType), ("pos", positionStructType)] False []
    external ptrType "runtimeSelect" [("tree", ptrType), ("expr", ptrType), ("pos", positionStructType)] False [FA.GroupID 0]
    external T.void  "initEnvironment" [("argc", intType), ("argv", ptrType)] False []
  --  external ptrType "runtimeIsConstr" [("value", ptrType), ("name", ptrType)] False [FA.GroupID 0]
    addDefn $ AST.FunctionAttributes (FA.GroupID 0) [FA.ReadOnly]


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
    defineNames = mapM (\name -> defineStringLit (show name)) entriesWithQNames

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
                            [globalStringRefAsPtr sname, constRef tpe (fromString sname), constInt arity]
                        where sname = show name

    funcsWithArities = List.foldl' go [] fns where
        go s (S.Data _ name tvars consts) =
            -- Add data constructors as global functions
            let constrFuncType args = funcType (ptrType) (map (const ptrType) args)
                addConstr n args = if null args then [] else [(n, n, (constrFuncType args), length args)]
                m = foldl (\acc (S.DataConst n args) -> addConstr n args ++ acc) [] consts
            in m ++ s
        go s f@(S.Function meta name tpe args body) = do
            if meta^.S.isExternal
            then let (S.Literal _ (S.StringLit externName)) = body
                 in (name, Name externName, (externFuncLLvmType f), length args) : s
            else (name, name, (funcLLvmType f), length args) : s
        go s _ = s

genRuntime opts fmt tst = defineConst "Runtime" runtimeStructType runtime
  where
    runtime = createStruct [constRef fmt "Functions", constRef tst "Types", constBool $ Opts.verboseMode opts]

genTypeStruct :: Name -> LLVM C.Constant
genTypeStruct name = do
    let nm = show name
    let sbsName = fromString nm
    let sbsLiteralName = fromString (nm ++ ".Literal")
    let typeName = fromString (nm ++ ".Type")
    let (charArray, len) = createCharString nm
    let laTypeStruct = createStruct [constRef (T.ArrayType (fromIntegral len) T.i8) sbsLiteralName]
    defineConst sbsLiteralName (T.ArrayType (fromIntegral len) T.i8) charArray
    defineConst typeName laTypeStructType laTypeStruct
    return $ constRef laTypeStructType typeName
    
--genData :: Ctx -> [S.DataDef] -> ([S.Arg] -> [(SBS.ShortByteString, AST.Type)]) -> LLVM ([C.Constant])
genData ctx defs argsToSig argToPtr = sequence [genDataStruct d | d <- defs]
  where genDataStruct dd@(S.DataDef name constrs) = do
            typePtr <- genTypeStruct name
            defineStringLit (show name)
            let literalName = fromString $ "Data." ++ (show name)
            let numConstructors = length constrs
            constructors <- genConstructors ctx typePtr dd
            let arrayOfConstructors = C.Array ptrType constructors
            let struct = createStruct [typePtr,
                                       globalStringRefAsPtr (show name),
                                       constInt numConstructors,
                                       arrayOfConstructors] -- struct Data
            let dataStructType numConstructors = T.StructureType False [
                      ptrType, ptrType, intType, T.ArrayType (fromIntegral numConstructors) ptrType
                    ]
            defineConst literalName (dataStructType numConstructors) struct
            return (constRef (dataStructType numConstructors) literalName)

        genConstructors ctx typePtr (S.DataDef name constrs) = do
            forM (zip constrs [0..]) $ \ ((S.DataConst n args), tag) ->
                defineConstructor ctx typePtr name n tag args

        defineConstructor ctx typePtr typeName name tag args  = do
          -- TODO optimize for zero args
            modState <- get
            let codeGenResult = codeGen typePtr modState
            let blocks = createBlocks codeGenResult

            if null args
            then do
                let singletonName = show name ++ ".Singleton"
                let dataValue = createStruct [constInt tag, C.Array ptrType []]
                defineConst (fromString singletonName) dataValueStructType dataValue

                let boxed = createStruct [typePtr, constRef dataValueStructType (fromString singletonName)]
                defineConst (fromString $ singletonName ++ ".Boxed") boxStructType boxed
                -- TODO use constRef
                let boxedRef = C.GlobalReference (T.ptr boxStructType) (AST.Name (fromString $ singletonName ++ ".Boxed"))
                let ptrRef = C.BitCast boxedRef ptrType
                defineStringLit (fromString $ show name)
                defineConst (fromString $ show name) ptrType ptrRef
            else do
                define ptrType (fromString $ show name) fargs blocks -- define constructor function
                forM_ args $ \ (S.Arg name _) -> defineStringLit (show name) -- define fields names as strings

            let structType = T.StructureType False [ptrType, ptrType, intType, T.ArrayType (fromIntegral len) ptrType]
            let struct =  createStruct [typePtr, globalStringRefAsPtr (show name), constInt len, fieldsArray]
            let literalName = fromString $ (show typeName) ++ "." ++ show name
            defineConst literalName structType struct

            return $ constRef structType literalName

          where
            len = length args
            arrayType = T.ArrayType (fromIntegral len) ptrType
            dataValueStructType = T.StructureType False [intType, arrayType] -- DataValue: {tag, values: []}
            fargs = argsToSig args
            fieldsArray = C.Array ptrType fields
            fields = map (\(S.Arg n _) -> globalStringRefAsPtr (show n)) args

            codeGen typePtr modState = execCodegen [] modState $ do
                entry <- addBlock entryBlockName
                setBlock entry
                (ptr, structPtr) <- gcMallocType dataValueStructType
                tagAddr <- getelementptr structPtr [constIntOp 0, constInt32Op 0] -- [dereference, 1st field] {tag, [arg1, arg2 ...]}
                store tagAddr (constIntOp tag)
                let argsWithId = zip args [0..]
                forM_ argsWithId $ \(arg, i) -> do
                    p <- getelementptr structPtr [constIntOp 0, constInt32Op 1, constIntOp i] -- [dereference, 2nd field, ith element] {tag, [arg1, arg2 ...]}
                    ref <- argToPtr arg
                    store p ref
                -- remove boxing after removing runtimeIsConstr from genPattern, do a proper tag check instead
                boxed <- box (constOp typePtr) ptr
                ret boxed
        --        ret ptr

codegenStartFunc ctx cgen mainName = do
    modState <- get
    define T.void "start" [("argc", intType), ("argv", ptrType)] (bls modState)
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
        store (global ptrType (fromString (show name))) v
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