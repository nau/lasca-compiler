{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Strict #-}

module Emit where

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
import qualified Syntax as S
import Syntax (Ctx, createGlobalContext)


uncurryLambda expr = go expr ([], expr) where
  go (S.Lam _ name e) result = let (args, body) = go e result in (name : args, body)
  go e (args, _) = (args, e)



transform :: (S.Expr -> LLVM S.Expr) -> [S.Expr] -> LLVM [S.Expr]
transform transformer exprs = sequence [transformExpr transformer expr | expr <- exprs]

transformExpr :: (S.Expr -> LLVM S.Expr) -> S.Expr -> LLVM S.Expr
transformExpr transformer expr = case expr of
  (S.If meta cond true false) -> do
    cond' <- go cond
    true' <- go true
    false' <- go false
    transformer (S.If meta cond' true' false')
  (S.Let meta n e body) -> do
    modStateLocals %= Set.insert n
    e' <- go e
    body' <- go body
    transformer (S.Let meta n e' body')
  (S.Lam m a@(S.Arg n t) e) -> do
    modify (\s -> s { _outers = _locals s } )
    modStateLocals .= Set.singleton n
    e' <- go e
    transformer (S.Lam m a e')
  (S.Apply meta e args) -> do
    e' <- go e
    args' <- sequence [go arg | arg <- args]
    transformer (S.Apply meta e' args')
  (S.Function meta name tpe args e1) -> do
    let argNames = Set.fromList (map (\(S.Arg n _) -> n) args)
    modify (\s -> s { _locals = argNames, _outers = Set.empty, _usedVars = Set.empty } )
    e' <- go e1
    transformer (S.Function meta name tpe args e')
  e -> transformer e
  where go e = transformExpr transformer e



defineStringConstants :: S.Expr -> LLVM ()
defineStringConstants (S.Literal meta (S.StringLit s)) = defineStringLit s
defineStringConstants (S.If _ cond true false) = do
  defineStringConstants cond
  defineStringConstants true
  defineStringConstants false
  return ()
defineStringConstants (S.Select _ lhs rhs) = do
  defineStringConstants lhs
  defineStringConstants rhs
  return ()
defineStringConstants (S.Array _ exprs) = mapM_ defineStringConstants exprs
defineStringConstants (S.Apply meta _ exprs) = mapM_ defineStringConstants exprs
defineStringConstants (S.Let _ _ e body) = do
  defineStringConstants e
  defineStringConstants body
  return ()
defineStringConstants (S.Match _ e cases) = do
  defineStringConstants e
  mapM_ (\(S.Case p e) -> defineStringConstants e) cases
  return ()
defineStringConstants _ = return ()

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

binops :: Map.Map String Int
binops = Map.fromList [("+", 10), ("-", 11), ("*", 12), ("/", 13),
  ("==", 42), ("!=", 43), ("<", 44), ("<=", 45), (">=", 46), (">", 47)]

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------
funcType retTy args = T.FunctionType retTy args False

initLascaRuntimeFuncType = funcType T.void [ptrType]
mainFuncType = funcType ptrType []
boxFuncType = funcType ptrType [T.i32]
boxArrayFuncType = T.FunctionType ptrType [T.i32] True
runtimeBinOpFuncType = funcType ptrType [T.i32, ptrType, ptrType]
runtimeApplyFuncType = funcType ptrType [ptrType, T.i32, ptrType, positionStructType]
runtimeSelectFuncType = funcType ptrType [ptrType, ptrType, positionStructType]
unboxFuncType = funcType ptrType [ptrType, T.i32]

sizeOfType tpe = do
  nullptr <- getelementptr (constOp (constNull tpe)) [constIntOp 1]
  sizeof <- ptrtoint nullptr T.i32 -- FIXME change to T.i64?
  return sizeof

gcMalloc size = callFn (funcType ptrType [T.i32]) "gcMalloc" [size]

gcMallocType tpe = do
  size <- sizeOfType tpe
  ptr <- gcMalloc size
  casted <- bitcast ptr (T.ptr tpe)
  return (ptr, casted)


box (S.BoolLit b) meta = callFn boxFuncType "boxBool" [constIntOp (boolToInt b)]
box (S.IntLit  n) meta = callFn boxFuncType "boxInt" [constIntOp n]
box (S.FloatLit  n) meta = callFn boxFuncType "boxFloat64" [constFloatOp n]
box S.UnitLit meta = callFn boxFuncType "box" [constIntOp 0,  constOp constNullPtr]
box (S.StringLit s) meta = do
  let name = getStringLitName s
  let len = ByteString.length . UTF8.fromString $ s
  let ref = global (stringStructType len) name
  ref' <- bitcast ref ptrType
  callFn boxFuncType "box" [constIntOp 4, ref']

createPosition S.NoPosition = createStruct [constInt 0, constInt 0] -- Postion (0, 0) means No Position. Why not.
createPosition S.Position{S.sourceLine, S.sourceColumn} = createStruct [C.Int 32 (toInteger sourceLine), C.Int 32 (toInteger sourceColumn)]

positionStructType = T.StructureType False [T.i32, T.i32]

boxArray values = callFn boxFuncType "boxArray" (constIntOp (length values) : values)

boxFunc name mapping = do
  let idx = fromMaybe (error ("No such function " ++ name)) (Map.lookup name mapping)
  callFn (funcType ptrType [T.i32]) "boxFunc" [constIntOp idx]

boxError name = do
  modify (\s -> s { generatedStrings = name : generatedStrings s })
  let strLitName = getStringLitName name
  let len = length name
  let ref = global (stringStructType len) strLitName
  ref <- bitcast ref ptrType
  callFn (funcType ptrType [T.i32]) "boxError" [ref]

boxClosure :: String -> Map.Map String Int -> [S.Arg] -> Codegen AST.Operand
boxClosure name mapping enclosedVars = do
  syms <- gets symtab
  let idx = fromMaybe (error ("Couldn't find " ++ name ++ " in mapping:\n" ++ show mapping)) (Map.lookup name mapping)
  let argc = length enclosedVars
  let findArg n = fromMaybe (error ("Couldn't find " ++ n ++ " variable in symbols " ++ show syms)) (lookup n syms)
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
  callFn (funcType ptrType [T.i32, T.i32, ptrType]) "boxClosure" [constIntOp idx, constIntOp argc, sargsPtr]

boolToInt True = 1
boolToInt False = 0

declareStdFuncs = do
  external T.void  "initLascaRuntime" [("runtime", ptrType)] False []
  external ptrType "gcMalloc" [("size", intType)] False []
  external ptrType "box" [("t", intType), ("ptr", ptrType)] False [FA.GroupID 0]
  external ptrType "unbox" [("t", intType), ("ptr", ptrType)] False [FA.GroupID 0]
  external ptrType "boxError" [("n", ptrType)] False [FA.GroupID 0]
  external ptrType "boxInt" [("d", intType)] False [FA.GroupID 0]
  external ptrType "boxBool" [("d", intType)] False [FA.GroupID 0]
  external ptrType "boxFunc" [("id", intType)] False [FA.GroupID 0]
  external ptrType "boxClosure" [("id", intType), ("argc", intType), ("argv", ptrType)] False []
  external ptrType "boxFloat64" [("d", T.double)] False [FA.GroupID 0]
  external ptrType "boxArray" [("size", intType)] True [FA.GroupID 0]
  external ptrType "runtimeBinOp"  [("code",  intType), ("lhs",  ptrType), ("rhs", ptrType)] False [FA.GroupID 0]
  external ptrType "runtimeApply"  [("func", ptrType), ("argc", intType), ("argv", ptrType), ("pos", positionStructType)] False []
  external ptrType "runtimeSelect" [("tree", ptrType), ("expr", ptrType), ("pos", positionStructType)] False [FA.GroupID 0]
  external T.void  "initEnvironment" [("argc", intType), ("argv", ptrType)] False []
--  external ptrType "runtimeIsConstr" [("value", ptrType), ("name", ptrType)] False [FA.GroupID 0]
  addDefn $ AST.FunctionAttributes (FA.GroupID 0) [FA.ReadOnly]

extractLambda :: S.Expr -> LLVM S.Expr
extractLambda (S.Lam meta arg expr) = do
  state <- get
  let nms = _modNames state
  let syntactic = _syntacticAst state
  let outerVars = _outers state
  let usedOuterVars = Set.toList (Set.intersection outerVars (_usedVars state))
  let enclosedArgs = map (\n -> S.Arg n typeAny) usedOuterVars
  let (funcName, nms') = uniqueName "lambda" nms
  let func = S.Function meta funcName typeAny (enclosedArgs ++ [arg]) expr
  modify (\s -> s { _modNames = nms', _syntacticAst = syntactic ++ [func] })
--   Debug.traceM ("Generated lambda " ++ show func ++ ", outerVars = " ++ show outerVars ++ ", usedOuterVars" ++ show usedOuterVars)
  return (S.BoxFunc funcName enclosedArgs)
extractLambda expr@(S.Ident _ n) = do
  modify (\s -> s { _usedVars = Set.insert n (_usedVars s)})
  return expr
extractLambda expr = return expr

functionStructType = T.StructureType False [ptrType, ptrType, T.i32]
functionsStructType len = T.StructureType False [T.i32, arrayTpe len]
  where arrayTpe len = T.ArrayType len functionStructType

genTypesStruct defs = do
  types <- genData defs
  let array = C.Array ptrType types
  defineConst "Types" structType (Just (struct array))
  where len = length defs
        struct a = createStruct [constInt len, a]
        structType = T.StructureType False [T.i32, T.ArrayType (fromIntegral len) ptrType]

genData :: [S.DataDef] -> LLVM ([C.Constant])
genData defs = sequence [genDataStruct d | d <- defs]
  
  where genDataStruct dd@(S.DataDef tid name constrs) = do
          defineStringLit name
          let literalName = "Data." ++ name
          let numConstructors = length constrs
          constructors <- genConstructors dd
          let arrayOfConstructors = C.Array ptrType constructors
          let struct = createStruct [constInt tid, globalStringRefAsPtr name, constInt numConstructors, arrayOfConstructors] -- struct Data
          defineConst literalName (T.StructureType False [T.i32, ptrType, T.i32, T.ArrayType (fromIntegral numConstructors) ptrType]) (Just struct)
          return (constRef literalName)

genConstructors (S.DataDef tid name constrs) = do
  forM (zip constrs [0..]) $ \ ((S.DataConst n args), tag) ->
    defineConstructor name n tid tag args

boxStructType = T.StructureType False [T.i32, ptrType]

defineConstructor typeName name tid tag args = do
  -- TODO optimize for zero args
  modState <- get
  let codeGenResult = codeGen modState
  let blocks = createBlocks codeGenResult

  if null args
  then do
    let singletonName = name ++ ".Singleton"
    let dataValue = createStruct [constInt tag, C.Array ptrType []]
    defineConst singletonName tpe (Just dataValue)

    let boxed = createStruct [constInt tid, constRef singletonName]
    defineConst (singletonName ++ ".Boxed") boxStructType (Just boxed)

    let boxedRef = C.GlobalReference boxStructType (AST.Name (singletonName ++ ".Boxed"))
    let ptrRef = C.BitCast boxedRef ptrType
    defineConst (name) ptrType (Just ptrRef)
  else do
    define ptrType name fargs blocks -- define constructor function
    forM_ args $ \ (S.Arg name _) -> defineStringLit name -- define fields names as strings
  
  let structType = T.StructureType False [T.i32, ptrType, T.i32, T.ArrayType (fromIntegral len) ptrType]
  let struct =  createStruct [C.Int 32 (fromIntegral tid), globalStringRefAsPtr name, constInt len, fieldsArray]
  let literalName = typeName ++ "." ++ name
  defineConst literalName structType (Just struct)

  return $ constRef literalName
  
  where
    len = length args
    arrayType = T.ArrayType (fromIntegral len) ptrType
    tpe = T.StructureType False [T.i32, arrayType] -- DataValue: {tag, values: []}
    fargs = toSig args
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
        store p (local n)
      boxed <- callFn ptrType "box" [constIntOp tid, ptr]
      ret boxed

genFunctionMap :: [S.Expr] -> LLVM ()
genFunctionMap fns = do
  defineNames
  defineConst "Functions" (functionsStructType len) (Just struct1)
--   Debug.traceM (show mapping)
--   Debug.traceM (show array)
  modify (\s -> s { functions = mapping })
  where

    defineNames = mapM (\(name, _) -> defineStringLit name) funcsWithArities

    len = fromIntegral (length funcsWithArities)


--     structType = T.StructureType False [stringStructType, T.i32]

    array = C.Array functionStructType (fmap snd entries)

    mapping :: Map.Map String Int
    mapping = go 0 Map.empty entries where
      go i m ((name, _) : es) = go (i + 1) (Map.insert name i m) es
      go i m [] = m



    entries = fmap (\(name, arity) -> (name, struct name arity)) funcsWithArities

    struct1 = createStruct [C.Int 32 (toInteger len), array]

    struct name arity = createStruct
                            [globalStringRefAsPtr name, constRef name, constInt arity]


    funcsWithArities = foldl go [] fns where
      go s (S.Data _ name consts) =
        -- Add data constructors as global functions
        let m = foldl (\acc (S.DataConst n args) -> (n, length args) : acc) [] consts
        in m ++ s
      go s (S.Function _ name tpe args body) = (name, length args) : s
      go s (S.Extern name tpe args) = (name, length args) : s
      go s _ = s


runtimeStructType = T.StructureType False [ptrType, ptrType, boolType]

genRuntime opts = defineConst "Runtime" runtimeStructType (Just runtime)
  where
      runtime = createStruct [constRef "Functions", constRef "Types", constBool $ S.verboseMode opts]


