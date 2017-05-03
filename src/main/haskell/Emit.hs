{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Strict #-}

module Emit (
  Ctx, DataDef(..), dataDefs,
  codegenModule,
  createGlobalContext
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

data DataDef = DataDef Int String [S.DataConst]
  deriving (Show, Eq)

data Ctx = Context {
  _globalFunctions :: Set.Set String,
  _globalVals :: Set.Set String,
  dataDefs :: [DataDef],
  typeId :: Int -- TODO remove this. Needed for type id generation. Move to ModuleState?
} deriving (Show, Eq)

globalFunctions :: Lens.Lens' Ctx (Set.Set String)
globalFunctions = Lens.lens _globalFunctions (\c e -> c { _globalFunctions = e } )

globalVals :: Lens.Lens' Ctx (Set.Set String)
globalVals = Lens.lens _globalVals (\c e -> c { _globalVals = e } )

emptyCtx = Context {
  _globalFunctions = Set.empty,
  _globalVals = Set.empty,
  dataDefs = [],
  typeId = 1000
}

externArgsToSig :: [S.Arg] -> [(S.Name, AST.Type)]
externArgsToSig = map (\(S.Arg name tpe) -> (name, typeMapping tpe))

uncurryLambda expr = go expr ([], expr) where
  go (S.Lam name e) result = let (args, body) = go e result in (name : args, body)
  go e (args, _) = (args, e)



transform :: (S.Expr -> LLVM S.Expr) -> [S.Expr] -> LLVM [S.Expr]
transform transformer exprs = sequence [transformExpr transformer expr | expr <- exprs]

transformExpr :: (S.Expr -> LLVM S.Expr) -> S.Expr -> LLVM S.Expr
transformExpr transformer expr = case expr of
  (S.If cond true false) -> do
    cond' <- go cond
    true' <- go true
    false' <- go false
    transformer (S.If cond' true' false')
  (S.Let n e body) -> do
    modStateLocals %= Set.insert n
    e' <- go e
    body' <- go body
    transformer (S.Let n e' body')
  (S.Lam n e) -> do
    modify (\s -> s { _outers = _locals s } )
    modStateLocals .= Set.singleton n
    e' <- go e
    transformer (S.Lam n e')
  (S.Apply meta e args) -> do
    e' <- go e
    args' <- sequence [go arg | arg <- args]
    transformer (S.Apply meta e' args')
  (S.Function name tpe args e1) -> do
    let argNames = Set.fromList (map (\(S.Arg n _) -> n) args)
    modify (\s -> s { _locals = argNames, _outers = Set.empty, _usedVars = Set.empty } )
    e' <- go e1
    transformer (S.Function name tpe args e')
  e -> transformer e
  where go e = transformExpr transformer e



defineStringConstants :: S.Expr -> LLVM ()
defineStringConstants (S.Literal meta (S.StringLit s)) = defineStringLit s
defineStringConstants (S.If cond true false) = do
  defineStringConstants cond
  defineStringConstants true
  defineStringConstants false
  return ()
defineStringConstants (S.Select _ lhs rhs) = do
  defineStringConstants lhs
  defineStringConstants rhs
  return ()
defineStringConstants (S.Array exprs) = mapM_ defineStringConstants exprs
defineStringConstants (S.Apply meta _ exprs) = mapM_ defineStringConstants exprs
defineStringConstants (S.Let _ e body) = do
  defineStringConstants e
  defineStringConstants body
  return ()
defineStringConstants (S.Match e cases) = do
  defineStringConstants e
  mapM_ (\(S.Case p e) -> defineStringConstants e) cases
  return ()
defineStringConstants _ = return ()

-- codegenTop :: S.Expr -> LLVM ()
codegenTop ctx (S.Val name expr) = do
  modify (\s -> s { _globalValsInit = _globalValsInit s ++ [(name, expr)] })
  defineGlobal (AST.Name name) ptrType (Just (C.Null ptrType))

codegenTop ctx (S.Function name tpe args body) = do
  r1 <- defineStringConstants body
--   Debug.traceM ("Generating function1 " ++ name ++ (show r1))
--   defineClosures ctx name body
--   Debug.traceM ("Generating function2 " ++ name ++ (show r1))
  modState <- get
  let codeGenResult = codeGen modState
  let blocks = createBlocks codeGenResult
  mapM_ defineStringLit (generatedStrings codeGenResult)
  define ptrType name largs blocks
  where
    largs = toSig args
    codeGen modState = execCodegen [] modState $ do
      entry <- addBlock entryBlockName
      setBlock entry
--       Debug.traceM ("Generating function2 " ++ name)
      forM_ args $ \(S.Arg n t) -> do
        var <- alloca ptrType
        store var (local n)
        assign n var
      cgen ctx body >>= ret

codegenTop ctx (S.Data name constructors) = return ()


codegenTop _ (S.Extern name tpe args) = external llvmType name fnargs False []
  where
    llvmType = typeMapping tpe
    fnargs = externArgsToSig args

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
  define T.void "start" [(intType, AST.Name "argc"), (ptrType, AST.Name "argv")] (bls modState)
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
      store (global ptrType name) v
      return v


-- Dynamic mode
typeMapping :: Type -> AST.Type
-- FIXME currently we assume every function returns a result and can't be Unit/void
--typeMapping (TypeIdent "Unit") = T.void
typeMapping _ = ptrType
-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

binops :: Map.Map String Int
binops = Map.fromList [("+", 10), ("-", 11), ("*", 12), ("/", 13),
  ("==", 42), ("!=", 43), ("<", 44), ("<=", 45), (">=", 46), (">", 47)]


cgen :: Ctx -> S.Expr -> Codegen AST.Operand
cgen ctx (S.Let a b c) = do
  i <- alloca ptrType
  val <- cgen ctx b
  store i val
  assign a i
  cgen ctx c
cgen ctx (S.Ident name) = do
  syms <- gets symtab
  modState <- gets moduleState
  let mapping = functions modState
  case lookup name syms of
    Just x ->
--       Debug.trace ("Local " ++ show name)
      load x
    Nothing | name `Set.member` _globalFunctions ctx -> boxFunc name mapping
            | name `Set.member` _globalVals ctx -> load (global ptrType name)
            | otherwise -> boxError name
cgen ctx (S.Literal l meta) = do
--  Debug.traceM $ "Generating literal " ++ show l ++ " on " ++ show (S.pos meta)
  box meta l
cgen ctx (S.Array exprs) = do
  vs <- values
  boxArray vs
  where values = sequence [cgen ctx e | e <- exprs]
cgen ctx (S.Select meta tree expr) = do
  tree <- cgen ctx tree
  e <- cgen ctx expr
  let pos = createPosition $ S.pos meta
  callFn runtimeSelectFuncType "runtimeSelect" [tree, e, constOp pos]
cgen ctx (S.Apply meta (S.Ident "or") [lhs, rhs]) = cgen ctx (S.If lhs (S.Literal S.emptyMeta (S.BoolLit True)) rhs)
cgen ctx (S.Apply meta (S.Ident "and") [lhs, rhs]) = cgen ctx (S.If lhs rhs (S.Literal S.emptyMeta (S.BoolLit False)))
cgen ctx (S.Apply meta (S.Ident fn) [lhs, rhs]) | fn `Map.member` binops = do
  llhs <- cgen ctx lhs
  lrhs <- cgen ctx rhs
  let code = fromMaybe (error ("Couldn't find binop " ++ fn)) (Map.lookup fn binops)
  let codeOp = constIntOp code
  callFn runtimeBinOpFuncType "runtimeBinOp" [codeOp, llhs, lrhs]
cgen ctx (S.Apply meta expr args) = do
  syms <- gets symtab
  largs <- mapM (cgen ctx) args
  let symMap = Map.fromList syms
  let isGlobal fn = (fn `Set.member` _globalFunctions ctx) && not (fn `Map.member` symMap)
  case expr of
     -- TODO Here are BUGZZZZ!!!! :)
     -- TODO check arguments!
     -- this is done to speed-up calls if you `a global function
    S.Ident fn | isGlobal fn -> callFn ptrType fn largs
    expr -> do
      modState <- gets moduleState
      e <- cgen ctx expr
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
cgen ctx (S.BoxFunc funcName enclosedVars) = do
  modState <- gets moduleState
  let mapping = functions modState
  if null enclosedVars then boxFunc funcName mapping
  else boxClosure funcName mapping enclosedVars
cgen ctx m@(S.Match expr cases) = do
  let result = genMatch ctx m
--  Debug.traceM $ "Generated " ++ show result
  cgen ctx result
cgen ctx (S.If cond tr fl) = do
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
  phi ptrType [(trval, ifthen), (flval, ifelse)]

cgen ctx e = error ("cgen shit " ++ show e)

genMatch :: Ctx -> S.Expr -> S.Expr
genMatch ctx m@(S.Match expr []) = error $ "Should be at least on case in match expression: " ++ show m
genMatch ctx (S.Match expr cases) = foldr (\(S.Case p e) acc -> genPattern ctx expr p e acc) genFail cases

genFail = S.Apply S.emptyMeta (S.Ident "die") [S.Literal S.emptyMeta $ S.StringLit "Match error!"]

genPattern ctx lhs S.WildcardPattern rhs = const rhs
genPattern ctx lhs (S.VarPattern name) rhs = const (S.Let name lhs rhs)
genPattern ctx lhs (S.LitPattern literal) rhs = S.If (S.Apply S.emptyMeta (S.Ident "==") [lhs, S.Literal S.emptyMeta literal]) rhs
genPattern ctx lhs (S.ConstrPattern name args) rhs = cond
  where cond fail = S.If constrCheck (checkArgs name fail) fail
        constrCheck = S.Apply S.emptyMeta (S.Ident "runtimeIsConstr") [lhs, S.Literal S.emptyMeta $ S.StringLit name]
        constrMap = let cs = foldr (\ (DataDef dn id constrs) acc -> constrs ++ acc) [] (dataDefs ctx)
                        tuples = fmap (\c@(S.DataConst n args) -> (n, args)) cs
                    in  Map.fromList tuples
        checkArgs nm fail =  case Map.lookup nm constrMap of
                            Nothing -> fail
                            Just constrArgs | length args == length constrArgs -> do
                              let argParam = zip args constrArgs
                              foldr (\(a, S.Arg n _) acc -> genPattern ctx (S.Select S.emptyMeta lhs (S.Ident n)) a acc fail) rhs argParam
                            Just constrArgs -> error (printf "Constructor %s has %d parameters, but %d given" nm (length constrArgs) (length args)) -- TODO box this error
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

gcMalloc size = callFn (funcType ptrType [T.i32]) "gcMalloc" [constIntOp size]

box (S.BoolLit b) meta = callFn boxFuncType "boxBool" [constIntOp (boolToInt b)]
box (S.IntLit  n) meta = callFn boxFuncType "boxInt" [constIntOp n]
box (S.FloatLit  n) meta = callFn boxFuncType "boxFloat64" [constFloat n]
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
  sargsPtr <- gcMalloc (ptrSize * argc)
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

codegenModule :: S.LascaOpts -> AST.Module -> [S.Expr] -> AST.Module
codegenModule opts modo exprs = modul
    where
        ctx = createGlobalContext exprs
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
          let defs = reverse (dataDefs ctx)
          genTypesStruct defs
          genRuntime opts
          defineStringLit "Match error!" -- TODO remove this hack
          mapM_ (codegenTop ctx) fns''
          codegenStartFunc ctx

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
extractLambda (S.Lam name expr) = do
  state <- get
  let nms = _modNames state
  let syntactic = _syntacticAst state
  let outerVars = _outers state
  let usedOuterVars = Set.toList (Set.intersection outerVars (_usedVars state))
  let enclosedArgs = map (\n -> S.Arg n typeAny) usedOuterVars
  let (funcName, nms') = uniqueName "lambda" nms
  let func = S.Function funcName typeAny (enclosedArgs ++ [S.Arg name typeAny]) expr
  modify (\s -> s { _modNames = nms', _syntacticAst = syntactic ++ [func] })
--   Debug.traceM ("Generated lambda " ++ show func ++ ", outerVars = " ++ show outerVars ++ ", usedOuterVars" ++ show usedOuterVars)
  return (S.BoxFunc funcName enclosedArgs)
extractLambda expr@(S.Ident n) = do
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

genData :: [DataDef] -> LLVM ([C.Constant])
genData defs = sequence [genDataStruct d | d <- defs]
  
  where genDataStruct dd@(DataDef tid name constrs) = do
          defineStringLit name
          let literalName = "Data." ++ name
          let numConstructors = length constrs
          constructors <- genConstructors dd
          let arrayOfConstructors = C.Array ptrType constructors
          let struct = createStruct [constInt tid, globalStringRefAsPtr name, constInt numConstructors, arrayOfConstructors] -- struct Data
          defineConst literalName (T.StructureType False [T.i32, ptrType, T.i32, T.ArrayType (fromIntegral numConstructors) ptrType]) (Just struct)
          return (constRef literalName)

genConstructors (DataDef tid name constrs) = do
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
      nullptr <- getelementptr (constOp (constNull tpe)) [constIntOp 1]
      sizeof <- ptrtoint nullptr T.i32 -- FIXME change to T.i64?
      ptr <- callFn ptrType "gcMalloc" [sizeof]
      structPtr <- bitcast ptr (T.ptr tpe)
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
      go s (S.Data name consts) =
        -- Add data constructors as global functions
        let m = foldl (\acc (S.DataConst n args) -> (n, length args) : acc) [] consts
        in m ++ s
      go s (S.Function name tpe args body) = (name, length args) : s
      go s (S.Extern name tpe args) = (name, length args) : s
      go s _ = s


runtimeStructType = T.StructureType False [ptrType, ptrType, boolType]

genRuntime opts = defineConst "Runtime" runtimeStructType (Just runtime)
  where
      runtime = createStruct [constRef "Functions", constRef "Types", constBool $ S.verboseMode opts]

createGlobalContext :: [S.Expr] -> Ctx
createGlobalContext exprs = execState (loop exprs) emptyCtx
  where
      loop [] = return ()
      loop (e:exprs) = do
        names e
        loop exprs

      names :: S.Expr -> State Ctx ()
      names (S.Val name _) = globalVals %= Set.insert name
      names (S.Function name _ _ _) = globalFunctions %= Set.insert name
      names (S.Data name consts) = do
        id <- gets typeId
        let dataDef = DataDef id name consts
        let (funcs, vals) = foldl (\(funcs, vals) (S.DataConst n args) ->
                              if null args
                              then (funcs, n : vals)
                              else (n : funcs, vals)) ([], []) consts
        modify (\s -> s { dataDefs =  dataDef : dataDefs s, typeId = id + 1 })
        globalVals %= Set.union (Set.fromList vals)
        globalFunctions %= Set.union (Set.fromList funcs)
      names (S.Extern name _ _) = globalFunctions %= Set.insert name
      names expr = error $ "Wat? Expected toplevel expression, but got " ++ show expr
