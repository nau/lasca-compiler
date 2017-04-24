{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Emit (
  codegenModule
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
  globalFunctions :: Set.Set String,
  globalVals :: Set.Set String,
  dataDefs :: [DataDef],
  typeId :: Int -- TODO remove this. Needed for type id generation. Move to ModuleState?
} deriving (Show, Eq)

emptyCtx = Context {
  globalFunctions = Set.empty,
  globalVals = Set.empty,
  dataDefs = [],
  typeId = 1000
}

one = constant $ C.Float (F.Double 1.0)
zero = constant $ C.Float (F.Double 0.0)
false = zero
true = one

externArgsToSig :: [S.Arg] -> [(S.Name, AST.Type)]
externArgsToSig = map (\(S.Arg name tpe) -> (name, typeMapping tpe))

toSig :: [S.Arg] -> [(AST.Type, AST.Name)]
toSig = map (\(S.Arg name tpe) -> (ptrType, AST.Name name))


stringStructType len = T.StructureType False [T.i32, T.ArrayType (fromIntegral len) T.i8]
applyStructType len = T.StructureType False [T.i32, T.ArrayType (fromIntegral len) T.i8]


getStringLitASTName s = AST.Name (getStringLitName s)

getStringLitName s = name
  where
    name = take 15 s ++ "." ++ show hash
    hash = hash32 s

createString s = (C.Struct Nothing False [C.Int 32 (toInteger len), C.Array T.i8 bytes], len)
  where
    bytestring = UTF8.fromString s
    constByte b = C.Int 8 (toInteger b)
    bytes = map constByte (ByteString.unpack bytestring)
    len = ByteString.length bytestring

defineStringLit :: String -> LLVM ()
defineStringLit s = addDefn $ AST.GlobalDefinition $ AST.globalVariableDefaults {
                          LLVM.AST.Global.name        = getStringLitASTName s
                        , LLVM.AST.Global.isConstant  = True
                        , LLVM.AST.Global.type' = stringStructType len
                        , LLVM.AST.Global.initializer = Just string
                        }
  where
    (string, len) = createString s

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
    modify (\s -> s { _locals = Set.insert n (_locals s) } )
    e' <- go e
    body' <- go body
    transformer (S.Let n e' body')
  (S.Lam n e) -> do
    modify (\s -> s { _outers = _locals s } )
    modify (\s -> s { _locals = Set.singleton n } )
    e' <- go e
    transformer (S.Lam n e')
  (S.Apply e args) -> do
    e' <- go e
    args' <- sequence [go arg | arg <- args]
    transformer (S.Apply e' args')
  (S.Function name tpe args e1) -> do
    let argNames = Set.fromList (map (\(S.Arg n _) -> n) args)
    modify (\s -> s { _locals = argNames, _outers = Set.empty, _usedVars = Set.empty } )
    e' <- go e1
    transformer (S.Function name tpe args e')
  e -> transformer e
  where go e = transformExpr transformer e



defineStringConstants :: S.Expr -> LLVM ()
defineStringConstants (S.Literal (S.StringLit s)) = defineStringLit s
defineStringConstants (S.If cond true false) = do
  defineStringConstants cond
  defineStringConstants true
  defineStringConstants false
  return ()
defineStringConstants (S.Apply _ exprs) = do
  mapM_ defineStringConstants exprs
  return ()
defineStringConstants (S.Let _ e body) = do
  defineStringConstants e
  defineStringConstants body
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
  define retType name largs blocks
  where
    largs = toSig args

    retType = ptrType -- typeMapping tpe

    codeGen modState = execCodegen [] modState $ do
      entry <- addBlock entryBlockName
      setBlock entry
--       Debug.traceM ("Generating function2 " ++ name)
      forM_ args $ \(S.Arg n t) -> do
        var <- alloca ptrType
        store var (local (AST.Name n))
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
        call (global initLascaRuntimeFuncType (AST.Name "initLascaRuntime")) []
        call (global (funcType T.void [intType, ptrType]) (AST.Name "initEnvironment")) [local (AST.Name "argc"), local (AST.Name "argv")]
        initGlobals
        call (global mainFuncType (AST.Name "main")) []
        terminator $ I.Do $ I.Ret Nothing []
        return ()

    initGlobals = do
      modState <- gets moduleState
      let globalValsInit = _globalValsInit modState
      mapM gen globalValsInit

    gen (name, expr) = do
      v <- cgen ctx expr
      store (global ptrType (AST.Name name)) v
      return v


-- Static mode
typeMapping :: Type -> AST.Type
typeMapping (TypeIdent "Any") = ptrType
typeMapping (TypeIdent "Unit") = T.void
typeMapping (TypeIdent "Bool") = T.i1
typeMapping (TypeIdent "Int") = T.i32
typeMapping (TypeIdent "Float") = T.double
typeMapping (TypeIdent "String") = ptrType

-- Dynamic mode
-- typeMapping _ = typeInfoType
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
cgen ctx (S.Var name) = do
  syms <- gets symtab
  modState <- gets moduleState
  let mapping = functions modState
  case lookup name syms of
    Just x ->
--       Debug.trace ("Local " ++ show name)
      load x
    Nothing | name `Set.member` globalFunctions ctx -> boxFunc name mapping
            | name `Set.member` globalVals ctx -> load (global ptrType (AST.Name name))
            | otherwise -> boxError name
cgen ctx (S.Literal l) = box l
cgen ctx (S.Array exprs) = do
  vs <- values
  boxArray vs
  where values = sequence [cgen ctx e | e <- exprs]
cgen ctx (S.Select tree expr) = do
  tree <- cgen ctx tree
  e <- cgen ctx expr
  let funcs = constRefOperand "Functions"
  let structs = constRefOperand "Structs"
  call (global runtimeSelectFuncType (AST.Name "runtimeSelect")) [funcs, structs, tree, e]
cgen ctx (S.Apply (S.Var "or") [lhs, rhs]) = cgen ctx (S.If lhs (S.Literal (S.BoolLit True)) rhs)
cgen ctx (S.Apply (S.Var "and") [lhs, rhs]) = cgen ctx (S.If lhs rhs (S.Literal (S.BoolLit False)))
cgen ctx (S.Apply (S.Var fn) [lhs, rhs]) | fn `Map.member` binops = do
  llhs <- cgen ctx lhs
  lrhs <- cgen ctx rhs
  let code = fromMaybe (error ("Couldn't find binop " ++ fn)) (Map.lookup fn binops)
  let codeOp = constInt code
  call (global runtimeBinOpFuncType (AST.Name "runtimeBinOp")) [codeOp, llhs, lrhs]
cgen ctx (S.Apply expr args) = do
  syms <- gets symtab
  largs <- mapM (cgen ctx) args
  let symMap = Map.fromList syms
  let isGlobal fn = (fn `Set.member` globalFunctions ctx) && not (fn `Map.member` symMap)
  case expr of
     -- TODO Here are BUGZZZZ!!!! :)
     -- TODO check arguments!
     -- this is done to speed-up calls if you call a global function
    S.Var fn | isGlobal fn -> call (global ptrType (AST.Name fn)) largs
    expr -> do
      modState <- gets moduleState
      e <- cgen ctx expr
      let funcs = functions modState
      let argc = constInt (length largs)
      let len = Map.size funcs
      let funcs = constRefOperand "Functions"
      sargsPtr <- allocaSize ptrType argc
      let asdf (idx, arg) = do
            p <- getelementptr sargsPtr [idx]
            store p arg
      sargs <- bitcast sargsPtr ptrType -- runtimeApply accepts i8*, so need to bitcast. Remove when possible
      -- cdecl calling convension, arguments passed right to left
      sequence_ [asdf (constInt i, a) | (i, a) <- zip [0 .. len] largs]
      call (global runtimeApplyFuncType (AST.Name "runtimeApply")) [funcs, e, argc, sargs]
--   call e largs
cgen ctx (S.BoxFunc funcName enclosedVars) = do
  modState <- gets moduleState
  let mapping = functions modState
  if null enclosedVars then boxFunc funcName mapping
  else boxClosure funcName mapping enclosedVars
cgen ctx (S.If cond tr fl) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"
  -- %entry
  ------------------
  cond <- cgen ctx cond
  -- unbox Bool
  voidPtrCond <- call (global unboxFuncType (AST.Name "unbox")) [constInt 1, cond]
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

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------
funcType retTy args = T.FunctionType retTy args False

initLascaRuntimeFuncType = funcType T.void []
mainFuncType = funcType ptrType []
boxFuncType = funcType ptrType [T.i32]
boxArrayFuncType = T.FunctionType ptrType [T.i32] True
runtimeBinOpFuncType = funcType ptrType [T.i32, ptrType, ptrType]
runtimeApplyFuncType = funcType ptrType [ptrType, ptrType, T.i32, ptrType]
runtimeSelectFuncType = funcType ptrType [ptrType, ptrType, ptrType, ptrType]
unboxFuncType = funcType ptrType [ptrType, T.i32]

gcMalloc size = call (global (funcType ptrType [T.i32]) (AST.Name "gcMalloc")) [constInt size]

box :: S.Lit -> Codegen AST.Operand
box (S.BoolLit b) = call (global boxFuncType (AST.Name "boxBool")) [constInt (boolToInt b)]
box (S.IntLit  n) = call (global boxFuncType (AST.Name "boxInt")) [constInt n]
box (S.FloatLit  n) = call (global boxFuncType (AST.Name "boxFloat64")) [constFloat n]
box S.UnitLit = call (global boxFuncType (AST.Name "box")) [constInt 0, constInt 0]
box (S.StringLit s) = do
  let name = getStringLitASTName s
  let len = ByteString.length . UTF8.fromString $ s
  let ref = global (stringStructType len) name
  ref' <- bitcast ref ptrType
  call (global boxFuncType (AST.Name "box")) [constInt 4, ref']

boxArray values = call (global boxFuncType (AST.Name "boxArray")) (constInt (length values) : values)

boxFunc name mapping = do
  let idx = fromMaybe (error ("No such function " ++ name)) (Map.lookup name mapping)
  call (global (funcType ptrType [T.i32]) (AST.Name "boxFunc")) [constInt idx]

boxError name = do
  modify (\s -> s { generatedStrings = name : generatedStrings s })
  let strLitName = getStringLitASTName name
  let len = length name
  let ref = global (stringStructType len) strLitName
  ref <- bitcast ref ptrType
  call (global (funcType ptrType [T.i32]) (AST.Name "boxError")) [ref]

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
  sequence_ [asdf (constInt i, a) | (i, a) <- zip [0 .. argc] args]
  call (global (funcType ptrType [T.i32, T.i32, ptrType]) (AST.Name "boxClosure"))
    [constInt idx, constInt argc, sargsPtr]

boolToInt True = 1
boolToInt False = 0

codegenModule :: AST.Module -> [S.Expr] -> AST.Module
codegenModule modo exprs = modul
    where
        ctx = createGlobalContext exprs
        modul = runLLVM modo genModule
        genModule = do
          st <- gets _llvmModule
          fns' <- transform extractLambda exprs
          syn <- gets _syntacticAst
          let fns'' = fns' ++ syn
          st' <- gets _llvmModule
--           Debug.traceM ("Rewritten exprs: " ++ show fns'')
--           Debug.traceM ("Rewritten exprs: " ++ show st')
--           Debug.traceM ("Rewritten exprs: " ++ show st')
          declareStdFuncs
          genFunctionMap fns''
          let structs = reverse (dataDefs ctx)
          genStructs structs
          mapM_ (codegenTop ctx) fns''
          codegenStartFunc ctx

declareStdFuncs = do
  external T.void "initLascaRuntime" [] False []
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
  external ptrType "runtimeApply"  [("funcs", ptrType), ("func", ptrType), ("argc", intType), ("argv", ptrType)] False []
  external ptrType "runtimeSelect" [("funcs", ptrType), ("structs", ptrType), ("tree", ptrType), ("expr", ptrType)] False [FA.GroupID 0]
  external T.void  "initEnvironment" [("argc", intType), ("argv", ptrType)] False []
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
extractLambda expr@(S.Var n) = do
  modify (\s -> s { _usedVars = Set.insert n (_usedVars s)})
  return expr
extractLambda expr = return expr

createStruct (DataDef tid name [S.DataConst n args]) =
--  C.Struct Nothing False [C.Int 32 (fromIntegral tid), globalStringRefAsPtr name, C.Int 32 (fromIntegral len)]
  C.Struct Nothing False [C.Int 32 (fromIntegral tid), globalStringRefAsPtr name, C.Int 32 (fromIntegral len), fieldsArray]
  where
    len = length args
    fieldsArray = C.Array ptrType fields
    fields = map (\(S.Arg n _) -> globalStringRefAsPtr n) args

defineStructs defs =
  forM_ defs $ \d@(DataDef _ name constrs) -> do
    let struct = createStruct d
    let (S.DataConst _ args) = head constrs
    let len = length args
    defineConst (AST.Name ("Struct." ++ name)) (T.StructureType False [T.i32, ptrType, T.i32, T.ArrayType (fromIntegral len) ptrType]) (Just struct)

createStructs structs = C.Struct Nothing False [C.Int 32 (toInteger len), array]
  where
    len = length structs
    array = C.Array ptrType refs
    refs = fmap (\(DataDef _ name _) -> constRef ("Struct." ++ name)) structs

functionStructType = T.StructureType False [ptrType, ptrType, T.i32]
functionsStructType len = T.StructureType False [T.i32, arrayTpe len]

arrayTpe len = T.ArrayType len functionStructType

globalStringRef name = C.GlobalReference (stringStructType (length name)) (getStringLitASTName name)
globalStringRefAsPtr name = C.BitCast (globalStringRef name) ptrType

genStructs defs = do
  let structs = createStructs defs
  let len = length defs
  defineNames defs
  defineStructs defs
  let structs = createStructs defs
  defineConst (AST.Name "Structs") (T.StructureType False [T.i32, T.ArrayType (fromIntegral len) ptrType]) (Just structs)
  forM_ defs $ \(DataDef tid name constrs)  -> forM_ constrs $ \ (S.DataConst _ args) -> 
    defineConstructor name tid args
  where
    defineNames defs =
     forM_ defs $ \(DataDef _ name constrs) -> forM_ constrs $
      \ (S.DataConst name args) ->
        do defineStringLit name
           forM_ args $ \ (S.Arg name _) -> defineStringLit name
           
    defineConstructor name tid args = do
          addDefn $ AST.TypeDefinition (AST.Name "Field") Nothing
          addDefn $ AST.TypeDefinition structTypeName (Just tpe)
          modState <- get
          let codeGenResult = codeGen modState
          let blocks = createBlocks codeGenResult
          define ptrType name fargs blocks -- define constructor function
          where
            structTypeName = AST.Name (name ++ "Struct")
            largs = map (\(S.Arg _ t) -> ptrType) args
            tpe = T.StructureType False largs
            fargs = toSig args

            codeGen modState = execCodegen [] modState $ do
              entry <- addBlock entryBlockName
              setBlock entry
--              nullptr <- getelementptr (constNull tpe) [constInt 1]
--              sizeof <- ptrtoint nullptr T.i32 -- FIXME change to T.i64?
              let len = length args
              ptr <- call (global ptrType (AST.Name "gcMalloc")) [constInt (ptrSize * len)]
              arrayPtr <- bitcast ptr (T.ptr (T.ArrayType (fromIntegral len) ptrType))
              let argsWithId = zip args [0..]
              forM_ argsWithId $ \(S.Arg n t, i) -> do
                p <- getelementptr arrayPtr [constInt 0, constInt i]
                store p (local (AST.Name n))
              boxed <- call (global ptrType (AST.Name "box")) [constInt tid, ptr]
              ret boxed

genFunctionMap :: [S.Expr] -> LLVM ()
genFunctionMap fns = do
  defineNames
  defineConst (AST.Name "Functions") (functionsStructType len) (Just struct1)
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

    struct1 = C.Struct Nothing False [C.Int 32 (toInteger len), array]

    struct name arity = C.Struct Nothing False
                            [globalStringRefAsPtr name, constRef name, C.Int 32 (toInteger arity)]


    funcsWithArities = foldl go [] fns where
      go s (S.Data name consts) =
        -- Add data constructors as global functions
        let m = foldl (\acc (S.DataConst n args) -> (n, length args) : acc) [] consts
        in m ++ s
      go s (S.Function name tpe args body) = (name, length args) : s
      go s (S.Extern name tpe args) = (name, length args) : s
      go s _ = s



createGlobalContext :: [S.Expr] -> Ctx
createGlobalContext exprs = execState (loop exprs) emptyCtx
  where
      loop [] = return ()
      loop (e:exprs) = do
        names e
        loop exprs

      names :: S.Expr -> State Ctx ()
      names (S.Val name _) = do
        globVals <- gets globalVals
        modify (\s -> s { globalVals = Set.insert name globVals } )
      names (S.Function name _ _ _) = do
        globFuncs <- gets globalFunctions
        modify (\s -> s { globalFunctions = Set.insert name globFuncs } )
      names (S.Data name consts) = do
        id <- gets typeId
        globFuncs <- gets globalFunctions
        let dataDef = DataDef id name consts
        let m = foldl (\acc (S.DataConst n _) -> n : acc) [] consts
        modify (\s -> s {
          dataDefs =  dataDef : dataDefs s,
          globalFunctions = globFuncs `Set.union` Set.fromList m,
          typeId = id + 1
          })
      names (S.Extern name _ _) = do
        globFuncs <- gets globalFunctions
        modify (\s -> s { globalFunctions = Set.insert name globFuncs } )
