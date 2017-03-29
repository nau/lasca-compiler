--------------------------------------------------------------------
-- |
-- Module    :  Emit
-- Copyright :  (c) Stephen Diehl 2013
-- License   :  MIT
-- Maintainer:  stephen.m.diehl@gmail.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Emit where

import LLVM.General.Module
import LLVM.General.Context
import LLVM.General.Analysis
import LLVM.General.PassManager

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Global
import qualified LLVM.General.AST.Type as T
import qualified LLVM.General.AST.Instruction as I
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.IntegerPredicate as IP
import qualified LLVM.General.AST.FloatingPointPredicate as FP

-- import qualified Data.Text as Text
import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as Encoding
import qualified Data.ByteString.UTF8 as UTF8

import LLVM.General.ExecutionEngine ( withMCJIT, withModuleInEngine, getFunction )

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
import qualified Debug.Trace as Debug

import Codegen
import Type
import JIT (runJIT)
import qualified Syntax as S

-- data Ctx = Context
type Ctx = Set.Set String

one = constant $ C.Float (F.Double 1.0)
zero = constant $ C.Float (F.Double 0.0)
false = zero
true = one

externArgsToSig :: [S.Arg] -> [(AST.Type, AST.Name)]
externArgsToSig = map (\(S.Arg name tpe) -> (typeMapping tpe, AST.Name name))

toSig :: [S.Arg] -> [(AST.Type, AST.Name)]
toSig = map (\(S.Arg name tpe) -> (ptrType, AST.Name name))


stringStructType len = T.StructureType False [T.i32, T.ArrayType (fromIntegral len) T.i8]
applyStructType len = T.StructureType False [T.i32, T.ArrayType (fromIntegral len) T.i8]


getStringLitASTName s = AST.Name (getStringLitName s)

getStringLitName s = name
  where
    name = (take 15 s) ++ "." ++ (show hash)
    hash = hash32 s

defineStringLit :: String -> LLVM ()
defineStringLit s = do  addDefn $ AST.GlobalDefinition $ AST.globalVariableDefaults {
                          LLVM.General.AST.Global.name        = getStringLitASTName s
                        , LLVM.General.AST.Global.isConstant  = True
                        , LLVM.General.AST.Global.type' = stringStructType len
                        , LLVM.General.AST.Global.initializer = Just (C.Struct (Nothing) False [C.Int 32 (toInteger len), C.Array T.i8 bytes])
                        }
  where
    bytestring = UTF8.fromString s
    constByte b = C.Int 8 (toInteger b)
    bytes = map constByte (ByteString.unpack bytestring)
    len = ByteString.length bytestring


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
    modify (\s -> s { _locals = (Set.insert n (_locals s)) } )
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
  e -> do
    transformer e
  where go e = transformExpr transformer e



defineStringConstants :: S.Expr -> LLVM ()
defineStringConstants (S.Literal (S.StringLit s)) = defineStringLit s
defineStringConstants (S.If cond true false) = do
  defineStringConstants cond
  defineStringConstants true
  defineStringConstants false
  return ()
defineStringConstants (S.Apply _ exprs) = do
  mapM defineStringConstants exprs
  return ()
defineStringConstants (S.Let _ e body) = do
  defineStringConstants e
  defineStringConstants body
  return ()
defineStringConstants _ = return ()

-- codegenTop :: S.Expr -> LLVM ()
codegenTop ctx (S.Data name args) = return ()

codegenTop ctx (S.Function name tpe args body) = do
  r1 <- defineStringConstants body
--   Debug.traceM ("Generating function1 " ++ name ++ (show r1))
--   defineClosures ctx name body
--   Debug.traceM ("Generating function2 " ++ name ++ (show r1))
  modState <- get
  define retType name largs (bls modState)
  where
    largs = toSig args
    retType = ptrType -- typeMapping tpe
    bls modState = createBlocks $ execCodegen [] modState $ do
      entry <- addBlock entryBlockName
      setBlock entry
--       Debug.traceM ("Generating function2 " ++ name)
      forM args $ \(S.Arg n t) -> do
--         var <- alloca (typeMapping t)   -- FIXME: this shouldn't be necessary
        var <- alloca ptrType
        store var (local (AST.Name n))
        assign n var
      cgen ctx body >>= ret

codegenTop _ (S.Extern name tpe args) = do
--   s <- gets _llvmModule
--   Debug.traceM("External function " ++ name)
--   Debug.traceM ("External function " ++ show s)
  external llvmType name fnargs
  where
    h s = show s
    llvmType = typeMapping tpe
    fnargs = externArgsToSig args

codegenTop ctx exp = do
  modState <- get
  define double "main" [] (bls modState)
  where
    bls modState = createBlocks $ execCodegen [] modState $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen ctx exp >>= ret

codegenStartFunc = do
  modState <- get
  define T.void "start" [] (bls modState)
  where
    bls modState = createBlocks $ execCodegen [] modState $ do
        entry <- addBlock entryBlockName
        setBlock entry
        call (global initLascaRuntimeFuncType (AST.Name "initLascaRuntime")) []
        call (global mainFuncType (AST.Name "main")) []
        terminator $ I.Do $ I.Ret (Nothing) []
        return ()


-- Static mode
typeMapping :: Type -> AST.Type
typeMapping (TypeIdent "Any") = ptrType
typeMapping (TypeIdent "Unit") = T.void
typeMapping (TypeIdent "Bool") = T.i1
typeMapping (TypeIdent "Int") = T.i32
typeMapping (TypeIdent "Float64") = T.double
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
  i <- alloca (T.ptr T.i8)
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
    Nothing {-| x `Set.member` ctx-} -> do
--       Debug.traceM ("Global " ++ show name)
      boxFunc name mapping
cgen ctx (S.Literal l) = box l
cgen ctx (S.Apply (S.Var "seq") [elm]) = cgen ctx elm
cgen ctx (S.Apply (S.Var "or") [lhs, rhs]) = cgen ctx (S.If lhs (S.Literal (S.BoolLit True)) rhs)
cgen ctx (S.Apply (S.Var "and") [lhs, rhs]) = cgen ctx (S.If lhs rhs (S.Literal (S.BoolLit False)))
cgen ctx (S.Apply (S.Var fn) [lhs, rhs]) | fn `Map.member` binops = do
  llhs <- cgen ctx lhs
  lrhs <- cgen ctx rhs
  let code = constInt (binops Map.! fn)
  call (global runtimeBinOpFuncType (AST.Name "runtimeBinOp")) [code, llhs, lrhs]
{-cgen ctx (S.Apply (S.Var fn) args) = do
  largs <- mapM (cgen ctx) args
  syms <- gets symtab
  let func = lookup fn syms
  Debug.traceM ("Found11 " ++ (show func))
  Debug.traceM ("symtab " ++ (show syms))
  case func of
    Just x -> do
      let arglist = map (\x -> ptrType) args
      x1 <- load x
      call (global runtimeApplyFuncType (AST.Name "runtimeApply")) (x1 : largs)
    Nothing -> call (global ptrType (AST.Name fn)) largs-}
cgen ctx (S.Apply expr args) = do
  modState <- gets moduleState
  e <- cgen ctx expr
  largs <- mapM (cgen ctx) args
  let funcs = functions modState
  let argc = constInt (length largs)
  let len = Map.size funcs
  let funcs = constRefOperand "Functions"
--   let argArray = C.Array (T.ArrayType (fromIntegral len) ptrType) largs
--   let funcsize = global T.i32 "FunctionsSize"
  let funcsize = constInt len
  sargsPtr <- allocaSize (ptrType) argc
  let asdf = (\(idx, arg) -> do {
    p <- getelementptr sargsPtr [idx];
    store p arg})
  sargs <- bitcast sargsPtr ptrType -- runtimeApply accepts i8*, so need to bitcast. Remove when possible
  -- cdecl calling convension, arguments passed right to left
  sequence [asdf (constInt i, a) | (i, a) <- (zip [0..len] (largs))]
  call (global runtimeApplyFuncType (AST.Name "runtimeApply")) ([funcs, e, argc, sargs])
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
  voidPtrCond <- call (global unboxFuncType (AST.Name "unbox")) [cond, constInt 1]
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
runtimeBinOpFuncType = funcType ptrType [T.i32, ptrType, ptrType]
runtimeApplyFuncType = funcType ptrType [ptrType, ptrType, T.i32, ptrType]
unboxFuncType = funcType ptrType [ptrType, T.i32]

gcMalloc size = call (global (funcType ptrType [T.i32]) (AST.Name "gcMalloc")) [constInt size]

box :: S.Lit -> Codegen AST.Operand
box (S.BoolLit b) = call (global boxFuncType (AST.Name "boxBool")) [constInt (boolToInt b)]
box (S.IntLit  n) = call (global boxFuncType (AST.Name "boxInt")) [constInt n]
box (S.UnitLit) = call (global boxFuncType (AST.Name "box")) [constInt 0, constInt 0]
box (S.StringLit s) = do
  let name = getStringLitASTName s
  let len = ByteString.length . UTF8.fromString $ s
  let ref = global (stringStructType len) name
  ref' <- bitcast ref ptrType
  call (global boxFuncType (AST.Name "box")) [constInt 4, ref']

boxFunc name mapping = do
  let idx = mapping Map.! name
  call (global (funcType ptrType [T.i32]) (AST.Name "boxFunc")) [constInt idx]

boxClosure :: String -> Map.Map String Int -> [S.Arg] -> Codegen AST.Operand
boxClosure name mapping enclosedVars = do
  syms <- gets symtab
  let symMap = Map.fromList syms
  let idx = mapping Map.! name
  let argc = length enclosedVars
  let args = map (\(S.Arg n _) -> symMap Map.! n) enclosedVars
  sargsPtr <- gcMalloc (ptrSize * argc)
  sargsPtr1 <- bitcast sargsPtr (T.ptr ptrType)
  let asdf = (\(idx, arg) -> do {
    p <- getelementptr sargsPtr1 [idx];
    bc1 <- bitcast p (T.ptr ptrType);
    bc <- load arg;
    store bc1 bc
  })
  let sargs = sargsPtr
  sequence [asdf (constInt i, a) | (i, a) <- zip [0..argc] args]
  call (global (funcType ptrType [T.i32, T.i32, ptrType]) (AST.Name "boxClosure"))
    [constInt idx, constInt argc, sargsPtr]

boolToInt True = 1
boolToInt False = 0

codegen :: S.LascaOpts -> AST.Module -> [S.Expr] -> IO AST.Module
codegen opts modo fns = do
  let ast = codegenModule modo fns
  runJIT opts ast
  return ast

codegenModule :: AST.Module -> [S.Expr] -> AST.Module
codegenModule modo fns = modul
    where
        ctx = createGlobalContext fns
        modul = runLLVM modo genModule
        genModule = do
          st <- gets _llvmModule
          fns' <- rewrite ctx fns
          syn <- gets _syntacticAst
          let fns'' = fns' ++ syn
          st' <- gets _llvmModule
--           Debug.traceM ("Rewritten exprs: " ++ show fns'')
--           Debug.traceM ("Rewritten exprs: " ++ show st')
--           Debug.traceM ("Rewritten exprs: " ++ show st')
          genFunctionMap fns''
          mapM (codegenTop ctx) fns''
          codegenStartFunc

rewrite ctx fns = transform extractLambda fns

extractLambda :: S.Expr -> LLVM S.Expr
extractLambda (S.Lam name expr) = do
  state <- get
  let nms = _modNames state
  let syntactic = _syntacticAst state
  let outerVars = _outers state
  let usedOuterVars = Set.toList (Set.intersection outerVars (_usedVars state))
  let enclosedArgs = map (\n -> S.Arg n typeAny) usedOuterVars
  let (funcName, nms') = uniqueName "lambda" nms
  let func = (S.Function (funcName) typeAny (enclosedArgs ++ [(S.Arg name typeAny)]) expr)
  modify (\s -> s { _modNames = nms', _syntacticAst = syntactic ++ [func] })
--   Debug.traceM ("Generated lambda " ++ show func ++ ", outerVars = " ++ show outerVars ++ ", usedOuterVars" ++ show usedOuterVars)
  return (S.BoxFunc funcName enclosedArgs)
extractLambda expr@(S.Var n) = do
  modify (\s -> s { _usedVars = Set.insert n (_usedVars s)})
  return expr
extractLambda expr = do
  return expr

functionStructType = T.StructureType False [ptrType, ptrType, T.i32]
functionsStructType len = T.StructureType False [T.i32, arrayTpe len]

arrayTpe len = T.ArrayType len functionStructType

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

    array = C.Array functionStructType (fmap (\(_, s) -> s) entries)

    mapping :: Map.Map String Int
    mapping = go 0 Map.empty entries where
      go i m ((name, _) : es) = go (i + 1) (Map.insert name i m) es
      go i m [] = m



    entries = fmap (\(name, arity) -> (name, struct name arity)) funcsWithArities

    struct1 = C.Struct (Nothing) False [C.Int 32 (toInteger len), array]

    struct name arity = C.Struct (Nothing) False
                            [C.BitCast (ref name) ptrType, constRef name, C.Int 32 (toInteger arity)]

    ref name = C.GlobalReference (stringStructType (length name)) (getStringLitASTName name)

    funcsWithArities = (foldl go [] fns) where
      go s (S.Function name tpe args body) = (name, length args) : s
      go s (S.Extern name tpe args) = (name, length args) : s
      go s _ = s

createGlobalContext :: [S.Expr] -> Set.Set String
createGlobalContext exprs = context
  where
      context = loop exprs Set.empty
      loop [] m = m
      loop (e:exprs) m = names e m `Set.union` loop exprs m
      names (S.Function name _ _ _) m = Set.insert name m
      names (S.Extern name _ _) m = Set.insert name m
