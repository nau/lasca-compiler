{-# LANGUAGE Strict #-}

module EmitDynamic (
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


-- codegenTop :: S.Expr -> LLVM ()
codegenTop ctx (S.Val _ name expr) = do
    modify (\s -> s { _globalValsInit = _globalValsInit s ++ [(name, expr)] })
    defineGlobal (fromString name) ptrType (Just (C.Null ptrType))

codegenTop ctx (S.Function meta name tpe args body) =
    if meta ^. S.isExternal then
        external (externalTypeMapping tpe) (fromString name ) (externArgsToSig args) False []
    else do
        modState <- get
        let codeGenResult = codeGen modState
        let blocks = createBlocks codeGenResult
        mapM_ defineStringLit (generatedStrings codeGenResult)
        define ptrType (fromString name) largs blocks
  where
    largs = toSig args
    codeGen modState = execCodegen [] modState $ do
        entry <- addBlock entryBlockName
        setBlock entry
  --       Debug.traceM ("Generating function2 " ++ name)
        forM_ args $ \(S.Arg n t) -> do
            var <- alloca ptrType
            store var (localPtr $ fromString n)
            assign n var
        cgen ctx body >>= ret

codegenTop ctx (S.Data _ name tvars constructors) = return ()

codegenTop ctx expr =
    error $ printf "Expression of this kind should not get to codegenTop. It's a bug. %s at %s"
        (show expr) (show $ S.exprPosition expr)

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

cgen :: Ctx -> S.Expr -> Codegen AST.Operand
cgen ctx (S.Let meta a b c) = do
    i <- alloca ptrType
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
cgen ctx (S.Literal l meta) = do
--  Debug.traceM $ "Generating literal " ++ show l ++ " on " ++ show (S.pos meta)
    box meta l
cgen ctx (S.Array _ exprs) = do
    vs <- values
    boxArray vs
  where values = sequence [cgen ctx e | e <- exprs]
cgen ctx (S.Select meta tree expr) = do
    tree <- cgen ctx tree
    e <- cgen ctx expr
    let pos = createPosition $ S.pos meta
    callFn "runtimeSelect" [tree, e, constOp pos]
cgen ctx (S.Apply meta (S.Ident _ "or") [lhs, rhs]) = cgen ctx (S.If meta lhs (S.Literal S.emptyMeta (S.BoolLit True)) rhs)
cgen ctx (S.Apply meta (S.Ident _ "and") [lhs, rhs]) = cgen ctx (S.If meta lhs rhs (S.Literal S.emptyMeta (S.BoolLit False)))
cgen ctx (S.Apply meta (S.Ident _ fn) [lhs, rhs]) | fn `Map.member` binops = do
    llhs <- cgen ctx lhs
    lrhs <- cgen ctx rhs
    let code = fromMaybe (error ("Couldn't find binop " ++ fn)) (Map.lookup fn binops)
    let codeOp = constIntOp code
    callFn "runtimeBinOp" [codeOp, llhs, lrhs]
cgen ctx (S.Apply meta expr args) = do
    syms <- gets symtab
    let symMap = Map.fromList syms
    let isGlobal fn = (fn `Map.member` S._globalFunctions ctx) && not (fn `Map.member` symMap)
    let funDecl fn = (ctx ^. S.globalFunctions) Map.! fn
    let isExtern fn = isGlobal fn && (funDecl fn ^. S.metaLens.S.isExternal)
    case expr of
       -- TODO Here are BUGZZZZ!!!! :)
       -- TODO check arguments!
       -- this is done to speed-up calls if you `a global function
        S.Ident _ fn | isExtern fn -> do
            let (S.Function _ _ returnType externArgs _) = S._globalFunctions ctx Map.! fn
            let argTypes = map (\(S.Arg n t) -> t) externArgs
--            Debug.traceM $ printf "Calling external %s(%s): %s" fn (show argTypes) (show returnType)
            largs <- forM (zip args argTypes) $ \(arg, tpe) -> do
                a <- cgen ctx arg
                case tpe of
                    TypeIdent "Int" -> callFn "unboxInt" [a]
                    TypeIdent "Float" -> callFn "unboxFloat64" [a]
                    _ -> return a
            case returnType of
                TypeIdent "Int" -> do
                    res <- callFn (fromString fn) largs
--                    Debug.traceM ("res = " ++ show res)
                    callFn "boxInt"  [res]
                TypeIdent "Float" -> do
                    res <- callFn (fromString fn) largs
--                    Debug.traceM ("res = " ++ show res ++ show largs ++ fn)
                    callFn "boxFloat64"  [res]
                _ -> callFn (fromString fn) largs

        S.Ident _ fn | isGlobal fn -> do
--            Debug.traceM $ printf "Calling %s" fn
            largs <- forM args $ \arg -> cgen ctx arg
            callFn (fromString fn) largs

        expr -> do
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
cgen ctx (S.BoxFunc _ funcName enclosedVars) = do
    modState <- gets moduleState
    let mapping = functions modState
    if null enclosedVars then boxFunc funcName mapping
    else boxClosure funcName mapping enclosedVars
cgen ctx m@S.Match{} =
    error $ printf "Match expressions should be already desugared! %s at: %s" (show m) (show $ S.exprPosition m)
cgen ctx (S.If meta cond tr fl) = do
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
    phi ptrType [(trval, ifthen), (flval, ifelse)]

cgen ctx e = error ("cgen shit " ++ show e)

-- Compilation
-------------------------------------------------------------------------------

codegenModule :: S.LascaOpts -> AST.Module -> [S.Expr] -> AST.Module
codegenModule opts modo exprs = modul
  where
    ctx = createGlobalContext opts exprs
    desugared' = desugarExprs ctx desugarExpr exprs
    desugared = delambdafy ctx desugared'
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
    types <- genData ctx defs toSig argToPtr
    let array = C.Array ptrType types
    defineConst "Types" structType (struct array)
  where len = length defs
        struct a = createStruct [constInt len, a]
        structType = T.StructureType False [T.i32, T.ArrayType (fromIntegral len) ptrType]

argToPtr (S.Arg n t) = return $ localPtr $ fromString n