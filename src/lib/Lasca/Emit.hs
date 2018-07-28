module Lasca.Emit (codegenTop, collectGlobals) where

import Text.Printf
import Data.String
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Debug.Trace as Debug

import Control.Monad.State
import Control.Lens.Operators

import Lasca.Codegen
import Lasca.Type
import Lasca.EmitCommon
import qualified Lasca.EmitStatic as EmitStatic
import Lasca.Syntax

genExternalFuncWrapper f@(Let True meta name returnType lam _) = do
    modState <- get
    let codeGenResult = codeGen modState
        blocks = createBlocks codeGenResult
        retType = typeMapping returnType

    define retType (nameToSBS name) (toSig externArgs) blocks
  where
    (externArgs, Literal _ (StringLit externName)) = uncurryLambda lam
    codeGen modState = execCodegen [] modState $ do
        entry <- addBlock entryBlockName
        setBlock entry
        let argTypes = map (\(Arg n t) -> t) externArgs
        largs <- forM externArgs $ \(Arg n tpe) -> do
            let argName = nameToSBS n
            EmitStatic.resolveBoxing EmitStatic.anyTypeVar tpe (localPtr argName)
        let retType = externalTypeMapping returnType
--        Debug.traceM $ printf "%s genExternalFuncWrapper %s, retType %s" (show name) (show $ externFuncLLvmType f) (show retType)
        res <- instrTyped retType $ callFnIns (externFuncLLvmType f) externName largs
        wrapped <- EmitStatic.resolveBoxing returnType EmitStatic.anyTypeVar res
        ret wrapped
genExternalFuncWrapper other = error $ "genExternalFuncWrapper got " ++ (show other)


collectGlobals ctx exprs = do
    execState (mapM toplevel exprs) ctx
  where
    toplevel expr = case expr of
        Let False meta name _ expr EmptyExpr -> globalVals %= Map.insert name expr
        Let True meta name _ lam EmptyExpr -> globalFunctions %= Map.insert name expr
        _ -> return ()

codegenTop ctx cgen topExpr = case topExpr of
    this@(Let False meta name _ expr _) -> do
        modify (\s -> s { _globalValsInit = _globalValsInit s ++ [(name, expr)] })
        let valType = llvmTypeOf this
    --    Debug.traceM $ printf "Cons %s: %s" (show name) (show valType)
        defineGlobal (nameToSBS name) valType (Just $ defaultValueForType valType)

    f@(Let True meta name tpe lam _) -> do
        if meta ^. isExternal then do
            let (Literal _ (StringLit externName)) = body
            external (externalTypeMapping tpe) (fromString externName) (externArgsToSig args) False []
            genExternalFuncWrapper f
        else do
            modState <- get
            let codeGenResult = codeGen modState
            let blocks = createBlocks codeGenResult
            mapM_ defineStringLit (generatedStrings codeGenResult)
            let retType = mappedReturnType args funcType
            define retType (nameToSBS name) largs blocks
      where
        (args, body) = uncurryLambda lam
        
        funcType = typeOf lam
        largs = map (\(n, t) -> (nameToSBS n, t)) argsWithTypes

        funcTypeToLlvm (Arg name _) (TypeFunc a b, acc) = (b, (name, typeMapping a) : acc)
        funcTypeToLlvm arg t = error $ "AAA3" ++ show arg ++ show t

        argsWithTypes = do
    --        Debug.traceM $ printf "codegenTop %s(%s): %s" (show name) (show args) (show funcType)
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

    (Data _ name tvars constructors) -> return ()
    Module{} -> return ()
    Import{} -> return ()
    _ -> error $ printf "Expression of this kind should not get to codegenTop. It's a bug. %s at %s"
            (show topExpr) (show $ exprPosition topExpr)