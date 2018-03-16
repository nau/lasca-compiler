module Lasca.Emit (codegenTop) where

import Text.Printf
import Data.String

import Control.Monad.State
import Control.Lens.Operators

import Lasca.Codegen
import Lasca.Type
import Lasca.EmitCommon
import qualified Lasca.EmitStatic as EmitStatic
import Lasca.Syntax

genExternalFuncWrapper f@(Function meta name returnType externArgs (Literal _ (StringLit externName))) = do
    modState <- get
    let codeGenResult = codeGen modState
        blocks = createBlocks codeGenResult
        retType = typeMapping returnType
    define retType (nameToSBS name) (toSig externArgs) blocks
  where
    codeGen modState = execCodegen [] modState $ do
        entry <- addBlock entryBlockName
        setBlock entry
        let argTypes = map (\(Arg n t) -> t) externArgs
        largs <- forM externArgs $ \(Arg n tpe) -> do
            let argName = nameToSBS n
            EmitStatic.resolveBoxing EmitStatic.anyTypeVar tpe (localPtr argName)
        res <- callFn (externFuncLLvmType f) externName largs
        wrapped <- EmitStatic.resolveBoxing returnType EmitStatic.anyTypeVar res
        ret wrapped
genExternalFuncWrapper other = error $ "genExternalFuncWrapper got " ++ (show other)

codegenTop ctx cgen topExpr = case topExpr of
    this@(Let meta name expr _) -> do
        modify (\s -> s { _globalValsInit = _globalValsInit s ++ [(name, expr)] })
        let valType = llvmTypeOf this
    --    Debug.traceM $ printf "Cons %s: %s" (show name) (show valType)
        defineGlobal (nameToSBS name) valType (Just $ defaultValueForType valType)

    f@(Function meta name tpe args body) ->
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
        funcType = typeOf f
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