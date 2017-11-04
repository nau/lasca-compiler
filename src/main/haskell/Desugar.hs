{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}

module Desugar where

-- import qualified Data.Text as Text
import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as Encoding
import Text.Printf
import qualified Data.ByteString.UTF8 as UTF8
import Data.String
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
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
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Debug.Trace as Debug

import Codegen
import Type
import qualified Syntax as S
import Syntax (Ctx, createGlobalContext)
import qualified Options as Opts

data DesugarPhaseState = DesugarPhaseState {
    _modNames :: Names,
    _currentFunctionName :: Name,
    _locals :: Map.Map Name Type.Type,
    _outers :: Map.Map Name Type.Type,
    _usedVars :: Set.Set Name,
    _syntacticAst :: [S.Expr],
    _freshId :: Int
} deriving (Show)
makeLenses ''DesugarPhaseState

emptyDesugarPhaseState = DesugarPhaseState {
    _modNames = Map.empty,
    _currentFunctionName = "",
    _locals = Map.empty,
    _outers = Map.empty,
    _usedVars = Set.empty,
    _syntacticAst = [],
    _freshId = 1
}

freshName n = do
    idx <- gets _freshId
    freshId += 1
    return $ fromString (n ++ show idx)

uncurryLambda expr = go expr ([], expr) where
  go (S.Lam _ name e) result = let (args, body) = go e result in (name : args, body)
  go e (args, _) = (args, e)

--transform :: (S.Expr -> LLVM S.Expr) -> [S.Expr] -> LLVM [S.Expr]
transform transformer exprs = sequence [transformExpr transformer expr | expr <- exprs]

--transformExpr :: (S.Expr -> LLVM S.Expr) -> S.Expr -> LLVM S.Expr
transformExpr transformer expr = case expr of
    expr@(S.Ident _ n) -> do
        modify (\s -> s { _usedVars = Set.insert n (_usedVars s)})
        return expr
    S.Array meta exprs -> do
        exprs' <- mapM go exprs
        return $ S.Array meta exprs'
    S.Select meta tree expr -> do
        tree' <- go tree
        expr' <- go expr
        return $ S.Select meta tree' expr'
    (S.If meta cond true false) -> do
        cond' <- go cond
        true' <- go true
        false' <- go false
        transformer (S.If meta cond' true' false')
    S.Match meta expr cases -> do
        cases1 <- forM cases $ \(S.Case p expr) -> do
            e <- go expr
            return $ S.Case p e
        transformer (S.Match meta expr cases1)
    (S.Let meta n e body) -> do
        case S.typeOf e of
          TypeFunc a b -> locals %= Map.insert n a
          _ -> locals %= Map.insert n typeAny
        e' <- go e
        body' <- go body
        transformer (S.Let meta n e' body')
    l@(S.Lam m a@(S.Arg n t) e) -> do
        oldOuters <- gets _outers
        oldUsedVars <- gets _usedVars
        modify (\s -> s { _outers = Map.union (_outers s) (_locals s) } )
        let r = case S.typeOf l of
                    TypeFunc a b -> Map.singleton n a
                    _ -> Map.singleton n typeAny
        locals .= r
        e' <- go e
        res <- transformer (S.Lam m a e')
        modify (\s  -> s {_outers = oldOuters, _locals = r})
        return res
    (S.Apply meta e args) -> do
        e' <- go e
        args' <- mapM go args
--        args' <- sequence [go arg | arg <- args]
        transformer (S.Apply meta e' args')
    f@(S.Function meta name tpe args e1) -> do
        let funcTypeToLlvm (S.Arg name _) (TypeFunc a b, acc) = (b, (name, a) : acc)
            funcTypeToLlvm arg t = error $ "AAA2" ++ show arg ++ show t
        let funcType = S.typeOf f
        let argsWithTypes = reverse $ snd $ foldr funcTypeToLlvm (funcType, []) (reverse args)
        let argNames = Map.fromList argsWithTypes
        modify (\s -> s { _currentFunctionName = name, _locals = argNames, _outers = Map.empty, _usedVars = Set.empty } )
        e' <- go e1
        transformer (S.Function meta name tpe args e')
    e -> transformer e

  where go e = transformExpr transformer e

extractLambda2 meta args expr = do
    state <- get
    curFuncName <- gets _currentFunctionName
    let nms = _modNames state
    let syntactic = _syntacticAst state
    let outerVars = Map.keysSet $ _outers state
    let usedOuterVars = Set.toList (Set.intersection outerVars (_usedVars state))
    let enclosedArgs = map (\n -> (S.Arg n typeAny, _outers state Map.! n)) usedOuterVars
    let (funcName', nms') = uniqueName (fromString $ (show curFuncName) ++ "_lambda") nms
    let funcName = Name $ Char8.unpack funcName'
    let enclosedArgType = TypeApply (TypeIdent "Array") [typeAny]
    let addEnclosedArgsParameter t = TypeFunc enclosedArgType t -- HACK with Array type
    let meta' = (S.exprType %~ addEnclosedArgsParameter) meta
    let generateEnclosedLocals ((S.Arg name tpe, _), idx) e = do
            let m = meta
            let body = S.Apply m (S.Ident meta "arrayApply") [
                  S.Ident (meta `S.withType` enclosedArgType) "$enclosed",
                  S.Literal (meta `S.withType` TypeInt) $ S.IntLit idx]
            S.Let m name body e
    let expr1 = foldr generateEnclosedLocals expr (zip enclosedArgs [0..])
    let func = S.Function meta' funcName typeAny (S.Arg "$enclosed" typeAny : args) expr1
    modify (\s -> s { _modNames = nms', _syntacticAst = syntactic ++ [func] })
    s <- get
--    Debug.traceM $ printf "Generated lambda %s, outerVars = %s, usedOuterVars = %s, state = %s" funcName (show outerVars) (show usedOuterVars) (show s)
    return (S.BoxFunc meta' funcName (map fst enclosedArgs))


delambdafy ctx exprs = let
        (desugared, st) = runState (mapM delambdafyExpr exprs) emptyDesugarPhaseState
        syn = _syntacticAst st
    in syn ++ desugared

    where delambdafyExpr expr = case expr of
            expr@(S.Ident _ n) -> do
                modify (\s -> s { _usedVars = Set.insert n (_usedVars s)})
                return expr
            S.Array meta exprs -> do
                exprs' <- mapM go exprs
                return $ S.Array meta exprs'
            S.Select meta tree expr -> do
                tree' <- go tree
                expr' <- go expr
                return $ S.Select meta tree' expr'
            (S.If meta cond true false) -> do
                cond' <- go cond
                true' <- go true
                false' <- go false
                return (S.If meta cond' true' false')
            S.Match meta expr cases -> do
                cases1 <- forM cases $ \(S.Case p expr) -> do
                    e <- go expr
                    return $ S.Case p e
                return (S.Match meta expr cases1)
            (S.Let meta n e body) -> do
                case S.typeOf e of
                  TypeFunc a b -> locals %= Map.insert n a
                  _ -> locals %= Map.insert n typeAny
                e' <- go e
                body' <- go body
                return (S.Let meta n e' body')
            l@(S.Lam m a@(S.Arg n t) e) -> do
                oldOuters <- gets _outers
                oldUsedVars <- gets _usedVars
                oldLocals <- gets _locals
                modify (\s -> s { _outers = Map.union (_outers s) (_locals s) } )
                let (args, e) = uncurryLambda l
                let r = foldr (\(S.Arg n t) -> Map.insert n typeAny) Map.empty args
                locals .= r
                e' <- go e
                res <- extractLambda2 m args e'
                modify (\s  -> s {_outers = oldOuters, _locals = oldLocals})
                return res
            (S.Apply meta e args) -> do
                e' <- go e
                args' <- mapM go args
        --        args' <- sequence [go arg | arg <- args]
                return (S.Apply meta e' args')
            f@(S.Function meta name tpe args e1) -> do
                let funcTypeToLlvm (S.Arg name _) (TypeFunc a b, acc) = (b, (name, a) : acc)
                    funcTypeToLlvm arg t = error $ "AAA2" ++ show arg ++ show t
                let funcType = S.typeOf f
                let argsWithTypes = reverse $ snd $ foldr funcTypeToLlvm (funcType, []) (reverse args)
                let argNames = Map.fromList argsWithTypes
                modify (\s -> s { _currentFunctionName = name, _locals = argNames, _outers = Map.empty, _usedVars = Set.empty } )
                e' <- go e1
                return (S.Function meta name tpe args e')
            e -> return e
            where go e = delambdafyExpr e

desugarAssignment expr = case expr of
    S.Apply meta (S.Ident imeta ":=") [ref, value] -> S.Apply meta (S.Ident imeta "updateRef") [ref, value]
    _ -> expr

desugarUnaryMinus expr = case expr of
    S.Apply _ (S.Ident _ ("unary-")) [S.Literal meta (S.IntLit v)] -> S.Literal meta (S.IntLit (-v))
    S.Apply _ (S.Ident _ ("unary-")) [S.Literal meta (S.FloatLit v)] -> S.Literal meta (S.FloatLit (-v))
    e -> e

desugarAndOr expr = case expr of
    S.Apply meta (S.Ident _ "or")  [lhs, rhs] -> S.If meta lhs (S.Literal S.emptyMeta (S.BoolLit True)) rhs
    S.Apply meta (S.Ident _ "and") [lhs, rhs] -> S.If meta lhs rhs (S.Literal S.emptyMeta (S.BoolLit False))
    e -> e

desugarExpr ctx expr = do
    let desugarFunc = desugarAndOr . desugarAssignment . desugarUnaryMinus
    let expr1 = desugarFunc expr
    expr2 <- genMatch ctx expr1
    return expr2

desugarExprs ctx func exprs = let
    (desugared, st) = runState (transform (func ctx) exprs) emptyDesugarPhaseState
    syn = _syntacticAst st
  in syn ++ desugared

--genMatch :: Ctx -> S.Expr -> S.Expr
genMatch ctx m@(S.Match meta expr []) = error $ "Should be at least on case in match expression: " ++ show m
genMatch ctx (S.Match meta expr cases) = do
    matchName <- freshName "$match"
    let body = foldr (\(S.Case p e) acc -> genPattern ctx (S.Ident (expr ^. S.metaLens) matchName) p e acc
                      ) genFail cases
    return $ S.Let meta matchName expr body
genMatch ctx expr = return expr

genFail = S.Apply S.emptyMeta (S.Ident S.emptyMeta "die") [S.Literal S.emptyMeta $ S.StringLit "Match error!"]

genPattern ctx lhs S.WildcardPattern rhs = const rhs
genPattern ctx lhs (S.VarPattern name) rhs = const (S.Let S.emptyMeta (Name name) lhs rhs)
genPattern ctx lhs (S.LitPattern literal) rhs = S.If S.emptyMeta (S.Apply S.emptyMeta (S.Ident S.emptyMeta "==") [lhs, S.Literal S.emptyMeta literal]) rhs
genPattern ctx lhs (S.ConstrPattern name args) rhs = cond
  where cond fail = S.If S.emptyMeta constrCheck (checkArgs name fail) fail
        constrCheck = S.Apply S.emptyMeta (S.Ident S.emptyMeta "runtimeIsConstr") [lhs, S.Literal S.emptyMeta $ S.StringLit (show name)]
        constrMap = let cs = foldr (\ (S.DataDef _ _ constrs) acc -> constrs ++ acc) [] (S.dataDefs ctx)
                        tuples = fmap (\c@(S.DataConst n args) -> (n, args)) cs
                    in  Map.fromList tuples
        checkArgs nm fail =  case Map.lookup nm constrMap of
                            Nothing -> fail
                            Just constrArgs | length args == length constrArgs -> do
                                let argParam = zip args constrArgs
                                foldr (\(a, S.Arg n _) acc -> genPattern ctx (S.Select S.emptyMeta lhs (S.Ident S.emptyMeta n)) a acc fail) rhs argParam
                            Just constrArgs -> error (printf "Constructor %s has %d parameters, but %d given" (show nm) (length constrArgs) (length args)) -- TODO box this error
