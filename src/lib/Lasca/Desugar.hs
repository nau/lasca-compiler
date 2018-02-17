{-# LANGUAGE TemplateHaskell #-}

module Lasca.Desugar where

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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Debug.Trace as Debug

import Lasca.Codegen
import Lasca.Type as Type
import Lasca.Syntax as S
import qualified Lasca.Options as Opts

data DesugarPhaseState = DesugarPhaseState {
    _modNames :: Names,
    _currentFunctionName :: Name,
    _locals :: Map Name Type.Type,
    _outers :: Map Name Type.Type,
    _usedVars :: Set Name,
    _syntacticAst :: [Expr],
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
  go (Lam _ name e) result = let (args, body) = go e result in (name : args, body)
  go e (args, _) = (args, e)

--transform :: (Expr -> LLVM Expr) -> [Expr] -> LLVM [Expr]
transform transformer exprs = sequence [transformExpr transformer expr | expr <- exprs]

--transformExpr :: (Expr -> LLVM Expr) -> Expr -> LLVM Expr
transformExpr transformer expr = case expr of
    expr@(Ident _ n) -> do
        usedVars %= Set.insert n
        return expr
    Array meta exprs -> do
        exprs' <- mapM go exprs
        return $ Array meta exprs'
    Select meta tree expr -> do
        tree' <- go tree
        expr' <- go expr
        return $ Select meta tree' expr'
    (If meta cond true false) -> do
        cond' <- go cond
        true' <- go true
        false' <- go false
        transformer (If meta cond' true' false')
    Match meta expr cases -> do
        expr1 <- go expr
        cases1 <- forM cases $ \(Case p expr) -> do
            e <- go expr
            return $ Case p e
        transformer (Match meta expr1 cases1)
    (Let meta n e body) -> do
        case typeOf e of
          TypeFunc a b -> locals %= Map.insert n a
          _ -> locals %= Map.insert n typeAny
        e' <- go e
        body' <- go body
        transformer (Let meta n e' body')
    l@(Lam m a@(Arg n t) e) -> do
        oldOuters <- gets _outers
        oldUsedVars <- gets _usedVars
        modify (\s -> s { _outers = Map.union (_outers s) (_locals s) } )
        let r = case typeOf l of
                    TypeFunc a b -> Map.singleton n a
                    _ -> Map.singleton n typeAny
        locals .= r
        e' <- go e
        res <- transformer (Lam m a e')
        modify (\s  -> s {_outers = oldOuters, _locals = r})
        return res
    (Apply meta e args) -> do
        e' <- go e
        args' <- mapM go args
        transformer (Apply meta e' args')
    f@(Function meta name tpe args e1) -> do
        let funcTypeToLlvm (Arg name _) (TypeFunc a b, acc) = (b, (name, a) : acc)
            funcTypeToLlvm arg t = error $ "AAA2" ++ show arg ++ show t
        let funcType = typeOf f
        let argsWithTypes = reverse $ snd $ foldr funcTypeToLlvm (funcType, []) (reverse args)
        let argNames = Map.fromList argsWithTypes
        modify (\s -> s {
            _currentFunctionName = name,
            _locals = argNames,
            _outers = Map.empty,
            _usedVars = Set.empty } )
        e' <- go e1
        transformer (Function meta name tpe args e')
    e -> transformer e

  where go e = transformExpr transformer e

extractLambda meta args expr = do
    state <- get
    curFuncName <- gets _currentFunctionName
    let nms = _modNames state
    let syntactic = _syntacticAst state
    -- lambda args are in locals. Shadow outers with same names.
    let outerVars = Set.difference (Map.keysSet $ _outers state) (Map.keysSet $ _locals state)
    let usedOuterVars = Set.toList (Set.intersection outerVars (_usedVars state))
    let enclosedArgs = map (\n -> (Arg n typeAny, _outers state Map.! n)) usedOuterVars
    let (funcName', nms') = uniqueName (fromString $ (show curFuncName) ++ "_lambda") nms
    let funcName = Name $ Char8.unpack funcName'
    let enclosedArgType = TypeApply (TypeIdent "Array") [typeAny]
    let addEnclosedArgsParameter t = TypeFunc enclosedArgType t -- HACK with Array type
    let meta' = (exprType %~ addEnclosedArgsParameter) meta
    let generateEnclosedLocals ((Arg name tpe, _), idx) e = do
            let m = meta
            let body = Apply m (Ident meta (NS "Prelude" "arrayApply")) [
                  Ident (meta `withType` enclosedArgType) "$enclosed",
                  Literal (meta `withType` TypeInt) $ IntLit idx]
            Let m name body e
    let expr1 = foldr generateEnclosedLocals expr (zip enclosedArgs [0..])
    let func = if null enclosedArgs then Function meta funcName typeAny args expr
               else Function meta' funcName typeAny (Arg "$enclosed" typeAny : args) expr1
    modify (\s -> s { _modNames = nms', _syntacticAst = syntactic ++ [func] })
    s <- get
--    Debug.traceM $ printf "Generated lambda %s, outerVars = %s, usedOuterVars = %s, state = %s" funcName (show outerVars) (show usedOuterVars) (show s)
    return (BoxFunc meta' funcName (map fst enclosedArgs))


delambdafy ctx exprs = let
        (desugared, st) = runState (mapM delambdafyExpr exprs) emptyDesugarPhaseState
        syn = _syntacticAst st
    in syn ++ desugared

    where
      delambdafyExpr expr = case expr of
          expr@(Ident _ n) -> do
              usedVars %= Set.insert n
              return expr
          Array meta exprs -> do
              exprs' <- mapM go exprs
              return $ Array meta exprs'
          Select meta tree expr -> do
              tree' <- go tree
              expr' <- go expr
              return $ Select meta tree' expr'
          (If meta cond true false) -> do
              cond' <- go cond
              true' <- go true
              false' <- go false
              return (If meta cond' true' false')
          Match meta expr cases -> do
              expr1 <- go expr
              cases1 <- forM cases $ \(Case p expr) -> do
                  e <- go expr
                  return $ Case p e
              return (Match meta expr1 cases1)
          (Let meta n e body) -> do
              case typeOf e of
                TypeFunc a b -> locals %= Map.insert n a
                _ -> locals %= Map.insert n typeAny
              e' <- go e
              body' <- go body
              return (Let meta n e' body')
          l@(Lam m a@(Arg n t) e) -> do
              oldOuters <- gets _outers
              oldUsedVars <- gets _usedVars
              oldLocals <- gets _locals
              modify (\s -> s { _outers = Map.union (_outers s) (_locals s) } )
              let (args, e) = uncurryLambda l
              let r = foldr (\(Arg n t) -> Map.insert n typeAny) Map.empty args
              locals .= r
              e' <- go e
              res <- extractLambda m args e'
              modify (\s  -> s {_outers = oldOuters, _locals = oldLocals})
              return res
          (Apply meta e args) -> do
              e' <- go e
              args' <- mapM go args
      --        args' <- sequence [go arg | arg <- args]
              return (Apply meta e' args')
          f@(Function meta name tpe args e1) -> do
              let funcTypeToLlvm (Arg name _) (TypeFunc a b, acc) = (b, (name, a) : acc)
                  funcTypeToLlvm arg t = error $ "AAA2" ++ show arg ++ show t
              let funcType = typeOf f
              let argsWithTypes = reverse $ snd $ foldr funcTypeToLlvm (funcType, []) (reverse args)
              let argNames = Map.fromList argsWithTypes
              modify (\s -> s {
                  _currentFunctionName = name,
                  _locals = argNames,
                  _outers = Map.empty,
                  _usedVars = Set.empty } )
              e' <- go e1
              return (Function meta name tpe args e')
          e -> return e
          where go e = delambdafyExpr e

desugarAssignment expr = case expr of
    Apply meta (Ident imeta ":=") [ref, value] -> Apply meta (Ident imeta (NS "Prelude" "updateRef")) [ref, value]
    _ -> expr

desugarUnaryMinus expr = case expr of
    Apply _ (Ident _ ("unary-")) [Literal meta (IntLit v)] -> Literal meta (IntLit (-v))
    Apply _ (Ident _ ("unary-")) [Literal meta (FloatLit v)] -> Literal meta (FloatLit (-v))
    e -> e

desugarAndOr expr = case expr of
    Apply meta (Ident _ "or")  [lhs, rhs] -> If meta lhs (Literal emptyMeta (BoolLit True)) rhs
    Apply meta (Ident _ "and") [lhs, rhs] -> If meta lhs rhs (Literal emptyMeta (BoolLit False))
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

--genMatch :: Ctx -> Expr -> Expr
genMatch ctx m@(Match meta expr []) = error $ "Should be at least on case in match expression: " ++ show m
genMatch ctx (Match meta expr cases) = do
    matchName <- freshName "$match"
    let body = foldr (\(Case p e) acc -> genPattern ctx (Ident (expr ^. metaLens) matchName) p e acc
                      ) genFail cases
    return $ Let meta matchName expr body
genMatch ctx expr = return expr

genFail = Apply emptyMeta (Ident emptyMeta (NS "Prelude" "die")) [Literal emptyMeta $ StringLit "Match error!"]

genPattern ctx lhs WildcardPattern rhs = const rhs
genPattern ctx lhs (VarPattern name) rhs = const (Let emptyMeta name lhs rhs)
genPattern ctx lhs (LitPattern literal) rhs =
    If emptyMeta (Apply emptyMeta (Ident emptyMeta "==") [lhs, Literal emptyMeta literal]) rhs
genPattern ctx lhs (ConstrPattern name args) rhs = cond
  where
    cond fail = If emptyMeta constrCheck (checkArgs name fail) fail
    constrCheck = Apply emptyMeta (Ident emptyMeta (NS "Prelude" "runtimeIsConstr"))
                      [lhs, Literal emptyMeta $ StringLit (show name)]
    constrMap = ctx ^. constructorArgs
    checkArgs nm fail =  case Map.lookup nm constrMap of
        Nothing -> fail
        Just constrArgs | length args == length constrArgs -> do
            let argParam = zip args constrArgs
            foldr (\(a, Arg n _) acc ->
                    genPattern ctx (Select emptyMeta lhs (Ident emptyMeta n)) a acc fail
                ) rhs argParam
        Just constrArgs -> error $ printf "Constructor %s has %d parameters, but %d given"
            (show nm) (length constrArgs) (length args) -- TODO box this error
