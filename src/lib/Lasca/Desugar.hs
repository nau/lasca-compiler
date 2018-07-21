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
import Lasca.Infer
import qualified Lasca.Options as Opts

data DesugarPhaseState = DesugarPhaseState {
    _modNames :: Names,
    _functionStack :: [Name],
    _locals :: Map Name Type.Type,
    _outers :: Map Name Type.Type,
    _usedVars :: Set Name,
    _syntacticAst :: [Expr],
    _renames :: Map Name Name,
    _freshId :: Int
} deriving (Show)
makeLenses ''DesugarPhaseState

emptyDesugarPhaseState = DesugarPhaseState {
    _modNames = Map.empty,
    _functionStack = [],
    _locals = Map.empty,
    _outers = Map.empty,
    _usedVars = Set.empty,
    _syntacticAst = [],
    _renames = Map.empty,
    _freshId = 1
}

freshName n = do
    idx <- gets _freshId
    freshId += 1
    return $ fromString (n ++ show idx)

uncurryLambda expr = go expr ([], expr) where
  go (Lam _ name e) result = let (args, body) = go e result in (name : args, body)
  go e (args, _) = (args, e)

curryLambda meta args expr = foldr (Lam meta) expr args


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
    (Let False meta n tpe e body) -> do
        case typeOf e of
          TypeFunc a b -> locals %= Map.insert n a
          _ -> locals %= Map.insert n TypeAny
        e' <- go e
        body' <- go body
        transformer (Let False meta n tpe e' body')
    l@(Lam m a@(Arg n t) e) -> do
        oldOuters <- gets _outers
        oldUsedVars <- gets _usedVars
        modify (\s -> s { _outers = Map.union (_outers s) (_locals s) } )
        let r = case typeOf l of
                    TypeFunc a b -> Map.singleton n a
                    _ -> Map.singleton n TypeAny
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
            _functionStack = name : (_functionStack s),
            _locals = argNames,
            _outers = Map.empty,
            _usedVars = Set.empty } )
        e' <- go e1
        functionStack %= tail
        transformer (Function meta name tpe args e')
    e -> transformer e

  where go e = transformExpr transformer e

extractFunction meta name args expr = do
    state <- get
    let nms = _modNames state
    let syntactic = _syntacticAst state
    -- lambda args are in locals. Shadow outers with same names.
    let outerVars = Set.difference (Map.keysSet $ _outers state) (Map.keysSet $ _locals state)
    let usedOuterVars = Set.toList (Set.intersection outerVars (_usedVars state))
    let enclosedArgs = map (\n -> (Arg n TypeAny, _outers state Map.! n)) usedOuterVars
    let (funcName', nms') = uniqueName (fromString name) nms
    let funcName = Name $ Char8.unpack funcName'
    let asdf t = foldr (\(_, t) resultType -> t ==> resultType) t enclosedArgs
    let meta' = (S.exprType %~ asdf) meta
    let func = S.Function meta' funcName TypeAny (map fst enclosedArgs ++ args) expr
    modify (\s -> s { _modNames = nms', _syntacticAst = syntactic ++ [func] })
--    s <- get
--    Debug.traceM $ printf "Generated lambda %s, outerVars = %s, usedOuterVars = %s, state = %s" (show funcName) (show outerVars) (show usedOuterVars) (show s)
    return (Closure meta' funcName (map fst enclosedArgs))

{-
  Only basic lambda lifting. Buggy as hell.
  Lifted function renaming is wrong in some cases of shadowing. FIXME
  Only simple inner functions are lifted. FIXME
  Inner function can be self recursive.
  Inner functions can't be mutually recursive. FIXME
  Only define before use semantics.  FIXME: allow mutual recursion
  Doesn't support passing enclosed arguments to inner recursive functions: FIXME
  def outer() =
      a = 1
      def fact(n) = if n == 1 then a else n * fact(n - 1)
      fact(2)
  Should be rewritten as:
  def outer_fact($a, n) = if n == 1 then $a else n * fact($a, n - 1)
  def outer() =
        a = 1
        outer_fact(a, 2)
-}
lambdaLiftPhase ctx exprs = let
        (desugared, st) = runState (mapM lambdaLiftExpr exprs) emptyDesugarPhaseState
        syn = _syntacticAst st
    in syn ++ desugared

    where
      lambdaLiftExpr expr = case expr of
          Ident meta name -> do
              renames <- gets _renames
              let n = Map.findWithDefault name name renames
              usedVars %= Set.insert n
              return $ Ident meta n
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
          (Let False meta n tpe e body) -> do
              oldRenames <- gets _renames
              case typeOf e of
                TypeFunc a b -> locals %= Map.insert n a
                _ -> locals %= Map.insert n TypeAny
              renames %= Map.delete n
              e' <- go e
              body' <- go body
              renames .= oldRenames
              return (Let False meta n tpe e' body')
          l@(Lam m a@(Arg n t) e) -> do
              oldOuters <- gets _outers
              oldUsedVars <- gets _usedVars
              oldLocals <- gets _locals
              modify (\s -> s { _outers = Map.union (_outers s) (_locals s) } )
              let (args, e) = uncurryLambda l
              let r = foldr (\(Arg n t) -> Map.insert n TypeAny) Map.empty args
              locals .= r
              e' <- go e
              modify (\s  -> s {_outers = oldOuters, _locals = oldLocals})
              return (curryLambda m args e')
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
              outerFuncStack <- gets _functionStack

              res <- case outerFuncStack of
                  [] -> do  modify (\s -> s {
                                _functionStack = [name],
                                _locals = argNames,
                                _outers = Map.empty,
                                _usedVars = Set.empty } )
                            e' <- go e1
                            return (Function meta name tpe args e')
                  _  -> do  oldOuters <- gets _outers
                            oldUsedVars <- gets _usedVars
                            oldLocals <- gets _locals
                            modify (\s -> s {
                                _functionStack = name : (_functionStack s),
                                _outers = Map.union (_outers s) (_locals s),
                                _locals = argNames } )
                            stack <- gets _functionStack
                            let fullName = List.intercalate "_" (map show . reverse $ stack)
                            renames %= Map.insert name (Name fullName)
                            e' <- go e1
                            r <- extractFunction meta fullName args e'
                            modify (\s  -> s {_outers = oldOuters, _locals = oldLocals})
                            return r

              functionStack %= tail
              return res
          e -> return e
          where go e = lambdaLiftExpr e

delambdafyPhase ctx exprs = let
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
          (Let False meta n tpe e body) -> do
              case typeOf e of
                TypeFunc a b -> locals %= Map.insert n a
                _ -> locals %= Map.insert n TypeAny
              e' <- go e
              body' <- go body
              return (Let False meta n tpe e' body')
          l@(Lam m a@(Arg n t) e) -> do
              state <- get
              let stack = state^.functionStack
                  oldOuters = _outers state
                  oldUsedVars = _usedVars state
                  oldLocals = _locals state
              modify (\s -> s { _outers = Map.union (_outers s) (_locals s) } )
              let (args, e) = uncurryLambda l
              let r = foldr (\(Arg n t) -> Map.insert n TypeAny) Map.empty args
              locals .= r
              e' <- go e
              let fullName = List.intercalate "_" (map show . reverse $ Name "lambda" : stack)
              res <- extractFunction m fullName args e'
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
                  _functionStack = name : (_functionStack s),
                  _locals = argNames,
                  _outers = Map.empty,
                  _usedVars = Set.empty } )
              e' <- go e1
              functionStack %= tail
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

desugarPhase ctx exprs = let
    (desugared, st) = runState (transform desugarExpr exprs) emptyDesugarPhaseState
    syn = _syntacticAst st
  in syn ++ desugared

  where
    desugarExpr expr = do
        let desugarFunc = desugarAndOr . desugarAssignment . desugarUnaryMinus
        let expr1 = desugarFunc expr
        return expr1

patmatPhase ctx exprs = let
    (desugared, st) = runState (transform desugarExpr exprs) emptyDesugarPhaseState
    syn = _syntacticAst st
  in syn ++ desugared

  where
    desugarExpr expr = do
        expr2 <- genMatch ctx expr
        return expr2


--genMatch :: Ctx -> Expr -> Expr
genMatch ctx m@(Match meta expr []) = error $ "Should be at least on case in match expression: " ++ show m
genMatch ctx m@(Match meta expr cases) = do
    matchName <- freshName "$match"
    let resultType = meta ^. exprType
    let exprType = typeOf expr
    let body = foldr (\(Case p e) acc -> genPattern ctx exprType resultType (Ident (expr ^. metaLens) matchName) p e acc
                      ) (genFail ctx resultType) cases
    let res = Let False (withType meta resultType) matchName TypeAny expr body
--    Debug.traceM $ printf "getMatch rewrite:\n%s\n============= becomes =========\n%s" (show m) (show res)
    return $ res
genMatch ctx expr = return expr

genFail ctx resultType = do
    let die = Ident (metaType (TypeString ==> resultType)) (NS "Prelude" "die")
    let expr = Apply (metaType resultType) die [Literal (metaType TypeString) $ StringLit "Match error!"]
    expr

genPattern ctx exprType resultType lhs ptrn rhs = case ptrn of
    WildcardPattern -> const rhs
    VarPattern name -> const (Let False (metaType resultType) name TypeAny lhs rhs)
    LitPattern literal -> do
      let eqFun = Ident (metaType $ exprType ==> exprType ==> TypeBool) "=="
      let applyEq = Apply (metaType TypeBool) eqFun [lhs, Literal (metaType exprType) literal]
      If (metaType resultType) applyEq rhs
    ConstrPattern name args -> cond
      where
        cond fail = If (metaType resultType) constrCheck (checkArgs name fail) fail

        ctorTag tpe ctorName = do
            let tpName = typeName tpe
                ctors = fromMaybe (error "ctorTag") $ Map.lookup tpName $ ctx ^. constructorTags
                tag = fromMaybe (error "ctorTag") $ Map.lookup ctorName ctors
            tag

        constrCheck = if isStaticMode ctx
            then do
                let tag = ctorTag exprType name
                    checkTag = Ident (metaType $ exprType ==> TypeInt ==> TypeBool) (NS "Prelude" "runtimeCheckTag")
                Apply (metaType TypeBool) checkTag [lhs, Literal (metaType TypeInt) (IntLit tag)]
            else do
                let isConstr = Ident (metaType $ exprType ==> TypeString ==> TypeBool) (NS "Prelude" "runtimeIsConstr")
                    ctorName = Literal (metaType TypeString) $ StringLit (show name)
                Apply (metaType TypeBool) isConstr [lhs, ctorName]
        constrMap = ctx ^. constructorArgs
        checkArgs nm fail =  case Map.lookup nm constrMap of
            Nothing -> fail
            Just constrArgs | length args == length constrArgs -> do
                let argParam = zip args constrArgs
                foldr (\(a, Arg n t) acc -> do
                        let select = Select (metaType t) lhs (Ident (metaType $ exprType ==> t) n)
                        genPattern ctx t resultType select a acc fail
                    ) rhs argParam
            Just constrArgs -> error $ printf "Constructor %s has %d parameters, but %d given"
                (show nm) (length constrArgs) (length args) -- TODO box this error
