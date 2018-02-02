{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lasca.Infer (
  normalizeType,
  typeCheck,
  inferExpr,
  showTypeError,
  showPretty,
  defaultTyenv
) where

import Prelude hiding (foldr)

import Lasca.Type
import Lasca.Syntax

import Control.Monad.State
import Control.Monad.Except
import qualified Control.Lens as Lens
import Control.Lens.TH
import Control.Lens.Operators

import Data.Monoid
import qualified Data.List as List
import Data.Foldable (foldr)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace as Debug
import Text.Printf
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String

newtype TypeEnv = TypeEnv (Map Name Type) deriving (Monoid)


instance Pretty TypeEnv where
    pretty (TypeEnv subst) = "Γ = {" <+> line <+> indent 2 elems <+> "}"
      where elems = vcat $ map (\(name, scheme) -> pretty name <+> ":" <+> pretty scheme) (Map.toList subst)

instance Show TypeEnv where
    show (TypeEnv subst) = "Γ = {\n" ++ elems ++ "}"
      where elems = List.foldl' (\s (name, scheme) -> s ++ show name ++ " : " ++ showPretty scheme ++ "\n") "" (Map.toList subst)

showPretty :: Pretty a => a -> String
showPretty = renderString . layoutPretty defaultLayoutOptions . pretty

data InferState = InferState {_count :: Int, _current :: Expr}
makeLenses ''InferState

normalizeType tpe = case Set.toList $ ftv tpe of
                  [] -> tpe
                  tvars -> Forall tvars tpe

type Infer = ExceptT TypeError (State InferState)
type Subst = Map TVar Type

data TypeError
    = UnificationFail Expr Type Type
    | InfiniteType Expr TVar Type
    | UnboundVariable Expr Name
    | UnificationMismatch Expr [Type] [Type]
    | ArityMismatch Expr Type Int Int
    deriving (Eq, Ord, Show)

showTypeError typeError = case typeError of
    UnificationFail expr expected infered ->
        printf "%s: Type error: expected type %s but got %s in expression %s"
          (show $ exprPosition expr) (show expected) (show infered) (show expr)
    InfiniteType expr tvar tpe ->
        printf "%s: Type error: infinite type %s %s in expression %s"
          (show $ exprPosition expr) (show tvar) (show tpe) (show expr)
    UnboundVariable expr symbol ->
        printf "%s: Type error: unknown symbol %s in expression %s"
          (show $ exprPosition expr) (show symbol) (show expr)
    UnificationMismatch expr expected infered ->
        printf "%s: Type error: expected type %s but got %s in expression %s"
          (show $ exprPosition expr) (show expected) (show infered) (show expr)
    ArityMismatch expr tpe expected actual ->
        printf "%s: Call error: applying %s arguments to a function of type %s with arity %s in expression %s"
          (show $ exprPosition expr) (show actual) (show tpe) (show expected) (show expr)

class Substitutable a where
    substitute :: Subst -> a -> a
    ftv   :: a -> Set TVar

instance Substitutable Type where
    {-# INLINE substitute #-}
    substitute _ (TypeIdent a)       = TypeIdent a
    substitute s t@(TVar a)     = Map.findWithDefault t a s
    substitute s (t1 `TypeFunc` t2) = substitute s t1 `TypeFunc` substitute s t2
    substitute s (TypeApply t args) = TypeApply (substitute s t) (substitute s args)
    substitute s (Forall tvars t) = Forall tvars $ substitute s' t
                               where s' = foldr Map.delete s tvars
--    substitute s t = error $ "Wat? " ++ show s ++ ", " ++ show t

    ftv TypeIdent{}         = Set.empty
    ftv (TVar a)       = Set.singleton a
    ftv (t1 `TypeFunc` t2) = ftv t1 `Set.union` ftv t2
    ftv (TypeApply t args)       = ftv args
    ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable a => Substitutable [a] where
    {-# INLINE substitute #-}
    substitute s a = (fmap . substitute) s a
    ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
    {-# INLINE substitute #-}
    substitute s (TypeEnv env) =  TypeEnv $ Map.map (substitute s) env
    ftv (TypeEnv env) = ftv $ Map.elems env


nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (substitute s1) s2 `Map.union` s1

unify ::  Type -> Type -> Infer Subst
unify (l `TypeFunc` r) (l' `TypeFunc` r')  = do
    s1 <- unify l l'
    s2 <- unify (substitute s1 r) (substitute s1 r')
    return (s2 `compose` s1)

unify (TypeIdent "Any") t = return nullSubst
unify t (TypeIdent "Any") = return nullSubst
unify t (TVar a) = bind a t
unify (TVar a) t = bind a t
unify (TypeIdent a) (TypeIdent b) | a == b = return nullSubst
unify (TypeApply (TypeIdent lhs) largs) (TypeApply (TypeIdent rhs) rargs) | lhs == rhs = unifyList largs rargs
unify t1 t2 = do
    expr <- gets _current
    let pos = exprPosition expr
    throwError $ UnificationFail expr t1 t2

unifyList :: [Type] -> [Type] -> Infer Subst
-- unifyList x y | traceArgs ["unifyList", show x, show y] = undefined
unifyList [TVar _] [] = return nullSubst
unifyList [] [] = return nullSubst
unifyList (t1 : ts1) (t2 : ts2) = do
    su1 <- unify t1 t2
    su2 <- unifyList (substitute su1 ts1) (substitute su1 ts2)
    return (su2 `compose` su1)
unifyList t1 t2 = do
    expr <- gets _current
    throwError $ UnificationMismatch expr t1 t2

bind ::  TVar -> Type -> Infer Subst
bind a t
  | t == TVar a     = return nullSubst
  | occursCheck a t = do
      expr <- gets _current
      throwError $ InfiniteType expr a t
  | otherwise       = return $ Map.singleton a t

occursCheck ::  Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
    s <- get
    count += 1
    return $ TVar $ TV (show $ _count s)

runInfer :: Expr -> Infer (Subst, Type) -> Either TypeError (Type, Expr)
runInfer e m =
    case runState (runExceptT m) (initState e) of
        (Left err, st)  -> Left err
        (Right res, st) -> do
            let (schema, expr) = closeOver res $ _current st
            Right (schema, {-Debug.trace (show expr)-} expr)

closeOver :: (Subst, Type) -> Expr -> (Type, Expr)
closeOver (sub, ty) e = do
    let e' = substituteAll sub e
    let sc = generalize emptyTyenv (substitute sub ty)
    let (normalized, mapping) = normalize sc
    let e'' = closeOverInner mapping e'
    let e''' = Lens.set (metaLens.exprType) normalized e''
    (normalized, {-Debug.trace ("Full type for" ++ show e''')-} e''')

substituteAll sub e = updateMeta f e
  where f meta = meta { _exprType = substitute sub (_exprType meta) }


closeOverInner mapping e = do
  {-Debug.trace (printf "Closing over inner %s with mapping %s" (show e) (show mapping)) $-} updateMeta f e
  where
    schema = getExprType e
    f meta = case _exprType meta of
        Forall tv t -> meta { _exprType = Forall tv $ substype schema t mapping }
        t -> meta

updateMeta f e =
    case e of
        EmptyExpr -> e
        Literal meta lit -> Literal (f meta) lit
        Ident meta name -> Ident (f meta) name
        Apply meta expr exprs -> Apply (f meta) (updateMeta f expr) (map (updateMeta f) exprs)
        Lam meta name expr -> Lam (f meta) name (updateMeta f expr)
        Select meta tree expr -> Select (f meta) (updateMeta f tree) (updateMeta f expr)
        Match meta expr cases -> Match (f meta) (updateMeta f expr) (map (\(Case pat e) -> Case pat (updateMeta f e)) cases)
        this@BoxFunc{} -> this
        Function meta name tpe args expr -> Function (f meta) name tpe args (updateMeta f expr)
        If meta cond tr fl -> If (f meta) (updateMeta f cond) (updateMeta f tr) (updateMeta f fl)
        Let meta name expr body -> Let (f meta) name (updateMeta f expr) (updateMeta f body)
        Array meta exprs -> Array (f meta) (map (updateMeta f) exprs)
        this@Data{} -> this
        Package{} -> e
        Import{} -> e

normalize schema@(Forall ts body) =
    let res = (Forall (fmap snd ord) (normtype body), ord)
    in {-Debug.trace (show res)-} res
  where
    ord = let a = zip (List.nub $ fv body) (fmap TV letters)
          in {-Debug.trace ("mapping = " ++ show a)-} a   -- from [b, c, c, d, f, e, g] -> [a, b, c, d, e, f]

    fv (TVar a)   = [a]
    fv (TypeFunc a b) = fv a ++ fv b
    fv (TypeIdent _)   = []
    fv (TypeApply t [])       = error "Should not be TypeApply without arguments!" -- TODO use NonEmpty List?
    fv (TypeApply t args)   = fv t ++ (args >>= fv)  -- FIXME args?

    normtype (TypeFunc a b)  = TypeFunc  (normtype a) (normtype b)
    normtype (TypeApply a b) = TypeApply (normtype a) (map normtype b)
    normtype (TypeIdent a)   = TypeIdent a
    normtype (TVar a)        =
        case lookup a ord of
            Just x -> {-Debug.trace (printf "lookup for %s, found %s" (show a) (show x))-} (TVar x)
            Nothing -> error $ printf "Type variable %s not in signature %s" (show a) (show schema)
normalize t = (t, [])

substype schema (TypeFunc a b)  ord = TypeFunc  (substype schema a ord) (substype schema b ord)
substype schema (TypeApply a b) ord = TypeApply (substype schema a ord) b
substype schema (TypeIdent a)   ord = TypeIdent a
substype schema t@(TVar a)        ord =
    case lookup a ord of
        Just x -> TVar x
        Nothing -> t

initState :: Expr -> InferState
initState e = InferState { _count = 0, _current = e}

extend :: TypeEnv -> (Name, Type) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env

emptyTyenv = TypeEnv Map.empty

defaultTyenv :: TypeEnv
defaultTyenv = TypeEnv builtinFunctions

instantiate :: Type -> Infer Type
instantiate (Forall as t) = do
    as' <- mapM (const fresh) as
    let s = Map.fromList $ zip as as'
    return $ substitute s t
instantiate t = return t

generalize :: TypeEnv -> Type -> Type
generalize env t  = case Set.toList $ ftv t `Set.difference` ftv env of
    [] -> t
    vars -> Forall vars t

ops = Map.fromList [
    ("and", TypeBool `TypeFunc` TypeBool `TypeFunc` TypeBool),
    ("or", TypeBool `TypeFunc` TypeBool `TypeFunc` TypeBool)
    ]

--lookupEnv :: TypeEnv -> String -> Infer (Subst, Type)
lookupEnv (TypeEnv env) x =
    case Map.lookup x env of
        Nothing -> do
            expr <- gets _current
            throwError $ UnboundVariable expr x
        Just s  -> do t <- instantiate s
                      return (nullSubst, t)

setType :: Expr -> Infer ()
setType e = modify (\s -> s {_current = e })

infer :: Ctx -> TypeEnv -> Expr -> Infer (Subst, Type)
infer ctx env ex = case ex of
    Ident meta x -> do
        (s, t) <- lookupEnv env x
    --    traceM $ printf "Ident %s: %s = %s" x (show t) (show s)
        setType $ Ident (meta `withType` t) x
        return (s, t)

    Apply meta (Ident _ op) [e1, e2] | op `Map.member` ops -> do
        (s1, t1) <- infer ctx env e1
        (s2, t2) <- infer ctx env e2
        tv <- fresh
        s3 <- unify (TypeFunc t1 (TypeFunc t2 tv)) (ops Map.! op)
        return (s1 `compose` s2 `compose` s3, substitute s3 tv)

    this@(Apply meta expr args) -> do
        tv <- fresh
        (s1, exprType) <- infer ctx env expr
        expr1 <- gets _current
        let exprArity = funcTypeArity exprType
        let argLen = length args
        {- Check we don't partially apply a function.
           Currently we only support fully applied function calls.

           Note, argLen /= exprArity is a valid scenario
           exptType can be a free type variable

           def foo(f) = f(1, 2)

           here exprType would be TV "1", and argLen = 2
           We should infer f: Int -> Int -> a, and foo: (Int -> Int -> a) -> a
           Same applies to recursive calls.
        -}
        if argLen < exprArity then do
--            Debug.traceM $ printf "%s subst %s env %s" (show exprType) (show s1) (show env)
            throwError $ ArityMismatch this exprType exprArity argLen
        else do
            (s2, applyType, args') <- inferPrim ctx env args exprType
            let subst = s2 `compose` s1
            let subApplyType = substitute subst applyType
            let subExprType = substitute subst exprType
--            Debug.traceM $ printf "%s === %s, subst %s" (show subExprType) (show subApplyType) (show subst)
            setType $ Apply (meta `withType` subApplyType) expr1 args'
            return (subst, subApplyType)

    Let meta x e1 EmptyExpr -> do
        (s1, t1) <- infer ctx env e1
        e1' <- gets _current
--        Debug.traceM $ printf "Let %s = %s, type %s, subst %s, env %s" (show x) (show e1) (show t1) (show s1) (show env)
        setType $ Let (meta `withType` t1) x e1' EmptyExpr
--        when (not $ Set.null $ ftv s1) $ error $ printf "%s: Global val %s has free type variables: !" (showPosition meta) (show x) (ftv s1)
        return (s1, t1)

    Let meta x e1 e2 -> do
        (s1, t1) <- infer ctx env e1
        e1' <- gets _current
        let env' = substitute s1 env
            t'   = generalize env' t1
        (s2, t2) <- infer ctx (env' `extend` (x, t')) e2
        e2' <- gets _current
        setType $ Let (meta `withType` t2) x e1' e2'
        let subst = s2 `compose` s1
    --    traceM $ printf "let %s: %s in %s = %s" x (show $ substitute subst t1) (show t2) (show subst)
        return (subst, t2)

    If meta cond tr fl -> do

        condTv <- fresh
        (s1, t1) <- infer ctx env cond
        s2 <- unify (substitute s1 t1) TypeBool
        let substCond = s2 `compose` s1
        cond' <- gets _current

        tv <- fresh
        let env' = substitute substCond env
        (s3, trueType) <- infer ctx env' tr
        s4 <- unify (substitute s3 trueType) tv
        let substTrue = s4 `compose` s3
        tr' <- gets _current

        let env'' = substitute substTrue env'
        (s5, falseType) <- infer ctx env'' fl
        s6 <- unify (substitute s5 falseType) tv
        let substFalse = s6 `compose` s5
        fl' <- gets _current

        let subst = substFalse `compose` substTrue `compose` substCond
        let resultType = substitute subst tv

        setType $ If (meta `withType` resultType) cond' tr' fl'
    --    traceM $ printf "if %s then %s else %s: %s"  (show substCond) (show substTrue) (show substFalse) (show resultType)
        return (subst, resultType)

    Lam meta arg@(Arg x argType) e -> do
        case argType of
            TypeIdent "Any" -> do
                tv <- fresh
                let env' = env `extend` (x, tv)
                (s1, t1) <- infer ctx env' e
                e' <- gets _current
                let resultType = substitute s1 tv `TypeFunc` t1
                setType $ Lam (meta `withType` resultType) arg e'
                return (s1, resultType)
            _ -> do
                let generalizedArgType = generalize env argType
                let env' = env `extend` (x, argType)
                (s1, t1) <- infer ctx env' e
                e' <- gets _current
                let resultType = substitute s1 argType `TypeFunc` t1
                setType $ Lam (meta `withType` resultType) arg e'
                return (s1, resultType)

    Function meta name tpe args e ->
        if meta^.isExternal
        then do
            let argToType (Arg _ t) = t
            let f z t = z `TypeFunc` t
            let ts = map argToType args
            let t = normalizeType $ foldr f tpe ts
            return (nullSubst, t)
        else do
            -- functions are recursive, so do Fixpoint for inference
            let nameArg = Arg name typeAny
            let curried = foldr (Lam meta) e (nameArg : args)
            tv <- fresh
            tv1 <- fresh
            -- fixpoint
            (s1, ttt) <- infer ctx env curried
            let composedType = substitute s1 (TypeFunc ttt tv1)
--            traceM $ "composedType " ++ show composedType
            s2 <- unify composedType ((tv `TypeFunc` tv) `TypeFunc` tv)
            let (s, t) = (s2 `compose` s1, substitute s2 tv1)
            e' <- gets _current
            let uncurried = foldr (\_ (Lam _ _ e) -> e) e' (nameArg : args)
            setType $ Function (meta `withType` t) name tpe args uncurried
--            traceM $ printf "def %s(%s): %s, subs: %s" name (List.intercalate "," $ map show args) (show t) (show s)
            return (s, t)


    Data meta name tvars constructors -> error $ "Shouldn't happen! " ++ show meta
    Select meta tree expr -> do
        (s, t) <- infer ctx env (Apply meta expr [tree])
        (Apply _ e' [tree']) <- gets _current
        setType $ Select (meta `withType` t) tree' e'
        return (s, t)

    Array meta exprs -> do
        tv <- fresh
        let tpe = foldr (\_ t -> tv `TypeFunc` t) (typeArray tv) exprs
        (subst, t, exprs') <- inferPrim ctx env exprs tpe
        setType $ Array (meta `withType` t) exprs'
        return (subst, t)

    Match meta expr cases -> do
        tv <- fresh
        {-
          Consider data Role = Admin | Group(id: Int)
                   data User = Person(name: String, role: Role)
                   match Person("God", Admin) { | Person(_ ,Group(i)) -> true }

                   unify: User -> User -> Bool
                   unify: String -> Role
                            _       Group(i)
                                      Int
                                       i
        -}
        (s1, te) <- infer ctx env expr
        e' <- gets _current
  --      Debug.traceM $ "Matching expression type " ++ show te
        (s2, te', exprs') <- foldM (inferCase te) (s1, tv, []) cases
        let cases' = map (\((Case pat e), e') -> Case pat e') (zip cases exprs')
  --      Debug.traceM $ printf "Matching result type %s in %s" (show te') (show s2)
        setType $ Match (meta `withType` te') e' cases'
        return (s2, te')
      where
        inferCase :: Type -> (Subst, Type, [Expr]) -> Case -> Infer (Subst, Type, [Expr])
        inferCase expectedType (s1, expectedResult, exprs) (Case pat e) = do
            (env1, patType) <- getPatType env pat   -- (name -> String, User)
            su <- unify expectedType patType
            let env2 = substitute su env1
  --              Debug.traceM $ printf "unify expectedType %s patType %s = %s %s" (show expectedType) (show patType) (show su) (show env2)
            (s2, te) <- infer ctx env2 e
            s3 <- unify expectedResult te
  --              Debug.traceM $ printf "s1 = %s, s2 = %s, s3 = %s, combined = %s" (show s1) (show s2) (show s3) (show (s3 `compose` s2 `compose` su `compose` s1))
            let resultType = substitute s3 te
            e' <- gets _current
            return (s3 `compose` s2 `compose` su `compose` s1, resultType, exprs ++ [e'])

        getPatType :: TypeEnv -> Pattern -> Infer (TypeEnv, Type)
        getPatType env pat = case pat of
            WildcardPattern -> do
                tv <- fresh
                return (env, tv)       -- (0, a)
            (VarPattern n)    -> do                  -- (n -> a, a)
                tv <- fresh
                return (env `extend` (n, tv), tv)
            (LitPattern lit) -> return (env, litType lit)   -- (0, litType)
            (ConstrPattern name args) -> do          -- (name -> a, a -> User)
                (_, t) <- lookupEnv env name           -- t = String -> User
                result <- fresh                        -- result = a
                (env', pattype) <- foldM (\ (accEnv, accType) pat -> do { (e, t) <- getPatType env pat; return (accEnv `mappend` e, TypeFunc t accType) }) (env, result) (reverse args)
  --                  Debug.traceM $ "Holy shit " ++ show env' ++ ", " ++ show pattype
                subst <- unify pattype t
                let restpe = substitute subst result
                let env'' =  substitute subst env'
  --                  Debug.traceM $ printf "restpe = %s" (show restpe)
                return (env'', restpe)                   -- (name -> String, User)

    Literal meta lit -> do
        let tpe = litType lit
        setType $ Literal (meta `withType` tpe) lit
        return (nullSubst, tpe)
    e -> error ("Wat? " ++ show e)

litType (IntLit _)    = TypeInt
litType (FloatLit _)  = TypeFloat
litType (BoolLit _)   = TypeBool
litType (StringLit _) = typeString
litType UnitLit       = typeUnit


inferPrim :: Ctx -> TypeEnv -> [Expr] -> Type -> Infer (Subst, Type, [Expr])
inferPrim ctx env l t = do
    tv <- fresh
    (s1, tf, exprs) <- foldM inferStep (nullSubst, id, []) l
    let composedType = substitute s1 (tf tv)
--    Debug.traceM $ "composedType " ++ show composedType
    s2 <- unify composedType t
    return (s2 `compose` s1, substitute s2 tv, reverse exprs)
  where
    inferStep (s, tf, exprs) exp = do
        (s', t) <- infer ctx (substitute s env) exp
        exp' <- gets _current
        return (s' `compose` s, tf . TypeFunc t, exp' : exprs)

inferExpr :: Ctx -> TypeEnv -> Expr -> Either TypeError (Type, Expr)
inferExpr ctx env e = runInfer e $ infer ctx env e

inferTop :: Ctx -> TypeEnv -> [(Name, Expr)] -> Either TypeError (TypeEnv, [Expr])
inferTop ctx env [] = Right (env, [])
inferTop ctx env ((name, ex):xs) = case inferExpr ctx env ex of
    Left err -> Left err
    Right (ty, ex') -> case inferTop ctx (extend env (name, ty)) xs of
                            Left err -> Left err
                            Right (ty, exs) -> Right (ty, ex' : exs)

data InferStuff = InferStuff {
    _names :: [(Name, Expr)],
    _types :: [(Name, Type)],
    _datas :: [Expr],
    _currentPackage :: Name
}
makeLenses ''InferStuff

collectNames exprs = forM_ exprs collectName

collectName :: Expr -> State InferStuff ()
collectName expr = case expr of
    Let _ name _ EmptyExpr -> do
        names %= (++ [(name, expr)])
        return ()
    Function _ name _ _ _ -> do
        names %= (++ [(name, expr)])
        return ()
    dat@(Data _ typeName tvars constrs) -> do
        curPkg <- gets _currentPackage
        let genTypes :: DataConst -> [(Name, Type)]
            genTypes (DataConst name args) =
                let tpe = normalizeType $ foldr (\(Arg _ tpe) acc -> tpe `TypeFunc` acc) dataTypeIdent args
                    accessors = map (\(Arg n tpe) -> (n, normalizeType $ TypeFunc dataTypeIdent tpe)) args
                in (name, tpe) : accessors
        let constructorsTypes = constrs >>= genTypes
        types %= (++ constructorsTypes)
        datas %= (++ [dat])
        return ()
      where
        dataTypeIdent = case tvars of
            [] -> TypeIdent typeName
            tvars -> TypeApply (TypeIdent typeName) (map TVar tvars)


    Package meta name -> do
        currentPackage .= name
        return ()
    Import{} -> return ()
    _ -> error ("What the fuck " ++ show expr)

typeCheck :: Ctx -> [Expr] -> Either TypeError (TypeEnv, [Expr])
typeCheck ctx exprs = do
    let stuff = execState (collectNames exprs) (InferStuff {_names = [], _types = [], _datas = [], _currentPackage = Name "default"})
    let namedExprs = stuff ^. names
    let dataConstructorsEnv = Map.fromList $ stuff ^. types
    let (TypeEnv te) = defaultTyenv
    let typeEnv = TypeEnv (Map.union te dataConstructorsEnv)
    let res = inferTop ctx typeEnv namedExprs
    fmap (\(typeEnv, exprs) -> (typeEnv, stuff ^. datas ++ exprs)) res
