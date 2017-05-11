{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Infer (
  typeCheck,
  inferExpr,
  defaultTyenv
) where

import Prelude hiding (foldr)

import Type
import Syntax

import Control.Monad.State
import Control.Monad.Except
import qualified Control.Lens as Lens
import Control.Lens.Operators

import Data.Monoid
import qualified Data.List as List
import Data.Foldable (foldr)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace as Debug
import Text.Printf

newtype TypeEnv = TypeEnv (Map.Map String Scheme) deriving (Monoid)

instance Show TypeEnv where
  show (TypeEnv subst) = "Γ = {\n" ++ elems ++ "}"
    where elems = List.foldl (\s (name, scheme) -> s ++ name ++ " : " ++ show scheme ++ "\n") "" (Map.toList subst)

data InferState = InferState {_count :: Int, _current :: Expr}

count :: Lens.Lens' InferState Int
count = Lens.lens _count (\c e -> c { _count = e } )

type Infer = ExceptT TypeError (State InferState)
type Subst = Map.Map TVar Type

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable String
  | UnificationMismatch [Type] [Type]
  deriving (Eq, Ord)

instance Show TypeError where
  show (UnificationFail t1 t2) = "Unification Fail " ++ show t1 ++ " " ++ show t2
  show (InfiniteType t1 t2) = "InfiniteType " ++ show t1 ++ " " ++ show t2
  show (UnboundVariable s) = "UnboundVariable " ++ s
  show (UnificationMismatch t1 t2) = "UnificationMismatch " ++ show t1 ++ " " ++ show t2


class Substitutable a where
  substitute :: Subst -> a -> a
  ftv   :: a -> Set.Set TVar

instance Substitutable Type where
  {-# INLINE substitute #-}
  substitute _ (TypeIdent a)       = TypeIdent a
  substitute s t@(TVar a)     = Map.findWithDefault t a s
  substitute s (t1 `TypeFunc` t2) = substitute s t1 `TypeFunc` substitute s t2
  substitute s (TypeApply t [args]) = TypeApply (substitute s t) [substitute s args]
  substitute s t = error $ "Wat? " ++ show s ++ ", " ++ show t

  ftv TypeIdent{}         = Set.empty
  ftv (TVar a)       = Set.singleton a
  ftv (t1 `TypeFunc` t2) = ftv t1 `Set.union` ftv t2
  ftv (TypeApply _ [])         = error "Should not be TypeApply without arguments!" -- TODO use NonEmpty List?
  ftv (TypeApply t args)       = ftv t  -- TODO do proper substitution

instance Substitutable Scheme where
  {-# INLINE substitute #-}
  substitute s (Forall as t)   = Forall as $ substitute s' t
                            where s' = foldr Map.delete s as
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
unify (TypeApply (TypeIdent "Array") [t]) (TypeFunc typeInt r) = unify t r -- FIXME special case of array(idx) syntax. Hack.
unify (TypeFunc typeInt t) (TypeApply (TypeIdent "Array") [r]) = unify r t -- FIXME special case of array(idx) syntax. Hack.
unify (TypeApply (TypeIdent lhs) largs) (TypeApply (TypeIdent rhs) rargs) | lhs == rhs = unifyList largs rargs
unify t1 t2 = throwError $ UnificationFail t1 t2

unifyList :: [Type] -> [Type] -> Infer Subst
-- unifyList x y | traceArgs ["unifyList", show x, show y] = undefined
unifyList [TVar _] [] = return nullSubst
unifyList [] [] = return nullSubst
unifyList (t1 : ts1) (t2 : ts2) =
  do su1 <- unify t1 t2
     su2 <- unifyList (substitute su1 ts1) (substitute su1 ts2)
     return (su2 `compose` su1)
unifyList t1 t2 = throwError $ UnificationMismatch t1 t2

bind ::  TVar -> Type -> Infer Subst
bind a t
  | t == TVar a     = return nullSubst
  | occursCheck a t = throwError $ InfiniteType a t
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

runInfer :: Expr -> Infer (Subst, Type) -> Either TypeError (Scheme, Expr)
runInfer e m =
  case runState (runExceptT m) (initState e) of
    (Left err, st)  -> Left err
    (Right res, st) -> do
      let (schema, expr) = closeOver res $ _current st
      Right (schema, expr)

closeOver :: (Subst, Type) -> Expr -> (Scheme, Expr)
closeOver (sub, ty) e = do
  let e' = substituteAll sub e
  let sc = generalize emptyTyenv (substitute sub ty)
  (normalize sc, e')

substituteAll sub e = updateMeta f e
  where f meta = meta { symbolType = substitute sub (symbolType meta) }

updateMeta f e =
  case e of
    Literal meta lit -> Literal (f meta) lit
    Ident meta name -> Ident (f meta) name
    Val meta name expr -> Val (f meta) name (updateMeta f expr)
    Apply meta expr exprs -> Apply (f meta) (updateMeta f expr) (map (updateMeta f) exprs)
    Lam meta name expr -> Lam (f meta) name (updateMeta f expr)
    Select meta tree expr -> Select (f meta) (updateMeta f tree) (updateMeta f expr)
    Match meta expr cases -> Match (f meta) (updateMeta f expr) (map (\(Case pat e) -> Case pat (updateMeta f e)) cases)
    this@BoxFunc{} -> this
    Function meta name tpe args expr -> Function (f meta) name tpe args (updateMeta f expr)
    this@Extern{} -> this
    If meta cond tr fl -> If (f meta) (updateMeta f cond) (updateMeta f tr) (updateMeta f fl)
    Let meta name expr body -> Let (f meta) name (updateMeta f expr) (updateMeta f body)
    Array meta exprs -> Array (f meta) (map (updateMeta f) exprs)
    this@Data{} -> this
               
normalize :: Scheme -> Scheme
normalize schema@(Forall ts body) = Forall (fmap snd ord) (normtype body)
  where
    ord = zip (List.nub $ fv body) (fmap TV letters)  -- from [b, c, c, d, f, e, g] -> [a, b, c, d, e, f]

    fv (TVar a)   = [a]
    fv (TypeFunc a b) = fv a ++ fv b
    fv (TypeIdent _)   = []
    fv (TypeApply t [])       = error "Should not be TypeApply without arguments!" -- TODO use NonEmpty List?
    fv (TypeApply t args)   = fv t  -- FIXME args?

    normtype (TypeFunc a b)  = TypeFunc  (normtype a) (normtype b)
    normtype (TypeApply a b) = TypeApply (normtype a) b
    normtype (TypeIdent a)   = TypeIdent a
    normtype (TVar a)        =
      case lookup a ord of
        Just x -> TVar x
        Nothing -> error $ printf "Type variable %s not in signature %s" (show a) (show schema)

initState :: Expr -> InferState
initState e = InferState { _count = 0, _current = e}

extend :: TypeEnv -> (String, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env

emptyTyenv = TypeEnv Map.empty

defaultTyenv :: TypeEnv
defaultTyenv = TypeEnv (Map.fromList [
    ("+",  Forall [a] (ta `TypeFunc` ta `TypeFunc` ta)),
    ("-",  Forall [a] (ta `TypeFunc` ta `TypeFunc` ta)),
    ("*",  Forall [a] (ta `TypeFunc` ta `TypeFunc` ta)),
    ("/",  Forall [a] (ta `TypeFunc` ta `TypeFunc` ta)),
    ("==", Forall [a] (ta `TypeFunc` ta `TypeFunc` typeBool)),
    ("!=", Forall [a] (ta `TypeFunc` ta `TypeFunc` typeBool)),
    ("<",  Forall [a] (ta `TypeFunc` ta `TypeFunc` typeBool)),
    ("<=", Forall [a] (ta `TypeFunc` ta `TypeFunc` typeBool)),
    (">",  Forall [a] (ta `TypeFunc` ta `TypeFunc` typeBool)),
    (">=", Forall [a] (ta `TypeFunc` ta `TypeFunc` typeBool))
  ])
  where a = TV "a"
        ta = TVar a

instantiate ::  Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Map.fromList $ zip as as'
  return $ substitute s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t  = Forall as t
  where as = Set.toList $ ftv t `Set.difference` ftv env

ops = Map.fromList [
  ("and", typeBool `TypeFunc` typeBool `TypeFunc` typeBool),
  ("or", typeBool `TypeFunc` typeBool `TypeFunc` typeBool)
  ]

lookupEnv :: TypeEnv -> String -> Infer (Subst, Type)
lookupEnv (TypeEnv env) x =
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable x
    Just s  -> do t <- instantiate s
                  return (nullSubst, t)

toSchema = Forall []

fromSchema (Forall [] t) = t

withType meta t = meta { symbolType = toSchema t }

setType :: Expr -> Infer ()
setType e = modify (\s -> s {_current = e })

infer :: Ctx -> TypeEnv -> Expr -> Infer (Subst, Type)
infer ctx env ex = case ex of
  Val meta n e -> do
    (s, t) <- infer ctx env e
    e' <- gets _current
    setType $ Val (meta `withType` t) n e'
--    Debug.trace ("Val " ++ n ++ " " ++ show s ++ " " ++ show t) return (s, t)
    return (s, t)

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

  Apply meta e1 [arg] -> do
    tv <- fresh
    (s1, t1) <- infer ctx env e1
    e1' <- gets _current
    (s2, t2) <- infer ctx (substitute s1 env) arg
    e2' <- gets _current
    s3       <- unify (substitute s2 t1) (TypeFunc t2 tv)
    let tpe = substitute s3 tv
    setType $ Apply (meta `withType` tpe) e1' [e2']
    return (s3 `compose` s2 `compose` s1, tpe)

  Apply meta e1 args -> do
     let curried = List.foldl' (\expr arg -> Apply meta expr [arg]) e1 args
     (subst, t) <- infer ctx env curried
     e' <- gets _current
     let uncurried = foldr (\_ (Apply _ e _) -> e) e' args
     setType $ Apply (meta `withType` t) uncurried args
     return (subst, t)

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
    s2 <- unify (substitute s1 t1) typeBool
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

  Extern name tpe args -> do
    let ts = map argToType args
    let t = foldr f tpe ts
    return (nullSubst, t)
    where
          argToType (Arg _ t) = t
          f z t = z `TypeFunc` t

  Lam meta x e -> do
      tv <- fresh
      let env' = env `extend` (x, Forall [] tv)
      (s1, t1) <- infer ctx env' e
      e' <- gets _current
      let resultType = substitute s1 tv `TypeFunc` t1
      setType $ Lam (meta `withType` resultType) x e'
      return (s1, resultType)

  Function meta name tpe args e -> do
    let largs = map (\(Arg a _) -> a) args
    -- functions are recursive, so do Fixpoint for inference
    let curried = foldr (\arg expr -> Lam emptyMeta arg expr) e (name:largs)
    tv <- fresh
    tv1 <- fresh
    -- fixpoint
    (s1, ttt) <- infer ctx env curried
    let composedType = substitute s1 (TypeFunc ttt tv1)
--    traceM $ "composedType " ++ show composedType
    s2 <- unify composedType ((tv `TypeFunc` tv) `TypeFunc` tv)
    let (s, t) = (s2 `compose` s1, substitute s2 tv1)
    e' <- gets _current
    let uncurried = foldr (\_ (Lam _ _ e) -> e) e' (name:largs)
    setType $ Function (meta `withType` t) name tpe args uncurried
--    traceM $ printf "def %s(%s): %s, subs: %s" name (List.intercalate "," $ map show args) (show t) (show s)
    return (s, t)


  Data meta name constructors -> error $ "Shouldn't happen! " ++ show meta
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
                  return (env `extend` (n, Forall [] tv), tv)
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
    setType $ Literal (meta `withType`tpe) lit
    return (nullSubst, tpe)
  e -> error ("Wat? " ++ show e)

litType (IntLit _)    = typeInt
litType (FloatLit _)  = typeFloat
litType (BoolLit _)   = typeBool
litType (StringLit _) = typeString
litType UnitLit       = typeUnit


inferPrim :: Ctx -> TypeEnv -> [Expr] -> Type -> Infer (Subst, Type, [Expr])
inferPrim ctx env l t = do
  tv <- fresh
  (s1, tf, exprs) <- foldM inferStep (nullSubst, id, []) l
  let composedType = substitute s1 (tf tv)
--  Debug.traceM $ "composedType " ++ show composedType
  s2 <- unify composedType t
  return (s2 `compose` s1, substitute s2 tv, exprs)
  where
  inferStep (s, tf, exprs) exp = do
    (s', t) <- infer ctx (substitute s env) exp
    exp' <- gets _current
    return (s' `compose` s, tf . TypeFunc t, exprs ++ [exp'])

inferExpr :: Ctx -> TypeEnv -> Expr -> Either TypeError (Scheme, Expr)
inferExpr ctx env e = runInfer e $ infer ctx env e

inferTop :: Ctx -> TypeEnv -> [(String, Expr)] -> Either TypeError (TypeEnv, [Expr])
inferTop ctx env [] = Right (env, [])
inferTop ctx env ((name, ex):xs) = case inferExpr ctx env ex of
  Left err -> Left err
  Right (ty, ex') -> case inferTop ctx (extend env (name, ty)) xs of
                        Left err -> Left err
                        Right (ty, exs) -> Right (ty, ex' : exs)

typeCheck :: Ctx -> [Expr] -> Either TypeError (TypeEnv, [Expr])
typeCheck ctx exprs = do
  let (dat, other) = List.partition pred exprs
  let dataConstructorsEnv = dat >>= ddd
  let a = map f other
  let (TypeEnv te) = defaultTyenv
  let typeEnv = TypeEnv (Map.union te (Map.fromList dataConstructorsEnv))
  let res = inferTop ctx typeEnv a
  fmap (\(typeEnv, exprs) -> (typeEnv, dat ++ exprs)) res
  where
            pred Data{} = True
            pred _      = False

            ddd :: Expr -> [(String, Scheme)]
            ddd (Data _ typeName constrs) = constrs >>= genScheme
              where
                 genScheme :: DataConst -> [(String, Scheme)]
                 genScheme (DataConst name args) =
                    let dataTypeIdent = TypeIdent typeName
                        tpe = foldr (\(Arg _ tpe) acc -> tpe `TypeFunc` acc) dataTypeIdent args
                        accessors = map (\(Arg n tpe) -> (n, Forall [] (TypeFunc dataTypeIdent tpe))) args
                    in (name, Forall [] tpe) : accessors
            ddd e = error ("What the hell" ++ show e)


            f e@(Val _ name _) = (name, e)
            f e@(Function _ name _ _ _) = (name, e)
            f e@(Extern name _ _) = (name, e)
            f e = error ("What the fuck " ++ show e)

