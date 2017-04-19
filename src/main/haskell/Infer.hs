{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE Strict #-}

module Infer where

import Prelude hiding (foldr)

import Type
import Syntax

import Control.Monad.State
import Control.Monad.Except

import Data.Monoid
import qualified Data.List as List
import Data.Foldable (foldr)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Debug.Trace as Debug

newtype TypeEnv = TypeEnv (Map.Map String Scheme)

instance Show TypeEnv where
  show (TypeEnv subst) = "Γ = {\n" ++ elems ++ "}"
    where elems = List.foldl (\s (name, scheme) -> s ++ name ++ " : " ++ show scheme ++ "\n") "" (Map.toList subst)

data Unique = Unique { count :: Int }

type Infer = ExceptT TypeError (State Unique)
type Subst = Map.Map TVar Type

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable String
  | UnificationMismatch [Type] [Type]
  deriving Show

runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = case evalState (runExceptT m) initUnique of
  Left err  -> Left err
  Right res -> Right $ closeOver res

closeOver :: (Map.Map TVar Type, Type) -> Scheme
closeOver (sub, ty) = normalize sc
  where sc = generalize emptyTyenv (apply sub ty)

initUnique :: Unique
initUnique = Unique { count = 0 }

extend :: TypeEnv -> (String, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env

emptyTyenv :: TypeEnv
emptyTyenv = TypeEnv (Map.fromList [
    ("+",  (Forall [a] (ta `TypeFunc` ta `TypeFunc` ta))),
    ("-",  (Forall [a] (ta `TypeFunc` ta `TypeFunc` ta))),
    ("*",  (Forall [a] (ta `TypeFunc` ta `TypeFunc` ta))),
    ("/",  (Forall [a] (ta `TypeFunc` ta `TypeFunc` ta))),
    ("==", (Forall [a] (ta `TypeFunc` ta `TypeFunc` typeBool))),
    ("!=", (Forall [a] (ta `TypeFunc` ta `TypeFunc` typeBool))),
    ("<",  (Forall [a] (ta `TypeFunc` ta `TypeFunc` typeBool))),
    ("<=", (Forall [a] (ta `TypeFunc` ta `TypeFunc` typeBool))),
    (">",  (Forall [a] (ta `TypeFunc` ta `TypeFunc` typeBool))),
    (">=", (Forall [a] (ta `TypeFunc` ta `TypeFunc` typeBool)))
  ])
  where a = TV "a"
        ta = TVar a

typeof :: TypeEnv -> String -> Maybe Type.Scheme
typeof (TypeEnv env) name = Map.lookup name env

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set TVar

instance Substitutable Type where
  {-# INLINE apply #-}
  apply _ (TypeIdent a)       = TypeIdent a
  apply s t@(TVar a)     = Map.findWithDefault t a s
  apply s (t1 `TypeFunc` t2) = apply s t1 `TypeFunc` apply s t2
  apply s (TypeApply t [args]) = TypeApply (apply s t) [args] -- TODO do proper substitution

  ftv TypeIdent{}         = Set.empty
  ftv (TVar a)       = Set.singleton a
  ftv (t1 `TypeFunc` t2) = ftv t1 `Set.union` ftv t2
  ftv (TypeApply t [args])       = ftv t  -- TODO do proper substitution

instance Substitutable Scheme where
  {-# INLINE apply #-}
  apply s (Forall as t)   = Forall as $ apply s' t
                            where s' = foldr Map.delete s as
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable a => Substitutable [a] where
  {-# INLINE apply #-}
  apply s a = (fmap . apply) s a
  ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
  {-# INLINE apply #-}
  apply s (TypeEnv env) =  TypeEnv $ Map.map (apply s) env
  ftv (TypeEnv env) = ftv $ Map.elems env


nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

unify ::  Type -> Type -> Infer Subst
unify (l `TypeFunc` r) (l' `TypeFunc` r')  = do
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  return (s2 `compose` s1)

unify (TypeIdent "Any") t = return nullSubst
unify t (TypeIdent "Any") = return nullSubst
unify t (TVar a) = bind a t
unify (TVar a) t = bind a t
unify (TypeIdent a) (TypeIdent b) | a == b = return nullSubst
unify (TypeApply (TypeIdent "Array") [t]) (TypeFunc typeInt r) = unify t r -- special case of array(idx) syntax. Hack.
unify (TypeFunc typeInt t) (TypeApply (TypeIdent "Array") [r]) = unify r t -- special case of array(idx) syntax. Hack.
unify (TypeApply (TypeIdent lhs) largs) (TypeApply (TypeIdent rhs) rargs) | lhs == rhs = (unifyList largs rargs)
unify t1 t2 = throwError $ UnificationFail t1 t2

unifyList :: [Type] -> [Type] -> Infer Subst
-- unifyList x y | traceArgs ["unifyList", show x, show y] = undefined
unifyList [TVar _] [] = return nullSubst
unifyList [] [] = return nullSubst
unifyList (t1 : ts1) (t2 : ts2) =
  do su1 <- unify t1 t2
     su2 <- unifyList (apply su1 ts1) (apply su1 ts2)
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
  put s{count = count s + 1}
  return $ TVar $ TV (letters !! count s)

instantiate ::  Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Map.fromList $ zip as as'
  return $ apply s t

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

infer :: TypeEnv -> Expr -> Infer (Subst, Type)
infer env ex = case ex of
  Val n e -> infer env e

  Var x -> lookupEnv env x

  Apply (Var op) [e1, e2] | op `Map.member` ops -> do
    (s1, t1) <- infer env e1
    (s2, t2) <- infer env e2
    tv <- fresh
    s3 <- unify (TypeFunc t1 (TypeFunc t2 tv)) (ops Map.! op)
    return (s1 `compose` s2 `compose` s3, apply s3 tv)

  Apply e1 [arg] -> do
    tv <- fresh
    (s1, t1) <- infer env e1
    (s2, t2) <- infer (apply s1 env) arg
    s3       <- unify (apply s2 t1) (TypeFunc t2 tv)
    return (s3 `compose` s2 `compose` s1, apply s3 tv)

  Apply e1 (args) -> do
     let curried = foldl (\expr arg -> Apply expr [arg]) e1 args
     infer env curried

  Let x e1 e2 -> do
    (s1, t1) <- infer env e1
    let env' = apply s1 env
        t'   = generalize env' t1
    (s2, t2) <- infer (env' `extend` (x, t')) e2
    return (s2 `compose` s1, t2)

  If cond tr fl -> do
    tv <- fresh
    inferPrim env [cond, tr, fl] (typeBool `TypeFunc` tv `TypeFunc` tv `TypeFunc` tv)

  Fix e1 -> do
      tv <- fresh
      inferPrim env [e1] ((tv `TypeFunc` tv) `TypeFunc` tv)

  Extern name tpe args -> do
    let ts = map argToType args
    let t = foldr f tpe ts
    return (nullSubst, t)
    where
          argToType (Arg _ t) = t
          f z t = z `TypeFunc` t

  Lam x e -> do
      tv <- fresh
      let env' = env `extend` (x, Forall [] tv)
      (s1, t1) <- infer env' e
      return (s1, apply s1 tv `TypeFunc` t1)

  Function name t (args) e -> do
    let largs = map (\(Arg a _) -> a) args
    let curried = Fix (foldr (\arg expr -> Lam arg expr) e (name:largs))
    infer env ({-Debug.trace ("Func " ++ show curried)-} curried)

  Data name constructors -> do
    return (nullSubst, typeUnit)

  Array exprs -> do
    tv <- fresh
    let tpe = foldr (\_ t -> tv `TypeFunc` t) (typeArray tv) exprs
    inferPrim env exprs tpe

  Literal (IntLit _)  -> return (nullSubst, typeInt)
  Literal (FloatLit _)  -> return (nullSubst, typeFloat)
  Literal (BoolLit _) -> return (nullSubst, typeBool)
  Literal (StringLit _) -> return (nullSubst, typeString)
  Literal (UnitLit) -> return (nullSubst, typeUnit)

inferPrim :: TypeEnv -> [Expr] -> Type -> Infer (Subst, Type)
inferPrim env l t = do
  tv <- fresh
  (s1, tf) <- foldM inferStep (nullSubst, id) l
  s2 <- unify (apply s1 (tf tv)) t
  return (s2 `compose` s1, apply s2 tv)
  where
  inferStep (s, tf) exp = do
    (s', t) <- infer (apply s env) exp
    return (s' `compose` s, tf . (TypeFunc t))

inferExpr :: TypeEnv -> Expr -> Either TypeError Scheme
inferExpr env = runInfer . infer env

inferTop :: TypeEnv -> [(String, Expr)] -> Either TypeError TypeEnv
inferTop env [] = Right env
inferTop env ((name, ex):xs) = case inferExpr env ex of
  Left err -> Left err
  Right ty -> inferTop (extend env (name, ty)) xs

normalize :: Scheme -> Scheme
normalize (Forall ts body) = Forall (fmap snd ord) (normtype body)
  where
    ord = zip (List.nub $ fv body) (fmap TV letters)

    fv (TVar a)   = [a]
    fv (TypeFunc a b) = fv a ++ fv b
    fv (TypeIdent _)   = []
    fv (TypeApply t [args])   = fv t

    normtype (TypeFunc a b)  = TypeFunc  (normtype a) (normtype b)
    normtype (TypeApply a b) = TypeApply (normtype a) b
    normtype (TypeIdent a)   = TypeIdent a
    normtype (TVar a)        =
      case lookup a ord of
        Just x -> TVar x
        Nothing -> error "type variable not in signature"

typeCheck :: [Expr] -> Either TypeError TypeEnv
typeCheck exprs = inferTop emptyTyenv ({-Debug.trace (show a)-} a)
  where
        typeEnv = emptyTyenv

        a = map f exprs
        f e@(Fix (Function name _ _ _)) = (name, e)
        f e@(Val name _) = (name, e)
        f e@(Function name _ _ _) = (name, e)
        f e@(Extern name _ _) = (name, e)
        f e@(Data name _) = (name, e)
        f e = error ("What the fuck " ++ show e)