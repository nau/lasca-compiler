{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
  deriving (Monoid)

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
emptyTyenv = TypeEnv Map.empty

typeof :: TypeEnv -> String -> Maybe Type.Scheme
typeof (TypeEnv env) name = Map.lookup name env

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set TVar

instance Substitutable Type where
  apply _ (TCon a)       = TCon a
  apply s t@(TVar a)     = Map.findWithDefault t a s
  apply s (t1 `TArr` t2) = apply s t1 `TArr` apply s t2

  ftv TCon{}         = Set.empty
  ftv (TVar a)       = Set.singleton a
  ftv (t1 `TArr` t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Scheme where
  apply s (Forall as t)   = Forall as $ apply s' t
                            where s' = foldr Map.delete s as
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable a => Substitutable [a] where
  apply s a = (fmap . apply) s a
  ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
  apply s (TypeEnv env) =  TypeEnv $ Map.map (apply s) env
  ftv (TypeEnv env) = ftv $ Map.elems env


nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

unify ::  Type -> Type -> Infer Subst
unify (l `TArr` r) (l' `TArr` r')  = do
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  return (s2 `compose` s1)

unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify (TCon a) (TCon b) | a == b || a == "Any" || b == "Any" = return nullSubst
unify t1 t2 = throwError $ UnificationFail t1 t2

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
  ("+", typeInt `TArr` typeInt `TArr` typeInt),
  ("-", typeInt `TArr` typeInt `TArr` typeInt),
  ("*", typeInt `TArr` typeInt `TArr` typeInt),
  ("/", typeInt `TArr` typeInt `TArr` typeInt),
  ("==", typeInt `TArr` typeInt `TArr` typeBool),
  ("!=", typeInt `TArr` typeInt `TArr` typeBool),
  ("<", typeInt `TArr` typeInt `TArr` typeBool),
  ("<=", typeInt `TArr` typeInt `TArr` typeBool),
  (">", typeInt `TArr` typeInt `TArr` typeBool),
  (">=", typeInt `TArr` typeInt `TArr` typeBool),
  ("and", typeBool `TArr` typeBool `TArr` typeBool),
  ("or", typeBool `TArr` typeBool `TArr` typeBool)
  ]

lookupEnv :: TypeEnv -> String -> Infer (Subst, Type)
lookupEnv (TypeEnv env) x =
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable (show x)
    Just s  -> do t <- instantiate s
                  return (nullSubst, t)

infer :: TypeEnv -> Expr -> Infer (Subst, Type)
infer env ex = case ex of

  Var x -> lookupEnv env x

  Apply e1 [arg] -> do
    tv <- fresh
    (s1, t1) <- infer env e1
    (s2, t2) <- infer (apply s1 env) arg
    s3       <- unify (apply s2 t1) (TArr t2 tv)
    return (s3 `compose` s2 `compose` s1, apply s3 tv)

  Apply (Var op) [e1, e2] | op `Map.member` ops -> do
    (s1, t1) <- infer env e1
    (s2, t2) <- infer env e2
    tv <- fresh
    s3 <- unify (TArr t1 (TArr t2 tv)) (ops Map.! op)
    return (s1 `compose` s2 `compose` s3, apply s3 tv)

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
    inferPrim env [cond, tr, fl] (typeBool `TArr` tv `TArr` tv `TArr` tv)

  Fix e1 -> do
      tv <- fresh
      inferPrim env [e1] ((tv `TArr` tv) `TArr` tv)

  Extern name tpe args -> do
    let ts = map argToType args
    let t = foldr f tpe ts
    return (nullSubst, t)
    where
          argToType (Arg _ t) = t
          f z t = z `TArr` t

  Lam x e -> do
      tv <- fresh
      let env' = env `extend` (x, Forall [] tv)
      (s1, t1) <- infer env' e
      return (s1, apply s1 tv `TArr` t1)

{-  Function name _ [] e -> do
    tv <- fresh
    (s1, t1) <- infer env e
    return (s1, t1)

  Function name _ [arg] e -> do
    tv <- fresh
    let (Arg name _) = arg
    let env' = env `extend` (name, Forall [] tv)
    (s1, t1) <- infer env' e
    return (s1, apply s1 tv `TArr` t1)-}

  Function name t (args) e -> do
    let largs = map (\(Arg a _) -> a) args
    let curried = Fix (foldr (\arg expr -> Lam arg expr) e (name:largs))
    infer env (Debug.trace ("Func " ++ show curried) curried)

  Literal (IntLit _)  -> return (nullSubst, typeInt)
  Literal (BoolLit _) -> return (nullSubst, typeBool)
  Literal (StringLit _) -> return (nullSubst, typeString)

inferPrim :: TypeEnv -> [Expr] -> Type -> Infer (Subst, Type)
inferPrim env l t = do
  tv <- fresh
  (s1, tf) <- foldM inferStep (nullSubst, id) l
  s2 <- unify (apply s1 (tf tv)) t
  return (s2 `compose` s1, apply s2 tv)
  where
  inferStep (s, tf) exp = do
    (s', t) <- infer (apply s env) exp
    return (s' `compose` s, tf . (TArr t))

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
    fv (TArr a b) = fv a ++ fv b
    fv (TCon _)   = []

    normtype (TArr a b) = TArr (normtype a) (normtype b)
    normtype (TCon a)   = TCon a
    normtype (TVar a)   =
      case lookup a ord of
        Just x -> TVar x
        Nothing -> error "type variable not in signature"