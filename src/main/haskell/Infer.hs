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
import Emit (Ctx, DataDef(..), dataDefs)

import Control.Monad.State
import Control.Monad.Except
import qualified Control.Lens as Lens
import Control.Lens.Operators

import Data.Monoid
import qualified Data.List as List
import Data.Foldable (foldr)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Debug.Trace as Debug
import Text.Printf

newtype TypeEnv = TypeEnv (Map.Map String Scheme) deriving (Monoid)

instance Show TypeEnv where
  show (TypeEnv subst) = "Γ = {\n" ++ elems ++ "}"
    where elems = List.foldl (\s (name, scheme) -> s ++ name ++ " : " ++ show scheme ++ "\n") "" (Map.toList subst)

data InferState = InferState {
  _count :: Int,
  ast :: [Expr]
}

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

runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = case runState (runExceptT m) initState of
  (Left err, st)  -> Left err
  (Right res, st) -> Right $ closeOver res

closeOver :: (Map.Map TVar Type, Type) -> Scheme
closeOver (sub, ty) = normalize sc
  where sc = generalize defaultTyenv (apply sub ty)

initState :: InferState
initState = InferState { _count = 0, ast = [] }

extend :: TypeEnv -> (String, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env

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

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set TVar

instance Substitutable Type where
  {-# INLINE apply #-}
  apply _ (TypeIdent a)       = TypeIdent a
  apply s t@(TVar a)     = Map.findWithDefault t a s
  apply s (t1 `TypeFunc` t2) = apply s t1 `TypeFunc` apply s t2
  apply s (TypeApply t [args]) = TypeApply (apply s t) [args] -- TODO do proper substitution
  apply s t = error $ "Wat? " ++ show s ++ ", " ++ show t

  ftv TypeIdent{}         = Set.empty
  ftv (TVar a)       = Set.singleton a
  ftv (t1 `TypeFunc` t2) = ftv t1 `Set.union` ftv t2
  ftv (TypeApply _ [])         = error "Should not be TypeApply without arguments!" -- TODO use NonEmpty List?
  ftv (TypeApply t args)       = ftv t  -- TODO do proper substitution

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
  count += 1
  return $ TVar $ TV (letters !! _count s)

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

infer :: Ctx -> TypeEnv -> Expr -> Infer (Subst, Type)
infer ctx env ex = case ex of
  Val n e -> do
    (s, t) <- infer ctx env e
--    Debug.trace ("Val " ++ n ++ " " ++ show s ++ " " ++ show t) return (s, t)
    return (s, t)

  Ident x -> lookupEnv env x

  Apply meta (Ident op) [e1, e2] | op `Map.member` ops -> do
    (s1, t1) <- infer ctx env e1
    (s2, t2) <- infer ctx env e2
    tv <- fresh
    s3 <- unify (TypeFunc t1 (TypeFunc t2 tv)) (ops Map.! op)
    return (s1 `compose` s2 `compose` s3, apply s3 tv)

  Apply meta e1 [arg] -> do
    tv <- fresh
    (s1, t1) <- infer ctx env e1
    (s2, t2) <- infer ctx (apply s1 env) arg
    s3       <- unify (apply s2 t1) (TypeFunc t2 tv)
    return (s3 `compose` s2 `compose` s1, apply s3 tv)

  Apply meta e1 args -> do
     let curried = foldl (\expr arg -> Apply meta expr [arg]) e1 args
     infer ctx env curried

  Let x e1 e2 -> do
    (s1, t1) <- infer ctx env e1
    let env' = apply s1 env
        t'   = generalize env' t1
    (s2, t2) <- infer ctx (env' `extend` (x, t')) e2
    return (s2 `compose` s1, t2)

  If cond tr fl -> do
    tv <- fresh
    inferPrim ctx env [cond, tr, fl] (typeBool `TypeFunc` tv `TypeFunc` tv `TypeFunc` tv)



  Fix e1 -> do
      tv <- fresh
      inferPrim ctx env [e1] ((tv `TypeFunc` tv) `TypeFunc` tv)

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
      (s1, t1) <- infer ctx env' e
      return (s1, apply s1 tv `TypeFunc` t1)

  Function name t args e -> do
    let largs = map (\(Arg a _) -> a) args
    let curried = Fix (foldr (\arg expr -> Lam arg expr) e (name:largs))
    infer ctx env ({-Debug.trace ("Func " ++ show curried)-} curried)

  Data name constructors -> error "Shouldn't happen!"
  Select meta tree expr -> infer ctx env (Apply meta expr [tree])

  Array exprs -> do
    tv <- fresh
    let tpe = foldr (\_ t -> tv `TypeFunc` t) (typeArray tv) exprs
    inferPrim ctx env exprs tpe

  Match expr cases -> do
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
--      Debug.traceM $ "Matching expression type " ++ show te
      (s2, te') <- foldM (inferCase te) (s1, tv) cases
--      Debug.traceM $ "Matching result type " ++ show te'
      return (s2, te')
      where
            inferCase :: Type -> (Subst, Type) -> Case -> Infer (Subst, Type)
            inferCase expectedType (s1, expectedResult) (Case pat e) = do
              (env1, patType) <- getPatType env pat   -- (name -> String, User)
              su <- unify expectedType patType
              let env2 = apply su env1
--              Debug.traceM $ printf "unify expectedType %s patType %s = %s %s" (show expectedType) (show patType) (show su) (show env2)
              (s2, te) <- infer ctx env2 e
              s3 <- unify expectedResult te
--              Debug.traceM $ printf "s1 = %s, s2 = %s, s3 = %s, cobined = %s" (show s1) (show s2) (show s3) (show (s3 `compose` s2 `compose` s1))
              return (s3 `compose` s2 `compose` s1, apply s3 te)

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
                  let restpe = apply subst result
                  let env'' =  apply subst env'
--                  Debug.traceM $ printf "restpe = %s" (show restpe)
                  return (env'', restpe)                   -- (name -> String, User)

  Literal meta lit ->
    return (nullSubst, litType lit)
  e -> error ("Wat? " ++ show e)

litType (IntLit _)    = typeInt
litType (FloatLit _)  = typeFloat
litType (BoolLit _)   = typeBool
litType (StringLit _) = typeString
litType UnitLit       = typeUnit


inferPrim :: Ctx -> TypeEnv -> [Expr] -> Type -> Infer (Subst, Type)
inferPrim ctx env l t = do
  tv <- fresh
  (s1, tf) <- foldM inferStep (nullSubst, id) l
  let composedType = apply s1 (tf tv)
--  Debug.traceM $ "composedType " ++ show composedType
  s2 <- unify composedType t
  return (s2 `compose` s1, apply s2 tv)
  where
  inferStep (s, tf) exp = do
    (s', t) <- infer ctx (apply s env) exp
    return (s' `compose` s, tf . TypeFunc t)

inferExpr :: Ctx -> TypeEnv -> Expr -> Either TypeError Scheme
inferExpr ctx env = runInfer . infer ctx env

inferTop :: Ctx -> TypeEnv -> [(String, Expr)] -> Either TypeError TypeEnv
inferTop ctx env [] = Right env
inferTop ctx env ((name, ex):xs) = case inferExpr ctx env ex of
  Left err -> Left err
  Right ty -> inferTop ctx (extend env (name, ty)) xs

normalize :: Scheme -> Scheme
normalize (Forall ts body) = Forall (fmap snd ord) (normtype body)
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
        Nothing -> error "type variable not in signature"

typeCheck :: Ctx -> [Expr] -> Either TypeError TypeEnv
typeCheck ctx exprs = do
  let (dat, other) = List.partition pred exprs
  let bbb = dat >>= ddd
  let a = map f other
  let (TypeEnv te) = defaultTyenv
  let typeEnv = TypeEnv (Map.union te (Map.fromList bbb))
  inferTop ctx typeEnv a
  where
            pred Data{} = True
            pred _      = False

            ddd :: Expr -> [(String, Scheme)]
            ddd (Data typeName constrs) = constrs >>= genScheme
              where
                 genScheme :: DataConst -> [(String, Scheme)]
                 genScheme (DataConst name args) =
                    let dataTypeIdent = TypeIdent typeName
                        tpe = foldr (\(Arg _ tpe) acc -> tpe `TypeFunc` acc) dataTypeIdent args
                        accessors = map (\(Arg n tpe) -> (n, Forall [] (TypeFunc dataTypeIdent tpe))) args
                    in (name, Forall [] tpe) : accessors
            ddd e = error ("What the hell" ++ show e)


            f e@(Fix (Function name _ _ _)) = (name, e)
            f e@(Val name _) = (name, e)
            f e@(Function name _ _ _) = (name, e)
            f e@(Extern name _ _) = (name, e)
            f e = error ("What the fuck " ++ show e)

