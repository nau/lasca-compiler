{-# LANGUAGE Strict #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Namer (
    NamerPhaseState(..),
    namerPhase
) where

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
import qualified Data.MultiSet as MSet
import qualified Data.Sequence as Seq
import qualified Debug.Trace as Debug

import Syntax
import Type
import Options (LascaOpts)

data NamerPhaseState = NamerPhaseState {
    _currentPackage :: Name,
    _importedPackages :: Set.Set Name,
    _exportedNames :: Map.Map Name (Set.Set Name),
    _exportedTypes :: Map.Map Name (Set.Set Name),
    _locals :: MSet.MultiSet Name,
    _context :: Ctx
} deriving (Show)
makeLenses ''NamerPhaseState

emptyNamerPhaseState opts = NamerPhaseState {
    _currentPackage = Name "default", -- TODO check it
    _importedPackages = Set.empty,
    _exportedNames = Map.empty,
    _exportedTypes = Map.empty,
    _locals = MSet.empty,
    _context = emptyCtx opts
}

importedNames state = Map.restrictKeys (state^.exportedNames) (Set.insert (state^.currentPackage) (state^.importedPackages))

findAllPackages state name = Map.foldrWithKey folder [] (state^.exportedNames)
  where
    folder pkg names res = if Set.member name names then pkg : res else res

resolveName name meta = do
    s <- get
    if Map.member name builtinFunctions || MSet.member name (s^.locals)
    then return name
    else return $ resolveNonLocalName name meta s

resolveNonLocalName name meta s = do
    case name of
        Name n -> case findAllPackages s name of
            [pkg] -> (NS pkg name)
            [] -> error $ printf "%s: Couldn't find name %s. Exported Names %s" (showPosition meta) (show n) (show (s^.exportedNames))
            pkgs -> error $ printf "%s: Ambiguous name %s found in packages %s" (showPosition meta) (show n) (show pkgs)
        NS pkg n ->
            case Map.lookup pkg (s^.exportedNames) of
                -- correctly qualified name
                Just names | Set.member n names -> name
                -- incorrectly qualified name, TODO add levenshtein distance based suggestions
                Just names -> error $ printf "%s: Couldn't find name %s in package %s." (showPosition meta) (show n) (show pkg)
                Nothing -> error $ printf "%s: No such package imported: %s at %s.\nAdd import %s" (showPosition meta) (show pkg) (show meta) (show pkg)


processData (Data _ name tvars consts) = do
    let dataDef = DataDef name consts
    let (funcs, vals) = foldl (\(funcs, vals) (DataConst n args) ->
                          if null args
                          then (funcs, n : vals)
                          else let dataTypeIdent = TypeIdent name
                                   tpe = (foldr (\(Arg _ tpe) acc -> tpe `TypeFunc` acc) dataTypeIdent args) -- FIXME forall
                                   meta = emptyMetaWithType tpe
                                   funDef = Function meta n dataTypeIdent args EmptyExpr
                               in ((n, funDef) : funcs, vals)) ([], []) consts
    -- FIXME Merge with above, create State?
    let argsWithIds = List.foldl' (\acc (DataConst _ args) ->
                                      fst $ List.foldl' (\(acc, idx) arg@(Arg n t)  ->
                                          if n `Map.member` acc
                                          then error $ printf "Field %s already defined in data %s" (show n) (show name)
                                          else (Map.insert n (arg, idx) acc, idx + 1) -- field name -> (S.Arg, field index in constructor) mapping
                                      ) (acc, 0) args
                                  ) Map.empty consts


    state <- get
    let s = state^.context
    context .= s {
        dataDefs =  dataDef : dataDefs s,
        dataDefsNames = Set.insert name (dataDefsNames s),
        dataDefsFields = Map.insert name argsWithIds (dataDefsFields s)
    }
    context.globalVals %= Set.union (Set.fromList vals)
    context.globalFunctions %= Map.union (Map.fromList funcs)


collectNames exprs = forM_ exprs $ \expr -> case expr of
    Let _ name _ EmptyExpr -> do
        curPkg <- gets _currentPackage
        exportedNames %= Map.adjust (Set.insert name) curPkg -- add this val name as current package exported name
        return ()
    Function _ name _ _ _ -> do
        curPkg <- gets _currentPackage
        exportedNames %= Map.adjust (Set.insert name) curPkg -- add this function name as current package exported name
        return ()
    dat@(Data _ typeName tvars constrs) -> do
        curPkg <- gets _currentPackage
        let constrNames = Set.fromList $ constrs >>= (\(DataConst name args) -> name : (fmap (\(Arg n t) -> n) args))
        exportedNames %= Map.adjust (Set.union constrNames) curPkg
        return ()
    Package _ name -> do
        currentPackage .= name
        -- for now, support only unique package names per file
        exportedNames %= Map.insert name Set.empty
        exportedTypes %= Map.insert name Set.empty
        return ()
    Import _ name -> do
        return ()
    _ -> error ("What the fuck " ++ show expr)

qualifiedSelect expr = case expr of
    Select meta (Ident _ p) (Ident _ n) -> Just $ NS p n
    Select meta e (Ident _ n) -> fmap (\prefix -> NS prefix n) (qualifiedSelect e)
    _ -> Nothing

isFullyQualifiedName state (NS pkgName name) =
    case Map.lookup pkgName (state^.exportedNames) of
        -- correctly qualified name
        Just names | Set.member name names -> True
        _ -> False
isFullyQualifiedName _ _ = False

namerTransform :: Expr -> State NamerPhaseState Expr
namerTransform expr = do
--    Debug.traceM $ printf "Current expr %s" (show expr)
    case expr of
        Package _ name -> do
            currentPackage .= name
            importedPackages .= Set.empty
            locals .= MSet.empty
--            Debug.traceM $ printf "Current package %s" (show name)
            return expr
        Import _ name -> do
            importedPackages %= Set.insert name
--            Debug.traceM $ printf "importedPackages %s" (show name)
            return expr
        Data meta name tvars constrs -> do
            curPkg <- gets _currentPackage
            let renamedArgs args = fmap (\(Arg name t) -> Arg (NS curPkg name) t) args
            let renamed = map (\(DataConst name args) -> DataConst (NS curPkg name) (renamedArgs args)) constrs
            let updated = Data meta name tvars renamed
            processData updated
            return updated
        Ident m n -> do
            fqn <- resolveName n m
            return (Ident m fqn)
        Array meta exprs -> do
            exprs' <- mapM go exprs
            return $ Array meta exprs'
        select@(Select meta tree expr) -> do
            state <- get
            case qualifiedSelect select of
                Just name | isFullyQualifiedName state name -> do
--                    Debug.traceM $ printf "Qualified Select %s" (show expr)
                    return $ Ident meta name
                _ -> do
--                    Debug.traceM $ printf "Other Select %s" (show expr)
                    tree' <- go tree
                    expr' <- go expr
                    return $ Select meta tree' expr'
        If meta cond true false -> do
            cond' <- go cond
            true' <- go true
            false' <- go false
            return (If meta cond' true' false')
        Match meta expr cases -> do
            expr' <- go expr
            cases1 <- forM cases $ \(Case p expr) -> do
                let collectLocals p =
                        case p of
                            VarPattern n -> MSet.singleton n
                            ConstrPattern n patterns -> foldr (\p ls -> MSet.union ls (collectLocals p)) MSet.empty patterns
                            _ -> MSet.empty
                let ls = collectLocals p
                s <- get
                let resolvePattern (ConstrPattern n patterns) = ConstrPattern (resolveNonLocalName n meta s) (map resolvePattern patterns)
                    resolvePattern p = p
                locals %= MSet.union ls
                expr' <- go expr
                s <- get
                locals %= (\l -> MSet.difference l ls)
                return (Case (resolvePattern p) expr')
            return (Match meta expr' cases1)
        -- global val
        Let meta name e EmptyExpr -> do
            e' <- go e
            curPkg <- gets _currentPackage
            let fqn = NS curPkg name
            context.globalVals %= Set.insert fqn
            return (Let meta fqn e' EmptyExpr)
        Let meta n e body -> do
            locals %= MSet.insert n
            e' <- go e
            body' <- go body
            locals %= MSet.delete n
            return (Let meta n e' body')
        Lam m a@(Arg n t) e -> do
            locals %= MSet.insert n
            e' <- go e
            locals %= MSet.delete n
            return (Lam m a e')
        Apply meta e args -> do
            e' <- go e
            args' <- mapM go args
            return (Apply meta e' args')
        Function meta name tpe args e1 -> do
            locals .= MSet.fromList (map (\(Arg n t) -> n) args)
            e' <- go e1
--            Debug.traceM $ printf "Current Function %s" (show name)
            locals .= MSet.empty
            curPkg <- gets _currentPackage
            let fqn = NS curPkg name
            context.globalFunctions %= Map.insert fqn expr
            return (Function meta fqn tpe args e')
        e -> return e
        where go e = namerTransform e

namerPhase :: LascaOpts -> [Expr] -> ([Expr], NamerPhaseState)
namerPhase opts exprs = do
    runState namer (emptyNamerPhaseState opts)
  where
    namer = do
      collectNames exprs
      forM exprs namerTransform