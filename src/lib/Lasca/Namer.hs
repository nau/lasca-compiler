{-# LANGUAGE Strict #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Lasca.Namer (
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
import Data.Foldable
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
import qualified Data.MultiSet as MSet
import qualified Data.Sequence as Seq
import qualified Debug.Trace as Debug

import Lasca.Syntax
import Lasca.Type
import Lasca.Options (LascaOpts)

data NamerPhaseState = NamerPhaseState {
    _currentPackage :: Name,
    _importedPackages :: Set Name,
    _packageQualifiers :: Map Name Name,  -- ^ MultiSet -> Data.MultiSet mapping
    _exportedNames :: Map Name (Set Name),
    _exportedTypes :: Map Name (Set Name),
    _locals :: MSet.MultiSet Name,
    _context :: Ctx
} deriving (Show)
makeLenses ''NamerPhaseState

emptyNamerPhaseState opts = NamerPhaseState {
    _currentPackage = Name "default", -- TODO check it
    _importedPackages = Set.empty,
    _packageQualifiers = Map.empty,
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


processData (Data meta name tvars consts) = do
    let dataDef = DataDef name consts
    let (funcs, vals, fieldsMap) = List.foldl' processConstructor ([], [], Map.empty) consts
    state <- get
    let s = state^.context
    context.dataDefs %= (:) dataDef
    context.dataDefsNames %= Set.insert name
    context.dataDefsFields %= Map.insert name fieldsMap
    context.globalVals %= Set.union (Set.fromList vals)
    context.globalFunctions %= Map.union (Map.fromList funcs)
  where
    processConstructor (funcs, vals, fieldsMap) (DataConst n args) =
        let updatedArgsWithIds = processConstructorArguments fieldsMap args
            (updatedFuncs, updatedVals) = if null args then (funcs, n : vals)
                else let dataTypeIdent = TypeIdent name
                         tpe = (foldr (\(Arg _ tpe) acc -> tpe `TypeFunc` acc) dataTypeIdent args) -- FIXME forall
                         m = meta `withType` tpe
                         funDef = Function meta n dataTypeIdent args EmptyExpr
                     in ((n, funDef) : funcs, vals)
        in (updatedFuncs, updatedVals, updatedArgsWithIds)

    processConstructorArguments fieldsMap args = List.foldl' (\fields (arg@(Arg n t), idx)  ->
            if n `Map.member` fields
            then error $ printf "Field %s already defined in data %s" (show n) (show name)
            else Map.insert n (arg, idx) fields -- field name -> (S.Arg, field index in constructor) mapping
        ) fieldsMap (zip args [0..])

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

isQualifiedName state qualifier name = do
    let fqpn = Map.lookup qualifier (state^.packageQualifiers)
    any (\fqpn -> isFullyQualifiedName state (NS fqpn name)) fqpn

findQualifiedName state qualifier name = do
    fqpn <- Map.lookup qualifier (state^.packageQualifiers)
    names <- Map.lookup fqpn (state^.exportedNames)
    if Set.member name names then return $ NS fqpn name else Nothing


namerTransform :: Expr -> State NamerPhaseState Expr
namerTransform expr = do
--    Debug.traceM $ printf "Current expr %s" (show expr)
    case expr of
        Package _ name -> do
            currentPackage .= name
            importedPackages .= Set.empty
            packageQualifiers .= Map.empty
            locals .= MSet.empty
--            Debug.traceM $ printf "Current package %s" (show name)
            return expr
        Import _ name -> do
            importedPackages %= Set.insert name
            case name of
                NS prefix qual -> packageQualifiers %= Map.insert qual name
                _ -> return ()
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
                Just nm@(NS qualifier name) | isQualifiedName state qualifier name -> do
                    let fqname = fromMaybe (error $ "Couldn't find name " ++ show nm) $ findQualifiedName state qualifier name
                    return $ Ident meta fqname
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
