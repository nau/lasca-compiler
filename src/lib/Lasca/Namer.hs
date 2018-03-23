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
    _currentModule :: Name,
    _importedModules :: Set Name,
    _moduleQualifiers :: Map Name Name,  -- ^ MultiSet -> Data.MultiSet mapping
    _exportedNames :: Map Name (Set Name),
    _exportedTypes :: Map Name (Set Name),
    _locals :: MSet.MultiSet Name,
    _callStack :: [Name],
    _context :: Ctx
} deriving (Show)
makeLenses ''NamerPhaseState

emptyNamerPhaseState opts = NamerPhaseState {
    _currentModule = Name "default", -- TODO check it
    _importedModules = Set.empty,
    _moduleQualifiers = Map.empty,
    _exportedNames = Map.empty,
    _exportedTypes = Map.empty,
    _locals = MSet.empty,
    _callStack = [],
    _context = emptyCtx opts
}

importedNames state = Map.restrictKeys (state^.exportedNames) (Set.insert (state^.currentModule) (state^.importedModules))

findAllModules state name = Map.foldrWithKey folder [] (state^.exportedNames)
  where
    folder mod names res = if Set.member name names then mod : res else res

resolveName name meta = do
    s <- get
    if Map.member name builtinFunctions || MSet.member name (s^.locals)
    then return name
    else return $ resolveNonLocalName name meta s

resolveNonLocalName name meta s = do
    case name of
        Name n -> case findAllModules s name of
            [mod] -> (NS mod name)
            [] -> error $ printf "%s: Couldn't find name %s. Exported Names %s" (showPosition meta) (show n) (show (s^.exportedNames))
            mods -> error $ printf "%s: Ambiguous name %s found in modules %s" (showPosition meta) (show n) (show mods)
        NS mod n ->
            case Map.lookup mod (s^.exportedNames) of
                -- correctly qualified name
                Just names | Set.member n names -> name
                -- incorrectly qualified name, TODO add levenshtein distance based suggestions
                Just names -> error $ printf "%s: Couldn't find name %s in module %s." (showPosition meta) (show n) (show mod)
                Nothing -> error $ printf "%s: No such module imported: %s at %s.\nAdd import %s" (showPosition meta) (show mod) (show meta) (show mod)


processData expr@(Data meta name tvars consts) = do
    state <- get
    let newCtx = execState (processConstructors consts) (state^.context)
    context .= newCtx
    context.dataDefs %= (:) expr
    context.dataDefsNames %= Set.insert name
  where
    processConstructors constrs = forM constrs processConstructor

    processConstructor (DataConst constrName args) = do
        processConstructorArguments constrName args
        if null args then globalVals %= Map.insert constrName expr
        else do let dataTypeIdent = TypeIdent name
                    tpe = (foldr (\(Arg _ tpe) acc -> tpe `TypeFunc` acc) dataTypeIdent args) -- FIXME forall
                    m = meta `withType` tpe
                    funDef = Function meta constrName dataTypeIdent args EmptyExpr
                globalFunctions %= Map.insert constrName funDef
        return ()

    processConstructorArguments constrName args = do
        let mapping = List.foldl' (\fields (arg@(Arg n t), idx)  ->
                    if n `Map.member` fields
                    then error $ printf "Field %s already defined in data %s" (show n) (show name)
                    else Map.insert n (arg, idx) fields -- field name -> (S.Arg, field index in constructor) mapping
                ) Map.empty (zip args [0..])
        constructorArgs %= Map.insert constrName args
        dataDefsFields %= Map.insertWith Map.union name mapping
        return ()
processData expr = error $ printf "%s: This should not happen: %s" (show $ exprPosition expr) (show expr)

collectNames exprs = forM_ exprs $ \expr -> case expr of
    Let _ name _ EmptyExpr -> do
        curMod <- gets _currentModule
        exportedNames %= Map.adjust (Set.insert name) curMod -- add this val name as current module exported name
        return ()
    Function _ name _ _ _ -> do
        curMod <- gets _currentModule
        exportedNames %= Map.adjust (Set.insert name) curMod -- add this function name as current module exported name
        return ()
    dat@(Data _ typeName tvars constrs) -> do
        curMod <- gets _currentModule
        let constrNames = Set.fromList $ constrs >>= (\(DataConst name args) -> name : (fmap (\(Arg n t) -> n) args))
        exportedNames %= Map.adjust (Set.union constrNames) curMod
        return ()
    Module _ name -> do
        currentModule .= name
        -- for now, support only unique module names per file
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

isFullyQualifiedName state (NS modName name) =
    case Map.lookup modName (state^.exportedNames) of
        -- correctly qualified name
        Just names | Set.member name names -> True
        _ -> False
isFullyQualifiedName _ _ = False

isQualifiedName state qualifier name = do
    let fqpn = Map.lookup qualifier (state^.moduleQualifiers)
    any (\fqpn -> isFullyQualifiedName state (NS fqpn name)) fqpn

findQualifiedName state qualifier name = do
    fqpn <- Map.lookup qualifier (state^.moduleQualifiers)
    names <- Map.lookup fqpn (state^.exportedNames)
    if Set.member name names then return $ NS fqpn name else Nothing


namerTransform :: Expr -> State NamerPhaseState Expr
namerTransform expr = do
--    Debug.traceM $ printf "Current expr %s" (show expr)
    case expr of
        Module _ name -> do
            currentModule .= name
            importedModules .= Set.empty
            moduleQualifiers .= Map.empty
            locals .= MSet.empty
--            Debug.traceM $ printf "Current module %s" (show name)
            return expr
        Import _ name -> do
            importedModules %= Set.insert name
            case name of
                NS prefix qual -> moduleQualifiers %= Map.insert qual name
                _ -> return ()
--            Debug.traceM $ printf "importedModules %s" (show name)
            return expr
        Data meta name tvars constrs -> do
            curMod <- gets _currentModule
            let renamedArgs args = fmap (\(Arg name t) -> Arg (NS curMod name) t) args
            let renamed = map (\(DataConst name args) -> DataConst (NS curMod name) (renamedArgs args)) constrs
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
            curMod <- gets _currentModule
            let fqn = NS curMod name
            context.globalVals %= Map.insert fqn expr
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
--            Debug.traceM $ printf "Current Function %s" (show name)
            let argNames = MSet.fromList (map (\(Arg n t) -> n) args)
            outerStack <- gets _callStack
            callStack %= (:) name
            case outerStack of
                [] -> do  locals .= argNames
                          e' <- go e1
                          callStack %= tail
                          curMod <- gets _currentModule
                          let fqn = NS curMod name
                          context.globalFunctions %= Map.insert fqn expr
                          locals .= MSet.empty
                          return (Function meta fqn tpe args e')
                _  -> do  oldLocals <- gets _locals
                          let names = MSet.insert name argNames
                          locals %= MSet.union names
                          e' <- go e1
                          callStack %= tail
                          locals .= MSet.insert name oldLocals
                          return (Function meta name tpe args e')
        e -> return e
        where go e = namerTransform e

namerPhase :: LascaOpts -> [Expr] -> ([Expr], NamerPhaseState)
namerPhase opts exprs = do
    runState namer (emptyNamerPhaseState opts)
  where
    namer = do
      collectNames exprs
      forM exprs namerTransform
