{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Lasca.Modules where

import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Printf

import           Control.Monad.State
import           Control.Lens hiding ((<.>))

import           Data.List
import           Data.IntMap.Strict             ( IntMap )
import qualified Data.IntMap.Strict            as IntMap
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map

import System.Environment
import System.Exit
import System.Directory
import System.FilePath

import Debug.Trace as Debug
import qualified Text.Megaparsec as Megaparsec

import Lasca.Syntax
import Lasca.Parser
import Lasca.Type

data LascaModule = LascaModule { 
    imports :: [LascaModule], 
    moduleExprs :: [Expr],
    modName :: Name 
}

instance Show LascaModule where
    show m = show (modName m)

instance Eq LascaModule where
    lhs == rhs = modName lhs == modName rhs

instance Ord LascaModule where
    compare lhs rhs = compare (modName lhs) (modName rhs)

data Dependencies = Dependencies {
    _modsByLevel :: IntMap [LascaModule],
    _modLevel :: Map LascaModule Int
} deriving (Show)
makeLenses 'Dependencies

calcModulesDependencies :: LascaModule -> Dependencies
calcModulesDependencies lascaModule = execState
    (getMaxLevel lascaModule)
    (Dependencies {_modsByLevel = IntMap.empty, _modLevel = Map.empty})
  where
    getMaxLevel :: LascaModule -> State Dependencies Int
    getMaxLevel m = do
        s <- get
        case Map.lookup m (s ^. modLevel) of
            Just l  -> return l
            Nothing -> do
                let mods = imports m
                levels <- forM mods getMaxLevel
                let level = case levels of
                        []     -> 0
                        levels -> 1 + maximum levels
                modLevel %= Map.insert m level
                modsByLevel %= IntMap.alter (joinModules m) level
                return level
      where
        joinModules new Nothing     = Just [new]
        joinModules new (Just mods) = Just $ new : mods

linearizeIncludes :: LascaModule -> [LascaModule]
linearizeIncludes lascaModule = do
    let all    = calcModulesDependencies lascaModule
    let pathes = snd <$> IntMap.toList (all ^. modsByLevel)
    foldr (\mods path -> sort mods ++ path) [] pathes

fixModuleAndImportPrelude :: FilePath -> [Expr] -> IO [Expr]
fixModuleAndImportPrelude filename exprs = case exprs of
    (mod@(Module _ name): exprs) -> do
        when (takeBaseName filename /= T.unpack (last $ nameToList name)) $
            die $ printf "Wrong module name in file %s. Module name should match file name, but was %s)" filename (show name)
        return $ mod : insertImportPrelude name exprs
    _ -> do
        let name = Name $ T.pack $ takeBaseName filename
        let mod = Module emptyMeta name
        return $ mod : insertImportPrelude name exprs

insertImportPrelude :: Name -> [Expr] -> [Expr]
insertImportPrelude name exprs = if name == Name "Prelude" then exprs else Import emptyMeta "Prelude" : exprs

moduleSearchPaths :: IO [FilePath]
moduleSearchPaths = do
    dir <- getCurrentDirectory
    lascaPathEnv <- lookupEnv "LASCAPATH"
    let lascaPaths = splitSearchPath $ fromMaybe "" lascaPathEnv
    absPaths <- mapM canonicalizePath lascaPaths
    existingPaths <- filterM doesDirectoryExist absPaths
    -- TODO add XDB paths
    return $ nub $ dir : existingPaths

findModulePath :: [FilePath] -> Name -> IO FilePath
findModulePath searchPaths name = do
    let relPath = path name <.> "lasca"
    result <- findFile searchPaths relPath
    case result of
        Just file -> return file
        Nothing -> error $ printf "Couldn't find module %s. Search path: %s" (show name) (show $ intercalate "," searchPaths)
    where
    path (Name n) = T.unpack n
    path (NS prefix n) = path prefix </> path n

type Mapping = Map Name LascaModule

loadModule :: [FilePath] -> Mapping -> [Name] -> FilePath -> Name -> IO (Mapping, LascaModule)
loadModule searchPaths imported importPath absoluteFilePath name = do
    file <- TIO.readFile absoluteFilePath
    case parseToplevelFilename absoluteFilePath file of
        Left err -> die $ Megaparsec.parseErrorPretty err
        Right exprs -> do
            canonizedExprs <- fixModuleAndImportPrelude absoluteFilePath exprs
            let imports = getImports canonizedExprs
            (newImported, modules) <- loadImports searchPaths imported (name : importPath) imports
            let thisModule = LascaModule { modName = name, imports = modules, moduleExprs = canonizedExprs }
            return (Map.insert name thisModule newImported, thisModule)

loadImports :: [FilePath] -> Mapping -> [Name] -> [Name] -> IO (Mapping, [LascaModule])
loadImports searchPaths imported importPath imports = do
--    Debug.traceM $ printf "loadImports %s %s %s" (show imported) (show importPath) (show $ imports)
    foldM (\(imported, modules) name -> do
        (newImported, lascaModule) <- loadImport searchPaths imported importPath name
        return (Map.union imported newImported, lascaModule : modules)
        ) (imported, []) imports

loadImport :: [FilePath] -> Mapping -> [Name] -> Name -> IO (Mapping, LascaModule)
loadImport searchPaths imported importPath name = do
--    Debug.traceM $ printf "loadImport %s %s %s" (show imported) (show importPath) (show name)
    when (name `elem` importPath) $ die (printf "Circular dependency in %s -> %s" (show importPath) (show name))
    case name `Map.lookup` imported of
        Just lascaModule -> return (imported, lascaModule)
        Nothing -> do
            absoluteFilePath <- findModulePath searchPaths name
            loadModule searchPaths imported importPath absoluteFilePath name

getImports :: [Expr] -> [Name]
getImports exprs = foldl' folder [] exprs
    where
        folder imports (Import _ name) = name : imports
        folder imports _ = imports
