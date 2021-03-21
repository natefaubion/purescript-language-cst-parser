module PureScript.CST.Utils.ModuleGraph
  ( moduleGraph
  , sortModules
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (all, foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import PureScript.CST.Types (ImportDecl(..), Module(..), ModuleHeader(..), ModuleName, Name(..))

type Graph a = Map a (Set a)

moduleGraph :: forall e. Array (Module e) -> Graph ModuleName
moduleGraph = Map.fromFoldable <<< map go
  where
  go (Module { header: ModuleHeader { name: Name { name }, imports } }) =
    Tuple name (Set.fromFoldable (map getImportName imports))

  getImportName (ImportDecl { "module": Name { name } }) = name

sortModules :: forall e. Array (Module e) -> Maybe (Array (Module e))
sortModules modules = do
  let
    moduleNames :: Map ModuleName (Module e)
    moduleNames =
      modules
        # map (\mod@(Module { header: (ModuleHeader { name: Name { name } }) }) -> Tuple name mod)
        # Map.fromFoldable

    graph = moduleGraph modules

  Array.mapMaybe (flip Map.lookup moduleNames) <$> topoSort graph

topoSort :: forall a. Ord a => Graph a -> Maybe (Array a)
topoSort graph = do
  let mbResults = go (Just { roots: startingModules, sorted: [], usages: importCounts })
  map (Array.reverse <<< _.sorted) mbResults
  where
  go
    :: Maybe { roots :: Set a, sorted :: Array a, usages :: Map a Int }
    -> Maybe { roots :: Set a, sorted :: Array a, usages :: Map a Int }
  go = case _ of
    Nothing -> Nothing
    Just { roots, sorted, usages } -> case Set.findMin roots of
      Nothing ->
        if all (eq 0) usages then
          Just { roots, sorted, usages }
        else
          Nothing
      Just curr -> do
        let
          sorted' = Array.snoc sorted curr

          reachable = fromMaybe Set.empty (Map.lookup curr graph)

          usages' = foldl decrementImport usages reachable

          roots' = foldl (appendRoots usages') (Set.delete curr roots) reachable

        go (Just { roots: roots', sorted: sorted', usages: usages' })

  appendRoots :: Map a Int -> Set a -> a -> Set a
  appendRoots usages roots curr = maybe roots (flip Set.insert roots) do
    count <- Map.lookup curr usages
    isRoot (Tuple curr count)

  decrementImport :: Map a Int -> a -> Map a Int
  decrementImport usages k = Map.insertWith add k (-1) usages

  startingModules :: Set a
  startingModules =
    importCounts
      # Map.toUnfoldable
      # Array.mapMaybe isRoot
      # Set.fromFoldable

  importCounts :: Map a Int
  importCounts = Map.fromFoldableWith add do
    Tuple a bs <- Map.toUnfoldable graph
    [ Tuple a 0 ] <> map (flip Tuple 1) (Set.toUnfoldable bs)

  isRoot :: Tuple a Int -> Maybe a
  isRoot (Tuple a count) = if count == 0 then Just a else Nothing
