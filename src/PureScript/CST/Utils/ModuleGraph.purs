module PureScript.CST.Utils.ModuleGraph
  ( moduleGraph
  , sortModules
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import PureScript.CST.Types (ImportDecl(..), Module(..), ModuleHeader(..), ModuleName, Name(..))

type Graph a = Map a (Set a)

moduleGraph :: forall e. Array (Module e) -> Graph ModuleName
moduleGraph = map go <<< moduleHeaders
  where
  go (ModuleHeader { imports }) =
    imports
      # map (\(ImportDecl { module: Name { name } }) -> name)
      # Set.fromFoldable

moduleHeaders :: forall e. Array (Module e) -> Map ModuleName (ModuleHeader e)
moduleHeaders mods =
  mods
    # map (\(Module { header: header@(ModuleHeader { name: Name { name } }) }) -> Tuple name header)
    # Map.fromFoldable

sortModules :: forall e. Array (Module e) -> Array (Module e)
sortModules modules = do
  let
    moduleNames :: Map ModuleName (Module e)
    moduleNames =
      modules
        # map (\mod@(Module { header: (ModuleHeader { name: Name { name } }) }) -> Tuple name mod)
        # Map.fromFoldable

    graph = moduleGraph modules

    sorted = topoSort graph

  Array.mapMaybe (flip Map.lookup moduleNames) sorted

topoSort :: forall a. Show a => Ord a => Graph a -> Array a
topoSort graph = do
  let { sorted } = go { roots: startingModules, sorted: [], usages: importCounts }
  sorted
  where
  go
    :: { roots :: Array a, sorted :: Array a, usages :: Map a Int }
    -> { roots :: Array a, sorted :: Array a, usages :: Map a Int }
  go { roots, sorted, usages } =
    case Array.uncons roots of
      Nothing -> { roots, sorted, usages }
      Just { head, tail } -> do
        let
          sorted' = Array.snoc sorted head

          reachable = maybe [] Set.toUnfoldable (Map.lookup head graph)

          usages' = foldl decrementImport usages reachable

          roots' = append tail $ fromMaybe [] do
            newUsages <- for reachable (\r -> Map.lookup r usages' >>= (Tuple r >>> pure))
            pure $ newUsages # Array.mapMaybe (\(Tuple a count) -> if count == 0 then Just a else Nothing)

        go { roots: roots', sorted: sorted', usages: usages' }

  decrementImport :: Map a Int -> a -> Map a Int
  decrementImport usages k = Map.insertWith add k (-1) usages

  startingModules :: Array a
  startingModules =
    importCounts
      # Map.toUnfoldable
      # Array.mapMaybe \(Tuple a count) -> if count == 0 then Just a else Nothing

  importCounts :: Map a Int
  importCounts = Map.fromFoldableWith add do
    Tuple a bs <- Map.toUnfoldable graph
    [ Tuple a 0 ] <> map (flip Tuple 1) (Set.toUnfoldable bs)
