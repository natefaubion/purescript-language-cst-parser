module PureScript.CST.Utils.ModuleGraph
  ( moduleGraph
  , sortModules
  ) where

import Prelude

import Control.Monad.Free (Free, runFree)
import Data.Array as Array
import Data.Foldable (all, foldl)
import Data.Identity (Identity(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (un)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import PureScript.CST.Types (ImportDecl(..), Module(..), ModuleHeader(..), ModuleName, Name(..))

type Graph a = Map a (Set a)

moduleGraph :: forall e. Array (Module e) -> Graph ModuleName
moduleGraph = Map.fromFoldable <<< map go
  where
  go (Module { header: ModuleHeader { name: Name { name }, imports } }) =
    Tuple name (Set.fromFoldable (map getImportName imports))

  getImportName (ImportDecl { module: Name { name } }) = name

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
  let mbResults = runFree (un Identity) (go (Just { roots: startingModules, sorted: [], usages: importCounts }))
  map (Array.reverse <<< _.sorted) mbResults
  where
  go
    :: Maybe { roots :: Array a, sorted :: Array a, usages :: Map a Int }
    -> Free Identity (Maybe { roots :: Array a, sorted :: Array a, usages :: Map a Int })
  go Nothing = pure Nothing
  go (Just { roots, sorted, usages }) =
    case Array.uncons roots of
      Nothing ->
        if all (eq 0) usages then
          pure (Just { roots, sorted, usages })
        else
          pure Nothing
      Just { head, tail } -> do
        let
          sorted' = Array.snoc sorted head

          reachable = maybe [] Set.toUnfoldable (Map.lookup head graph)

          usages' = foldl decrementImport usages reachable

          roots' = append tail $ fromMaybe [] do
            newUsages <- for reachable (\r -> Map.lookup r usages' >>= (Tuple r >>> pure))
            pure $ Array.mapMaybe (\(Tuple a count) -> if count == 0 then Just a else Nothing) newUsages

        go (Just { roots: roots', sorted: sorted', usages: usages' })

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
