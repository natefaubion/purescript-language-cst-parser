module PureScript.CST.Utils.ModuleGraph where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Set as Set
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
  let { l } = go { l: [], s: roots, g: graph, usages: usageCounts }
  l
  where
  go
    :: { l :: Array a, s :: Array a, g :: Graph a, usages :: Map a Int }
    -> { l :: Array a, s :: Array a, g :: Graph a, usages :: Map a Int }
  go { l, s, g, usages } =
    case Array.uncons s of
      Nothing -> { l, s, g, usages }
      Just { head, tail } -> do
        let
          l' = Array.snoc l head

          ms = maybe [] Set.toUnfoldable (Map.lookup head g)

          usages' = foldl decrementUsage usages ms

          roots' =
            usages'
              # Map.toUnfoldable
              # Array.mapMaybe \(Tuple a count) -> if count == 0 then Just a else Nothing

        go { l: l', s: tail <> roots', g: Map.delete head g, usages: usages' }

  decrementUsage :: Map a Int -> a -> Map a Int
  decrementUsage usages k = Map.insertWith add k (-1) usages

  roots :: Array a
  roots =
    usageCounts
      # Map.toUnfoldable
      # Array.mapMaybe \(Tuple a count) -> if count == 0 then Just a else Nothing

  usageCounts :: Map a Int
  usageCounts = Map.fromFoldable do
    Tuple a bs <- Map.toUnfoldable graph
    [ Tuple a 0 ] <> map (flip Tuple 1) (Set.toUnfoldable bs)
