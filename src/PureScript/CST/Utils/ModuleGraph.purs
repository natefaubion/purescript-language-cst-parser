module PureScript.CST.Utils.ModuleGraph where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst)
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
  let { l } = go { l: [], s: roots, g: reversed, usages: usageCounts, visited: Set.empty }
  l
  where
  go
    :: { l :: Array a, s :: Array a, g :: Graph a, usages :: Map a Int, visited :: Set a }
    -> { l :: Array a, s :: Array a, g :: Graph a, usages :: Map a Int, visited :: Set a }
  go { l, s, g, usages, visited } =
    case Array.uncons s of
      Nothing -> { l, s, g, usages, visited }
      Just { head, tail } -> do
        let
          l' = Array.snoc l head

          visited' = Set.insert head visited

          ms = Array.mapMaybe (\b -> (Map.lookup b g >>= ((Set.toUnfoldable :: Set a -> Array a) >>> Array.find (eq head))) $> b) all

          usages' = foldl decrementUsage usages ms

          roots' =
            usages'
              # Map.toUnfoldable
              # Array.mapMaybe \(Tuple a count) -> if count == 0 && Array.any (eq a) ms then Just a else Nothing

          g' = foldl (updateGraph head) g ms

          s' =
            tail
              # flip append roots'
              # Set.fromFoldable
              # flip Set.difference visited'
              # Set.toUnfoldable

        go { l: l', s: s', g: g', usages: usages', visited: visited' }

  updateGraph :: a -> Graph a -> a -> Graph a
  updateGraph s g a = Map.update (Set.delete a >>> pure) s g

  all :: Array a
  all =
    graph
      # Map.toUnfoldable
      # map fst

  reversed :: Graph a
  reversed = Map.fromFoldableWith Set.union do
    Tuple a bs <- Map.toUnfoldable graph
    b <- Set.toUnfoldable bs
    [ Tuple b (Set.singleton a) ]

  decrementUsage :: Map a Int -> a -> Map a Int
  decrementUsage usages k = Map.insertWith add k (-1) usages

  roots :: Array a
  roots =
    usageCounts
      # Map.toUnfoldable
      # Array.mapMaybe \(Tuple a count) -> if count == 0 then Just a else Nothing

  usageCounts :: Map a Int
  usageCounts = Map.fromFoldableWith add do
    Tuple a bs <- Map.toUnfoldable graph
    [ Tuple a 0 ] <> map (flip Tuple 1) (Set.toUnfoldable bs)
