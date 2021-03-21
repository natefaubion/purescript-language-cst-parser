module PureScript.CST.ModuleGraph
  ( moduleGraph
  , sortModules
  ) where

import Prelude

import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Foldable (all, foldl)
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Control.Alt (alt)
import PureScript.CST.Types (ImportDecl(..), Module(..), ModuleHeader(..), ModuleName, Name(..))

type Graph a = Map a (Set a)

moduleGraph :: forall e. Array (Module e) -> Graph ModuleName
moduleGraph = Map.fromFoldable <<< map go
  where
  go (Module { header: ModuleHeader { name: Name { name }, imports } }) =
    Tuple name (Set.fromFoldable (map getImportName imports))

  getImportName (ImportDecl { "module": Name { name } }) = name

sortModules :: forall e. Array (Module e) -> Either (Array (Module e)) (Array (Module e))
sortModules modules = do
  let
    moduleNames :: Map ModuleName (Module e)
    moduleNames =
      modules
        # map (\mod@(Module { header: (ModuleHeader { name: Name { name } }) }) -> Tuple name mod)
        # Map.fromFoldable

    graph = moduleGraph modules

  bimap (Array.mapMaybe (flip Map.lookup moduleNames) <<< List.toUnfoldable) (Array.mapMaybe (flip Map.lookup moduleNames) <<< List.toUnfoldable) (topoSort graph)

topoSort :: forall a. Ord a => Graph a -> Either (List a) (List a)
topoSort graph = do
  let mbResults = go { roots: startingModules, sorted: Nil, usages: importCounts }
  map _.sorted mbResults
  where
  go
    :: { roots :: Set a, sorted :: List a, usages :: Map a Int }
    -> Either (List a) { roots :: Set a, sorted :: List a, usages :: Map a Int }
  go { roots, sorted, usages } = case Set.findMin roots of
    Nothing ->
      if all (eq 0) usages then
        Right { roots, sorted, usages }
      else do
        let
          nonLeaf = Set.fromFoldable do
            Tuple a count <- Map.toUnfoldable usages :: Array (Tuple a Int)
            if count /= 0 && Map.lookup a graph /= Nothing && Map.lookup a graph /= Just (Set.empty) then
              [ a ]
            else
              []

        case foldl (\b a -> alt b (depthFirst { path: Nil, visited: Set.empty, curr: a })) Nothing nonLeaf of
          Just cycle -> Left cycle
          Nothing -> Left Nil

    Just curr -> do
      let
        sorted' = Cons curr sorted

        reachable = fromMaybe Set.empty (Map.lookup curr graph)

        usages' = foldl decrementImport usages reachable

        roots' = foldl (appendRoots usages') (Set.delete curr roots) reachable

      go { roots: roots', sorted: sorted', usages: usages' }

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

  depthFirst :: { path :: List a, visited :: Set a, curr :: a } -> Maybe (List a)
  depthFirst { path, visited, curr } =
    if Set.member curr visited then
      Just (Cons curr path)
    else if Map.lookup curr graph == Just Set.empty || Map.lookup curr graph == Nothing then
      Nothing
    else do
      reachable <- Map.lookup curr graph
      foldl (\b a -> alt b (depthFirst { path: Cons curr path, visited: Set.insert curr visited, curr: a })) Nothing reachable
