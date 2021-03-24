module PureScript.CST.ModuleGraph
  ( moduleGraph
  , sortModules
  ) where

import Prelude

import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Foldable (all, foldl)
import Data.Functor.Compose (Compose(..))
import Data.Identity (Identity(..))
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (un)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Control.Monad.Free (Free, runFree)
import PureScript.CST.Types (ImportDecl(..), ModuleHeader(..), ModuleName, Name(..))

type Graph a = Map a (Set a)

moduleGraph :: forall e. Array (ModuleHeader e) -> Graph ModuleName
moduleGraph = Map.fromFoldable <<< map go
  where
  go (ModuleHeader { name: Name { name }, imports }) =
    Tuple name (Set.fromFoldable (map getImportName imports))

  getImportName (ImportDecl { "module": Name { name } }) = name

sortModules :: forall e. Array (ModuleHeader e) -> Either (Array (ModuleHeader e)) (Array (ModuleHeader e))
sortModules moduleHeaders = do
  let
    knownModuleHeaders :: Map ModuleName (ModuleHeader e)
    knownModuleHeaders =
      moduleHeaders
        # map (\header@(ModuleHeader { name: Name { name } }) -> Tuple name header)
        # Map.fromFoldable

    graph = moduleGraph moduleHeaders

  bimap (Array.mapMaybe (flip Map.lookup knownModuleHeaders) <<< List.toUnfoldable) (Array.mapMaybe (flip Map.lookup knownModuleHeaders) <<< List.toUnfoldable) (topoSort graph)

type TopoSortArgs a =
  { roots :: Set a
  , sorted :: List a
  , usages :: Map a Int
  }

topoSort :: forall a. Ord a => Graph a -> Either (List a) (List a)
topoSort graph = do
  _.sorted <$> go { roots: startingModules, sorted: Nil, usages: importCounts }
  where
  go
    :: TopoSortArgs a
    -> Either (List a) (TopoSortArgs a)
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

        case foldl (\b a -> if isJust b then b else runFree (un Identity) (un Compose (depthFirst { path: Nil, visited: Set.empty, curr: a }))) Nothing nonLeaf of
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

  depthFirst :: { path :: List a, visited :: Set a, curr :: a } -> Compose (Free Identity) Maybe (List a)
  depthFirst { path, visited, curr } =
    if Set.member curr visited then
      pure (Cons curr path)
    else if Map.lookup curr graph == Just Set.empty || Map.lookup curr graph == Nothing then
      Compose $ pure Nothing
    else Compose $ pure $ do
      reachable <- Map.lookup curr graph
      foldl (\b a -> if isJust b then b else runFree (un Identity) (un Compose (depthFirst { path: Cons curr path, visited: Set.insert curr visited, curr: a }))) Nothing reachable
