module PureScript.CST.Utils.ModuleGraph where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import PureScript.CST.Types (ImportDecl(..), Module(..), ModuleHeader(..), ModuleName, Name(..))

moduleGraph :: forall e. Array (Module e) -> Map ModuleName (Set ModuleName)
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
