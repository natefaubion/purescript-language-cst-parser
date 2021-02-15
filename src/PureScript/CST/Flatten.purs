module PureScript.CST.Flatten where

import Prelude

import Data.Array as Array
import Data.Foldable (fold, foldMap)
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..))
import PureScript.CST.Types (DataMembers(..), Delimited, DelimitedNonEmpty, Export(..), Module(..), ModuleBody(..), ModuleHeader(..), Name(..), Separated(..), SourceToken, Wrapped(..), ImportDecl(..), Import(..))

module_ :: forall e. Module e -> Array SourceToken
module_ (Module { header, body }) =
  moduleHeader header <> moduleBody body

moduleHeader :: forall e. ModuleHeader e -> Array SourceToken
moduleHeader (ModuleHeader h) =
  [h.keyword] <>
  name h.name <>
  foldMap (delimitedNonEmpty export) h.exports <>
  [h.where] <>
  foldMap importDecl h.imports

moduleBody :: forall e. ModuleBody e -> Array SourceToken
moduleBody (ModuleBody h) = []

export :: forall e. Export e -> Array SourceToken
export = case _ of
  ExportValue x -> name x
  ExportOp x -> name x
  ExportType x y -> name x <> foldMap dataMembers y
  ExportTypeOp t x -> [t] <> name x
  ExportClass t x -> [t] <> name x
  ExportKind t x -> [t] <> name x
  ExportModule t x -> [t] <> name x
  ExportError e -> [] -- TODO: I think we should be able to print lossless in the case of an error?

importDecl :: forall e. ImportDecl e -> Array SourceToken
importDecl (ImportDecl id) =
  [id.keyword] <>
  name id.module <>
  foldMap (\(Tuple x y) -> foldMap Array.singleton x <> delimitedNonEmpty import_ y) id.names <>
  foldMap (\(Tuple x y) -> [x] <> name y) id.qualified

import_ :: forall e. Import e -> Array SourceToken
import_ = case _ of
  ImportValue x -> name x
  ImportOp x -> name x
  ImportType x y -> name x <> foldMap dataMembers y
  ImportTypeOp x y -> [x] <> name y
  ImportClass x y -> [x] <> name y
  ImportKind x y -> [x] <> name y
  ImportError _ -> []

dataMembers :: DataMembers -> Array SourceToken
dataMembers = case _ of
  DataAll x -> [x]
  DataEnumerated xs -> delimited name xs

name :: forall a. Name a -> Array SourceToken
name (Name { token }) = [token]

delimited :: forall a. (a -> Array SourceToken) -> Delimited a -> Array SourceToken
delimited f (Wrapped { open, value, close }) =
   [open] <> maybe [] (separated f) value <> [close]

delimitedNonEmpty :: forall a. (a -> Array SourceToken) -> DelimitedNonEmpty a -> Array SourceToken
delimitedNonEmpty f (Wrapped { open, value, close }) = [open] <> separated f value <> [close]

separated :: forall a. (a -> Array SourceToken) -> Separated a -> Array SourceToken
separated f (Separated { head, tail }) =
  f head <> foldMap (\(Tuple t a) -> [t] <> f a) tail
