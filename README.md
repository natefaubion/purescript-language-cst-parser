# PureScript Language CST Parser

A parser for the PureScript concrete syntax tree.

## Usage

The supported parsers are exported from `PureScript.CST`. The parsers support
some error recovery, which is reflected in the `RecoveredParserResult` type.
The CST types in `PureScript.CST.Types` are indexed by an error type, which
is fixed to `RecoveredError` in the case of failures. Parses that succeed
without failures have the error type fixed to `Void`.

```purescript
import PureScript.CST (RecoveredParserResult(..), parseModule)

example = case parseModule myModuleSource of
  ParseSucceeded cst ->
    -- `cst` is type `Module Void` to indicate no errors
  ParseSucceededWithErrors cst errors ->
    -- `cst is type `Module RecoveredError` and contains error nodes at points of failure.
  ParseFailed error ->
    -- An unrecoverable error was encountered.
```

## Traversals

`PureScript.CST.Traversal` contains traversals for analyzing and rewriting
the CST. These folds take a language visitor record with functions for
handling the primary types in the CST. Default records are provided that do
nothing for the cases you don't care about.

For example, if one wanted to quickly gather all the identifiers used in the
expressions of a module, one might use `foldMapModule` and only provide a
case for `onExpr`.

```purescript
import Data.Set (Set, singleton)
import PureScript.CST.Traversal (foldMapModule, defaultMonoidalVisitor)
import PureScript.CST.Types (Expr(..), Ident, Module)

getExprIdents :: forall a. Module a -> Set Ident
getExprIdents = foldMapModule $ defaultMonoidalVisitor
  { onExpr = case _ of
      ExprIdent ident -> singleton ident
      _ -> mempty
  }
```

## Development

The provided integration test attempts to parse a provided package set, and
will report any errors it encounters as well as listing the fastest and
slowest parse times along with the mean parse time for the set.

```sh
npm run parse-package-set
```
