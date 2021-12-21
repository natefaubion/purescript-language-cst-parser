{ name = "language-cst-parser"
, dependencies =
  [ "arrays"
  , "const"
  , "control"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "free"
  , "functors"
  , "identity"
  , "integers"
  , "lazy"
  , "lists"
  , "maybe"
  , "newtype"
  , "numbers"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  , "st"
  , "strings"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "unfoldable"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
