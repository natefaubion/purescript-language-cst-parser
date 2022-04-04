{ name = "language-cst-parser"
, dependencies =
  [ "arrays"
  , "console"
  , "const"
  , "control"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "free"
  , "functions"
  , "functors"
  , "identity"
  , "integers"
  , "lazy"
  , "lists"
  , "maybe"
  , "newtype"
  , "node-process"
  , "numbers"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "st"
  , "strings"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "unfoldable"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/Main.purs" ]
}
