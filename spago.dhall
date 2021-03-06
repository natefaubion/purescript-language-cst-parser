{ name = "language-cst-parser"
, dependencies =
  [ "arrays"
  , "console"
  , "const"
  , "debug"
  , "effect"
  , "either"
  , "filterable"
  , "foldable-traversable"
  , "free"
  , "functors"
  , "maybe"
  , "numbers"
  , "psci-support"
  , "safe-coerce"
  , "strings"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
