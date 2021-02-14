{ name = "language-cst-parser"
, dependencies =
  [ "arrays"
  , "console"
  , "debug"
  , "effect"
  , "either"
  , "filterable"
  , "foldable-traversable"
  , "maybe"
  , "numbers"
  , "psci-support"
  , "strings"
  , "tuples"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
