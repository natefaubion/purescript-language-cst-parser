{ name = "language-cst-parser"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "either"
  , "filterable"
  , "foldable-traversable"
  , "free"
  , "maybe"
  , "parsing"
  , "psci-support"
  , "string-parsers"
  , "strings"
  , "tuples"
  , "unicode"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
