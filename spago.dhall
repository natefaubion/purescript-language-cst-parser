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
  , "numbers"
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
