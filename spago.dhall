{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
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
