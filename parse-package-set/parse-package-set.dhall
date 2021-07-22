let conf = ../spago.dhall

in conf // {
  sources = conf.sources # [ "parse-package-set/**/*.purs" ],
  dependencies = conf.dependencies #
    [ "aff"
    , "avar"
    , "console"
    , "datetime"
    , "exceptions"
    , "filterable"
    , "node-buffer"
    , "node-child-process"
    , "node-fs"
    , "node-fs-aff"
    , "node-path"
    , "numbers"
    , "parallel"
    , "strings"
    ]
}
