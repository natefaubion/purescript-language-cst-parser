let conf = ../spago.dhall

in conf // {
  sources = conf.sources # [ "bench/**/*.purs" ],
  dependencies = conf.dependencies #
    [ "aff"
    , "console"
    , "minibench"
    , "node-buffer"
    , "node-fs-aff"
    , "node-process"
    , "numbers"
    , "strings"
    ]
}
