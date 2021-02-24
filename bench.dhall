let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "bench/**/*.purs" ],
  dependencies = conf.dependencies # [ "node-fs", "node-fs-aff", "strings", "node-child-process", "node-process", "minibench", "console", "avar", "now", "numbers" ]
}
