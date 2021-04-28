let conf = ../spago.dhall

in conf // {
  sources = conf.sources # [ "bench/**/*.purs" ],
  dependencies = conf.dependencies # [ "aff", "node-fs-aff", "strings", "node-process", "minibench", "console", "numbers", "node-buffer" ]
}
