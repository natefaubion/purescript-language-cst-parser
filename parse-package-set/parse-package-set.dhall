let conf = ../spago.dhall

in conf // {
  sources = conf.sources # [ "parse-package-set/**/*.purs" ],
  dependencies = conf.dependencies # [ "node-fs", "node-fs-aff", "strings", "node-child-process", "console", "avar", "numbers", "aff", "datetime", "exceptions", "node-buffer", "node-path", "parallel", "filterable" ]
}
