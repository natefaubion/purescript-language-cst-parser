let conf = ../spago.dhall

in conf // {
  sources = conf.sources # [ "parse-package-set/**/*.purs" ],
  dependencies = conf.dependencies # [ "node-fs", "node-fs-aff", "strings", "node-child-process", "console", "avar", "now", "numbers" ]
}
