let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.7-20230401/packages.dhall
        sha256:d385eeee6ca160c32d7389a1f4f4ee6a05aff95e81373cdc50670b436efa1060

in  upstream
      with node-event-emitter.version = "v3.0.0"
      with node-event-emitter.dependencies =
        [ "effect"
        , "either"
        , "functions"
        , "maybe"
        , "nullable"
        , "prelude"
        , "unsafe-coerce"
        ]
      with node-buffer.version = "v9.0.0"
      with node-buffer.dependencies =
        [ "arraybuffer-types"
        , "effect"
        , "maybe"
        , "st"
        , "unsafe-coerce"
        , "nullable"
        ]
      with node-streams.version = "v9.0.0"
      with node-streams.dependencies =
        [ "aff"
        , "effect"
        , "exceptions"
        , "maybe"
        , "node-buffer"
        , "node-event-emitter"
        , "nullable"
        , "prelude"
        , "unsafe-coerce"
        ]
      with node-fs.version = "v9.1.0"
      with node-fs.dependencies =
        [ "datetime"
        , "effect"
        , "either"
        , "enums"
        , "exceptions"
        , "functions"
        , "integers"
        , "js-date"
        , "maybe"
        , "node-buffer"
        , "node-path"
        , "node-streams"
        , "nullable"
        , "partial"
        , "prelude"
        , "strings"
        , "unsafe-coerce"
        ]
      with node-process.version = "v11.2.0"
      with node-process.dependencies =
        [ "effect"
        , "foreign-object"
        , "foreign"
        , "maybe"
        , "node-streams"
        , "node-event-emitter"
        , "posix-types"
        , "prelude"
        , "unsafe-coerce"
        ]
