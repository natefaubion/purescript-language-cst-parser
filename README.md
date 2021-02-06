# PureScript Language CST Parser

A parser for the PureScript concrete syntax tree.

## Development

The provided integration test attempts to parse a provided package set, and will report any errors it encounters.

```sh
npm run parse-package-set
```

Example output:

```text
---- [Error 3014 of 3047] ----
Failed to parse module at path:
./integration/tmp/.spago/web-url/v1.0.2/src/Web/URL/URLSearchParams.purs

With error:
(ParseError "Unexpected EOF" (Position { line: 1, column: 1 }))
```
