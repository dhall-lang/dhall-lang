# B05 — Parser: imports

Depends on: B02, B03  
Read first: `prompts/any-language/00-shared.md`

## Spec

`dhall.abnf` `import-type`, `import-hashed`, `import`. Extend with
`as Source`:

```text
import = import-hashed [ whsp1 as whsp1 (Text / Location / Bytes / Source) ]
```

URL grammar is RFC 3986-ish as in the ABNF (percent-encoding, no quoted
URL path components). File prefixes `.` `..` `/` `~`. `env:VAR`.
`missing`. Optional `using headers` expression. Optional `sha256:…`.

Quoted path components: see `imports.md` “Quoted paths”.

## Tests that must pass after B07+B09

```text
tests/parser/success/unit/import/**/*A.dhall
tests/parser/failure/unit/Import*.dhall
```

## Done when

Import AST nodes can be produced from text for http(s), local, env,
missing, all modes including Source, with and without hashes.
