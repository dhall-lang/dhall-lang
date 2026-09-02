# B32 — HTTP(S), headers, CORS, remaining imports + prelude tests

Depends on: B31  
Read first: `prompts/any-language/00-shared.md`

## Spec

`tests/README.md` local server contract — **reimplement** it (do not copy
Haskell). HTTP `127.0.0.1:18080`, HTTPS `127.0.0.1:18443`, GET only,
static `/tests/import/*` with CORS `*`, `/foo` `/bar` header `Test:
example`, `/user-agent`, `/random-string`, CORS table. Unix newlines.
Self-signed TLS accepted in tests.

`imports.md` CORS judgment; origin headers from
`DHALL_HEADERS` / XDG / `~/.config/dhall/headers.dhall`.

Then enable prelude/cache type-inference, leftover normalization, semantic
hash prelude.

## Tests that must pass

Entire `tests/import/**`, then:

```text
tests/type-inference/success/prelude/**
tests/type-inference/success/CacheImports*.dhall
tests/type-inference/success/**/*A.dhall
tests/normalization/success/**/*A.dhall
tests/semantic-hash/success/**/*A.dhall
```

Slow is fine.

## Done when

The full language acceptance suite in `tests/` passes.
