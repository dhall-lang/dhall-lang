# A26 — Prelude, cache type-inference, leftover normalization / hash

Depends on: A25, A19, A10, A04  
Read first: `prompts/haskell/00-shared.md`

## Goal

Enable every remaining acceptance test that needs imports.

Naive evaluation of Prelude is expected to be slow. Do not optimize.

`tests/README.md`: CacheImports* need the local server and no Dhall cache
for type-inference (`--no-cache` equivalent).

## Tests that must pass

```text
tests/type-inference/success/prelude/**/*A.dhall
tests/type-inference/success/preludeA.dhall
tests/type-inference/success/CacheImportsA.dhall
tests/type-inference/success/CacheImportsCanonicalizeA.dhall
tests/type-inference/success/**/*A.dhall          # entire tree
tests/semantic-hash/success/prelude/**/*A.dhall
tests/semantic-hash/success/remoteSystemsA.dhall
tests/semantic-hash/success/**/*A.dhall
tests/normalization/success/**/*A.dhall           # including skipped import cases
tests/import/**                                   # already green from A25
```

Plus parser, binary-decode, alpha-normalization, type-inference failure.

## Done when

`cabal test` runs the full `tests/` tree as specified in `tests/README.md`
and passes. If something still fails, stop and report spec vs test
disagreement — do not weaken the suite.
