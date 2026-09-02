# A19 — Type inference: simple, regression, remaining failures

Depends on: A18  
Read first: `prompts/haskell/00-shared.md`

## Goal

Finish every type-inference test that does **not** need import resolution
or the cache HTTP server.

## Tests that must pass

```text
tests/type-inference/success/simple/**/*A.dhall
tests/type-inference/success/regression/**/*A.dhall
tests/type-inference/success/unit/**/*A.dhall          # all 203
tests/type-inference/failure/unit/**/*.dhall          # all 121
tests/type-inference/failure/*.dhall                  # except Cache-related
```

Skip until A26:

```text
tests/type-inference/success/prelude/**
tests/type-inference/success/preludeA.dhall
tests/type-inference/success/CacheImportsA.dhall
tests/type-inference/success/CacheImportsCanonicalizeA.dhall
```

Skip any remaining success file whose parsed A contains `Import`.

Timeout on all failure tests.

## Done when

Import-free type-inference success and all failure tests pass.
