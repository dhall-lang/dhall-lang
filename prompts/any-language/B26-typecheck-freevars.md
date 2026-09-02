# B26 — `freeVars` and remaining type-inference (import-free)

Depends on: B25  
Read first: `prompts/any-language/00-shared.md`

## Spec

`type-inference.md` free-variable rules. Fill `…` by union of subterms.
Fix the unannotated-let conclusion to follow the premises (`V₄ = V₂ ∪ V₃`).

## Tests that must pass

```text
tests/type-inference/failure/unit/MergeHandlerFreeVar.dhall
tests/type-inference/success/unit/**/*A.dhall
tests/type-inference/success/simple/**/*A.dhall
tests/type-inference/success/regression/**/*A.dhall
tests/type-inference/failure/**/*.dhall
```

Skip prelude and CacheImports* until B32.

## Done when

All import-free type-inference tests pass.
