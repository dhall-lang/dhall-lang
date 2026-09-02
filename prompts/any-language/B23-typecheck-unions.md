# B23 — Type inference: unions, merge, toMap, showConstructor

Depends on: B22  
Read first: `prompts/any-language/00-shared.md`

## Spec

`type-inference.md` Unions. Defer `freeVars` (B26) if `MergeHandlerFreeVar`
fails.

## Tests that must pass

Success: `Union*`, `Merge*`, `ToMap*`, `ShowConstructor*`.
Failure: `Union*`, `Merge*` (except FreeVar), `*ToMap*`, `ShowConstructor*`.

## Done when

Those tests pass.
