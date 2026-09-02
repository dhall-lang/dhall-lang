# A16 — Type inference: `with` expressions

Depends on: A14  
Read first: `prompts/haskell/00-shared.md`

## Spec

`standard/type-inference.md` — `with` expressions / nested record updates.

## Tests that must pass

```text
tests/type-inference/success/unit/With*A.dhall
tests/type-inference/failure/unit/With*.dhall
```

Plus previous type-inference tests.

## Done when

All `With*` unit success and failure tests pass.
