# B24 — Type inference: `with`

Depends on: B22  
Read first: `prompts/any-language/00-shared.md`

## Spec

`type-inference.md` `with` / nested record updates.

## Tests that must pass

```text
tests/type-inference/success/unit/With*A.dhall
tests/type-inference/failure/unit/With*.dhall
```

## Done when

All With* unit tests pass.
