# B21 — Type inference: List and Optional

Depends on: B20  
Read first: `prompts/any-language/00-shared.md`

## Spec

`type-inference.md` List and Optional. Empty list needs an annotation;
elements share one type; `None` is applied to a type.

## Tests that must pass

Success: `List*`, `Optional*`, `Some*`, `None*`.
Failure: `ListLiteral*`, `RemovedBuiltinOptionalBuild`,
`RemovedBuiltinOptionalFold`.

## Done when

Those unit tests pass.
