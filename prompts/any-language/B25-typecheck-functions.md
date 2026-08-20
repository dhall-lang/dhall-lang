# B25 — Type inference: functions, let, annotations, assert

Depends on: B23, B20  
Read first: `prompts/any-language/00-shared.md`

## Spec

`type-inference.md`: Functions, let, annotations, assertions, `===`.
Unresolved imports are type errors. Use `functionCheck` and `equivalent`.

## Tests that must pass

Success: `Function*`, `Let*`, `Assert*`, `Equivalence*`, remaining
`Operator*`, `ConstructorShift`.
Failure: `Function*`, `Let*`, `Assert*`, `Equivalence*`, `Operator*`,
`NestedAnnot*`, `SortInLet.dhall`, `hurkensParadox.dhall` (timeout).

## Done when

Those tests pass.
