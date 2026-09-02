# A17 — Type inference: functions, let, annotations, assert, ≡

Depends on: A15, A12  
Read first: `prompts/haskell/00-shared.md`

## Spec

`standard/type-inference.md`: Functions, let expressions, type
annotations, assertions, equivalence operator `===`.

Use `FunctionCheck.functionCheck` for Π/λ universes. Use
`Equivalence.equivalent` for `===` and annotation matching.

Unresolved `Import` nodes are a type error (`type-inference.md` — Imports).

## Tests that must pass

Success `tests/type-inference/success/unit/`:

```text
Function*
Let*
Assert*
Equivalence*
Operator*              # remaining operator unit tests
ConstructorShift
```

Failure `tests/type-inference/failure/unit/`:

```text
Function*
Let*
Assert*
Equivalence*
Operator*
NestedAnnot*
```

Also `tests/type-inference/failure/SortInLet.dhall` and
`tests/type-inference/failure/hurkensParadox.dhall` (must fail; timeout
allowed).

Plus A11–A16.

## Done when

Those tests pass. Prelude and import-backed tests wait for A26.
