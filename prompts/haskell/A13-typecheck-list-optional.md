# A13 — Type inference: List, Optional, Some, None

Depends on: A12  
Read first: `prompts/haskell/00-shared.md`

## Spec

`standard/type-inference.md` — List, Optional.

Empty list **requires** a type annotation. Homogeneous elements. `None`
needs a type argument. `Some` wraps a term.

## Tests that must pass

Success `tests/type-inference/success/unit/`:

```text
List*
Optional*
Some*
None*
```

Failure `tests/type-inference/failure/unit/`:

```text
ListLiteral*
EmptyToMap          # skip if toMap is A15
Some*               # if any
Optional*
RemovedBuiltinOptionalBuild
RemovedBuiltinOptionalFold
```

Plus A11–A12.

## Done when

List/Optional unit success and the listed failures pass.
