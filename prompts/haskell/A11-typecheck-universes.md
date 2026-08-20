# A11 — Type inference: module, universes, variables, function check

Depends on: A03 (β), A10 not required  
Read first: `prompts/haskell/00-shared.md`

## Goal

Create literate `TypeInference` from `type-inference.md` (today: judgments
only). Implement constants, variables, and the call into `FunctionCheck`.

## Spec

- `standard/type-inference.md` — Normalization note, Constants, Variables
- `standard/function-check.md` — already implemented

Inferred types in the spec are β-normal. You MAY skip normalizing inferred
types if they remain `equivalent` to the spec types; the unit tests compare
the inferred type to `*B.dhall` after parsing B (typically already normal).
Prefer following the spec and β-normalizing inferred types.

Context `Γ`: ordered list of `(name, type)` pairs; De Bruijn index selects
which binder. `Sort` has no type.

## Do

1. Add `type-inference.md` to the literate symlink map from A00
   (`TypeInference.lhs` → `type-inference.md`).
2. Export `inferType :: [(Text, Expression)] -> Expression -> Maybe Expression`
   (or `Either` with a simple error). Empty context for closed tests.
3. Tasty group for type-inference **success** unit tests whose names are
   exactly:

```text
Type, Kind
```

and any `Variable*` files if present under
`tests/type-inference/success/unit/`. Also enable failure tests:

```text
tests/type-inference/failure/unit/Z.dhall
tests/type-inference/failure/SortInLet.dhall   # skip if it needs more rules
```

`Z.dhall` is the unbound variable / no type of Sort case used as a
canary — if the filename is literally `Z`, treating it as “must fail” is
enough.

4. Register the module in `standard.cabal`.

## Tests that must pass

Whatever unit success/failure cases you enable in this slice (document the
list). Prefer starting tiny and adding files as later slices land.

Minimum: inferring `Type : Kind` and `Kind : Sort` against the
corresponding unit fixtures if they exist (`TypeA.dhall` / `KindA.dhall`).

## Done when

`cabal build` exposes `TypeInference`. The small unit set passes. Later
slices extend the same `inferType` function.
