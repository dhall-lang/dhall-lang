# B19 — Type inference: universes and variables

Depends on: B18  
Read first: `prompts/any-language/00-shared.md`

## Spec

`type-inference.md`: inferred types are β-normal if Γ is. Context is an
ordered list of binders; `x@n` picks the n-th `x`. `Type : Kind`,
`Kind : Sort`, `Sort` has no type.

## Tests that must pass

```text
tests/type-inference/success/unit/TypeA.dhall
tests/type-inference/success/unit/KindA.dhall
```

(If names differ slightly, match the Type/Kind unit files.) Failure:
unbound variables / `Z.dhall`. Timeout on failures.

## Done when

`inferType(Γ, t)` exists and those tests pass.
