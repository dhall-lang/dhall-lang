# B13 — α-normalization

Depends on: B12  
Read first: `prompts/any-language/00-shared.md`

## Spec

`standard/alpha-normalization.md` — `t₀ ↦ t₁`. Bound names become `_`;
free variables unchanged.

## Tests that must pass

```text
tests/alpha-normalization/success/**/*A.dhall
# α(parse(A)) equals α(parse(B)) via encode
```

10 cases (9 unit + preludeBoolFold).

## Done when

Those 10 tests pass.
