# A02 — Wire α-normalization tests

Depends on: A01  
Read first: `prompts/haskell/00-shared.md`

## Goal

The literate implementation in `alpha-normalization.md` already exists.
Connect it to the acceptance suite.

## Spec

- `standard/alpha-normalization.md` — judgment `t₀ ↦ t₁`
- `tests/README.md` — parse A and B, α-normalize **both**, compare

## Do

1. Add a tasty group: for each `tests/alpha-normalization/success/**/*A.dhall`,
   parse A and B, apply `AlphaNormalization.alphaNormalize` to both, compare
   via `Binary.encode` (NaN-aware).
2. If tests fail, fix `alpha-normalization.md` (and only that) to match the
   judgments. Do not “fix” tests to match a simpler algorithm.

## Tests that must pass

```text
tests/alpha-normalization/success/unit/*A.dhall          # 9
tests/alpha-normalization/success/regression/*A.dhall    # 1 (preludeBoolFold)
```

Plus all A01 parser tests.

## Done when

Those 10 cases pass. Parser suites still pass.
