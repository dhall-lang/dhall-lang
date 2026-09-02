# B17 — β-normalization: functions, let, annotation, assert, remaining unit

Depends on: B16  
Read first: `prompts/any-language/00-shared.md`

## Spec

`beta-normalization.md`: Functions, let, type annotations, assertions.
β-reduce `(λ(x : A) → b) a` via shift/substitute. `let` as specified
(usually: annotate, then substitute). `assert` stays `assert` after
normalizing the type. Imports are **not** reduced (wait for B27+).

## Tests that must pass

Remaining `tests/normalization/success/unit/**` (246 total), then
import-free:

```text
tests/normalization/success/simple/**
tests/normalization/success/simplifications/**
tests/normalization/success/haskell-tutorial/**
tests/normalization/success/regression/**
```

Skip parsed ASTs that still contain `Import`.

## Done when

All import-free normalization tests pass.
