# A03 — Wire β-normalization unit tests

Depends on: A02  
Read first: `prompts/haskell/00-shared.md`

## Goal

`beta-normalization.md` already implements `betaNormalize`. Run the **unit**
normalization suite (no import resolution).

## Spec

- `standard/beta-normalization.md` — `t₀ ⇥ t₁`
- `tests/README.md` — parse A and B; **do not** normalize B; compare
  `betaNormalize(A)` to parsed B. Unit tests must not require imports.

## Do

1. Tasty group over `tests/normalization/success/unit/**/*A.dhall` (246).
2. Skip any case whose parsed A still contains an `Import` node (should be
   none in `unit/`).
3. If failures appear, fix `beta-normalization.md` (or `shift.md` /
   `substitution.md` if the bug is clearly there). Keep literate style.

## Tests that must pass

```text
tests/normalization/success/unit/**/*A.dhall
```

Plus A01–A02.

## Done when

All 246 unit normalization tests pass. Report any skipped Import cases.
