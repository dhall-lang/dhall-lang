# B11 — Shift

Depends on: B01  
Read first: `prompts/any-language/00-shared.md`

## Spec

`standard/shift.md` — `↑(d, x, m, e₀) = e₁`. Used to avoid capture with
De Bruijn indices. Implement every case (the document is already
algorithmic). No performance tricks.

## Tests that must pass

No dedicated suite. Add internal tests from the examples in `shift.md`.
α/β slices will exercise it for real.

## Done when

`shift` is total on your AST and matches the document’s examples.
