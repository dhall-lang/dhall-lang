# B18 — Equivalence and function check

Depends on: B13, B17, B09  
Read first: `prompts/any-language/00-shared.md`

## Spec

- `standard/equivalence.md` — `l ≡ r` iff `encode(α(β(l)))` equals
  `encode(α(β(r)))` (NaN-safe because you compare CBOR, not IEEE `==`).
  No η-equivalence.
- `standard/function-check.md` — `c₀ ↝ c₁ : c₂` and `T₀ ⋁ T₁` for
  Type/Kind/Sort.

## Tests that must pass

No dedicated suite. Internal tests: `λ(a : Type) → a` ≡ `λ(b : Type) → b`;
`λ(f : Bool → Bool) → λ(x : Bool) → f x` is **not** ≡ `λ(f : Bool → Bool) → f`.
Function check table from the document.

## Done when

`equivalent` and `functionCheck` are used by type inference (B19+).
