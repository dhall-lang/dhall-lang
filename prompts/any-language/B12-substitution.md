# B12 — Substitution

Depends on: B11  
Read first: `prompts/any-language/00-shared.md`

## Spec

`standard/substitution.md` — `e₀[x@n ≔ a] = e₁`. Uses `shift`. Implement
every case.

## Tests that must pass

Internal tests from the document. β-reduction will be the real test (B14+).

## Done when

Substitution matches the judgments, including incrementing indices under
binders of the same name.
