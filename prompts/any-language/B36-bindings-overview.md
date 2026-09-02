# B36 — Bindings overview (not in the language standard)

Depends on: B26, B17  
Read first: `prompts/any-language/00-shared.md`

## Goal

A library that turns a **type-checked, β-normalized, import-free**
expression into a host value, and the reverse. `standard/README.md`
leaves this unspecified; these slices **freeze one portable mapping**.
They are **not** required for `tests/` .

dhall-haskell `FromDhall` / `ToDhall` is inspiration only.

## API shape (adapt names to the language)

- `Decoder a`: expected Dhall type + extract function from an expression
- `Encoder a`: Dhall type + inject function to an expression
- `decode(decoder, expr) -> a | error` after normalize
- `encode(encoder, a) -> expr`

Reject leftover λ, `Type`/`Kind`/`Sort`, and unevaluated builtins unless
a later slice says otherwise.

## Tests that must pass

Language-local (create `bindings-tests/` in **your** project, not
dhall-lang):

1. Decode `True` / `False` as booleans.
2. Reject `Natural` when a boolean decoder is used.

## Done when

Decoder/Encoder types exist; two local tests pass.
