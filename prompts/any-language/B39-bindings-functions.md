# B39 — Bindings: functions

Depends on: B38, B25  
Read first: `prompts/any-language/00-shared.md`

## Mapping

A Dhall value of type `∀(x : A) → B` (or `A → B`) becomes a host function
`a -> b` **only if** both `A` and `B` have bindings.

To apply: encode the host argument with `Encoder A`, apply in the Dhall
AST, β-normalize, type-check if you wish (should already be well-typed),
decode with `Decoder B`.

If the result is still a function, return another callable.

Do not bind un-normalized λ that is not a value of a function type you
declared. Do not bind `Type`/`Kind`/`Sort` as runtime values.

## Tests (local)

- Dhall `λ(x : Bool) → x == False` decoded as `bool -> bool`; apply
  `True` get `False`.
- Dhall `λ(x : Natural) → x + 1` applied to `41` yields `42`.
- Reject decoding a `Natural` with a function decoder.

## Done when

Those tests pass. Nested functions work at least one extra arrow
(`Bool → Bool → Bool`) or you document a limitation.
