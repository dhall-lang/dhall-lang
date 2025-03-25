{-| Provide a general validation condition as a dependent type.

For example, `Validate Natural Natural/isZero 0` is the unit type `{}`,
while `Validate Natural Natural/isZero 1` is the void type `<>`.
-}
let Validate
    : ∀(T : Type) → ∀(cond : T → Bool) → T → Type
    = λ(T : Type) → λ(cond : T → Bool) → λ(t : T) → if cond t then {} else <>

let boolTrue = λ(_ : Bool) → True

let Bool/not =
        missing
          sha256:723df402df24377d8a853afed08d9d69a0a6d86e2e5b2bac8960b0d4756c7dc4
      ? ../Bool/not.dhall

let voidIdentity = λ(x : <>) → x

let _ = {=} : Validate Bool boolTrue False

let _ = {=} : Validate Bool boolTrue True

let _ = {=} : Validate Bool Bool/not False

let _ =
    -- Verify that `Validate Bool Bool/not True` is the void type by type-annotating the `voidIdentity`.
    -- (Note that we cannot use `assert` directly on types as `assert : <> === <>`.)
      voidIdentity : Validate Bool Bool/not True → <>

in  Validate
