--| `build` is the inverse of `fold`
let build
    : (∀(bool : Type) → ∀(true : bool) → ∀(false : bool) → bool) → Bool
    = λ(f : ∀(bool : Type) → ∀(true : bool) → ∀(false : bool) → bool) →
        f Bool True False

let example0 =
        assert
      : build (λ(bool : Type) → λ(true : bool) → λ(false : bool) → true) ≡ True

let example1 =
        assert
      :   build (λ(bool : Type) → λ(true : bool) → λ(false : bool) → false)
        ≡ False

in  build
