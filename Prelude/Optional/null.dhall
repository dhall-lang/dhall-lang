--| Returns `True` if the `Optional` value is absent and `False` if present
let null
    : ∀(a : Type) → Optional a → Bool
    = λ(a : Type) →
      λ(xs : Optional a) →
        merge { Some = λ(_ : a) → False, None = True } xs

let example0 = assert : null Natural (Some 2) ≡ False

let example1 = assert : null Natural (None Natural) ≡ True

in  null
