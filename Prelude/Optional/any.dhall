{-|
Returns `True` if the supplied function returns `True` for a present element and
`False` otherwise
-}
let any
    : ∀(a : Type) → (a → Bool) → Optional a → Bool
    = λ(a : Type) →
      λ(f : a → Bool) →
      λ(xs : Optional a) →
        merge { Some = f, None = False } xs

let example0 = assert : any Natural Natural/even (Some 2) ≡ True

let example1 = assert : any Natural Natural/even (None Natural) ≡ False

in  any
