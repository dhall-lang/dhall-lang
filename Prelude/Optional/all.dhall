{-|
Returns `False` if the supplied function returns `False` for a present element
and `True` otherwise:
-}
let all
    : ∀(a : Type) → (a → Bool) → Optional a → Bool
    = λ(a : Type) →
      λ(f : a → Bool) →
      λ(xs : Optional a) →
        merge { Some = f, None = True } xs

let example0 = assert : all Natural Natural/even (Some 3) ≡ False

let example1 = assert : all Natural Natural/even (None Natural) ≡ True

in  all
