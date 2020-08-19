{-|
Returns `1` if the `Optional` value is present and `0` if the value is absent
-}
let length
    : ∀(a : Type) → Optional a → Natural
    = λ(a : Type) →
      λ(xs : Optional a) →
        merge { Some = λ(_ : a) → 1, None = 0 } xs

let example0 = assert : length Natural (Some 2) ≡ 1

let example1 = assert : length Natural (None Natural) ≡ 0

in  length
