{-|
Returns `True` if the supplied function returns `True` for all elements in the
`List`
-}
let all
    : ∀(a : Type) → (a → Bool) → List a → Bool
    = λ(a : Type) →
      λ(f : a → Bool) →
      λ(xs : List a) →
        List/fold a xs Bool (λ(x : a) → λ(r : Bool) → f x && r) True

let example0 = assert : all Natural Natural/even [ 2, 3, 5 ] ≡ False

let example1 = assert : all Natural Natural/even ([] : List Natural) ≡ True

in  all
