{-|
Returns `True` if the supplied function returns `True` for any element in the
`List`
-}
let any
    : ∀(a : Type) → (a → Bool) → List a → Bool
    = λ(a : Type) →
      λ(f : a → Bool) →
      λ(xs : List a) →
        List/fold a xs Bool (λ(x : a) → λ(r : Bool) → f x || r) False

let example0 = assert : any Natural Natural/even [ 2, 3, 5 ] ≡ True

let example1 = assert : any Natural Natural/even ([] : List Natural) ≡ False

in  any
