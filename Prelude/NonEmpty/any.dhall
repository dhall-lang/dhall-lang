{-|
Returns `True` if the supplied function returns `True` for any element in the
`NonEmpty` list
-}
let NonEmpty = ./Type.dhall

let NonEmpty/toList = ./toList.dhall

let any
    : ∀(a : Type) → (a → Bool) → NonEmpty a → Bool
    = λ(a : Type) →
      λ(f : a → Bool) →
      λ(xs : NonEmpty a) →
        List/fold
          a
          (NonEmpty/toList a xs)
          Bool
          (λ(x : a) → λ(r : Bool) → f x || r)
          False

let example0 =
      assert : any Natural Natural/even { head = 2, tail = [ 3, 5 ] } ≡ True

let example1 =
        assert
      : any Natural Natural/even { head = 3, tail = [] : List Natural } ≡ False

in  any
