{-|
Returns `True` if the supplied function returns `True` for all elements in the
`NonEmpty` list
-}
let NonEmpty = ./Type.dhall

let NonEmpty/toList = ./toList.dhall

let all
    : ∀(a : Type) → (a → Bool) → NonEmpty a → Bool
    = λ(a : Type) →
      λ(f : a → Bool) →
      λ(xs : NonEmpty a) →
        List/fold
          a
          (NonEmpty/toList a xs)
          Bool
          (λ(x : a) → λ(r : Bool) → f x && r)
          True

let example0 =
      assert : all Natural Natural/even { head = 2, tail = [ 3, 5 ] } ≡ False

let example1 =
        assert
      : all Natural Natural/even { head = 2, tail = [] : List Natural } ≡ True

in  all
