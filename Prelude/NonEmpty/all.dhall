{-|
Returns `True` if the supplied function returns `True` for all elements in the
`NonEmpty` list
-}
let NonEmpty =
        missing
          sha256:e2e247455a858317e470e0e4affca8ac07f9f130570ece9cb7ac1f4ea3deb87f
      ? ./Type.dhall

let NonEmpty/toList =
        missing
          sha256:0977fe14b77232a4451dcf409c43df4589c4b3cdde7b613aab8df183be1b53f5
      ? ./toList.dhall

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
