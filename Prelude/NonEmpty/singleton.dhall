--| Create a `NonEmpty` list with just one element
let NonEmpty =
        ./Type.dhall
          sha256:e2e247455a858317e470e0e4affca8ac07f9f130570ece9cb7ac1f4ea3deb87f
      ? ./Type.dhall

let singleton
    : ∀(a : Type) → a → NonEmpty a
    = λ(a : Type) → λ(x : a) → { head = x, tail = [] : List a }

let example =
      assert : singleton Natural 2 ≡ { head = 2, tail = [] : List Natural }

in  singleton
