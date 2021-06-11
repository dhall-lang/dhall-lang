{-|
Create a `NonEmpty` list using a function instead of a record

This might come in handy if you want to decouple the `NonEmpty` list
construction from the specific names of the fields.
-}
let NonEmpty =
        ./Type.dhall
          sha256:e2e247455a858317e470e0e4affca8ac07f9f130570ece9cb7ac1f4ea3deb87f
      ? ./Type.dhall

let make
    : ∀(a : Type) → ∀(head : a) → ∀(tail : List a) → NonEmpty a
    = λ(a : Type) → λ(head : a) → λ(tail : List a) → { head, tail }

let example = assert : make Natural 1 [ 2, 3 ] ≡ { head = 1, tail = [ 2, 3 ] }

in  make
