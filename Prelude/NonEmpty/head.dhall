--| Retrieve the first element of the `NonEmpty` list
let NonEmpty =
        ./Type.dhall
          sha256:e2e247455a858317e470e0e4affca8ac07f9f130570ece9cb7ac1f4ea3deb87f
      ? ./Type.dhall

let head
    : ∀(a : Type) → NonEmpty a → a
    = λ(a : Type) → λ(xs : NonEmpty a) → xs.head

let example = assert : head Natural { head = 0, tail = [ 1, 2 ] } ≡ 0

in  head
