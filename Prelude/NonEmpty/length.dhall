--| Returns the number of elements in a `NonEmpty` list
let NonEmpty =
        ./Type.dhall
          sha256:e2e247455a858317e470e0e4affca8ac07f9f130570ece9cb7ac1f4ea3deb87f
      ? ./Type.dhall

let length
    : ∀(a : Type) → NonEmpty a → Natural
    = λ(a : Type) → λ(xs : NonEmpty a) → List/length a xs.tail + 1

let example0 = assert : length Natural { head = 0, tail = [ 1, 2 ] } ≡ 3

let example1 =
      assert : length Natural { head = 0, tail = [] : List Natural } ≡ 1

in  length
