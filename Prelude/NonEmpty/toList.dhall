--| Convert a `NonEmpty` list into the equivalent `List`
let NonEmpty =
        ./Type.dhall
          sha256:e2e247455a858317e470e0e4affca8ac07f9f130570ece9cb7ac1f4ea3deb87f
      ? ./Type.dhall

let toList
    : ∀(a : Type) → NonEmpty a → List a
    = λ(a : Type) → λ(xs : NonEmpty a) → [ xs.head ] # xs.tail

let example0 =
      assert : toList Natural { head = 2, tail = [ 3, 5 ] } ≡ [ 2, 3, 5 ]

let example1 =
      assert : toList Natural { head = 2, tail = [] : List Natural } ≡ [ 2 ]

in  toList
