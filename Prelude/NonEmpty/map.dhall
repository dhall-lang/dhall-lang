--| Transform a `NonEmpty` list by applying a function to each element
let NonEmpty =
        ./Type.dhall
          sha256:e2e247455a858317e470e0e4affca8ac07f9f130570ece9cb7ac1f4ea3deb87f
      ? ./Type.dhall

let List/map =
        ../List/map.dhall
          sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680
      ? ../List/map.dhall

let map
    : ∀(a : Type) → ∀(b : Type) → (a → b) → NonEmpty a → NonEmpty b
    = λ(a : Type) →
      λ(b : Type) →
      λ(f : a → b) →
      λ(xs : NonEmpty a) →
        { head = f xs.head, tail = List/map a b f xs.tail }

let example0 =
        assert
      :   map Natural Bool Natural/even { head = 2, tail = [ 3, 5 ] }
        ≡ { head = True, tail = [ False, False ] }

let example1 =
        assert
      :   map Natural Bool Natural/even { head = 2, tail = [] : List Natural }
        ≡ { head = True, tail = [] : List Bool }

in  map
