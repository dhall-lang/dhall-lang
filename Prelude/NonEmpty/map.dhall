--| Transform a `NonEmpty` list by applying a function to each element
let NonEmpty = ./Type.dhall

let List/map = ../List/map.dhall

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
