--| Convert a `NonEmpty` list into the equivalent `List`
let NonEmpty = ./Type.dhall

let toList
    : ∀(a : Type) → NonEmpty a → List a
    = λ(a : Type) → λ(xs : NonEmpty a) → [ xs.head ] # xs.tail

let example0 =
      assert : toList Natural { head = 2, tail = [ 3, 5 ] } ≡ [ 2, 3, 5 ]

let example1 =
      assert : toList Natural { head = 2, tail = [] : List Natural } ≡ [ 2 ]

in  toList
