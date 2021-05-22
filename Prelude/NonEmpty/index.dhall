--| Retrieve an element from a `NonEmpty` list using its 0-based index
let NonEmpty = ./Type.dhall

let List/index = ../List/index.dhall

let index
    : Natural → ∀(a : Type) → NonEmpty a → Optional a
    = λ(n : Natural) →
      λ(a : Type) →
      λ(xs : NonEmpty a) →
        if    Natural/isZero n
        then  Some xs.head
        else  List/index (Natural/subtract 1 n) a xs.tail

let property =
      λ(n : Natural) →
      λ(a : Type) →
      λ(xs : NonEmpty a) →
        assert : index 0 a xs ≡ Some xs.head

let example0 = assert : index 1 Natural { head = 2, tail = [ 3, 5 ] } ≡ Some 3

let example1 =
        assert
      : index 1 Natural { head = 2, tail = [] : List Natural } ≡ None Natural

in  index
