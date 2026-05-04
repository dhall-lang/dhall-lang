--| Retrieve an element from a `NonEmpty` list using its 0-based index
let NonEmpty =
        missing
          sha256:e2e247455a858317e470e0e4affca8ac07f9f130570ece9cb7ac1f4ea3deb87f
      ? ./Type.dhall

let List/index =
        missing
          sha256:99124de1a23182471a3f5c3edfdc9b0daec10a59ae6ef2b6c8f59ccb4ea32a25
      ? ../List/index.dhall

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
