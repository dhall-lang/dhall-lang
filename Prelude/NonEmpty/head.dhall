--| Retrieve the first element of the `NonEmpty` list
let NonEmpty = ./Type.dhall

let head
    : ∀(a : Type) → NonEmpty a → a
    = λ(a : Type) → λ(xs : NonEmpty a) → xs.head

let example = assert : head Natural { head = 0, tail = [ 1, 2 ] } ≡ 0

in  head
