--| Retrieve the last element of the `NonEmpty` list
let NonEmpty = ./Type.dhall

let last
    : ∀(a : Type) → NonEmpty a → a
    = λ(a : Type) →
      λ(xs : NonEmpty a) →
        merge { Some = λ(x : a) → x, None = xs.head } (List/last a xs.tail)

let example0 = assert : last Natural { head = 0, tail = [ 1, 2 ] } ≡ 2

let example1 = assert : last Natural { head = 0, tail = [] : List Natural } ≡ 0

in  last
