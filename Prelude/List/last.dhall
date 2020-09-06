--| Retrieve the last element of the list
let last
    : ∀(a : Type) → List a → Optional a
    = List/last

let example0 = assert : last Natural [ 0, 1, 2 ] ≡ Some 2

let example1 = assert : last Natural ([] : List Natural) ≡ None Natural

in  last
