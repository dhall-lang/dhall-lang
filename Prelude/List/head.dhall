--| Retrieve the first element of the list
let head
    : ∀(a : Type) → List a → Optional a
    = List/head

let example0 = assert : head Natural [ 0, 1, 2 ] ≡ Some 0

let example1 = assert : head Natural ([] : List Natural) ≡ None Natural

in  head
