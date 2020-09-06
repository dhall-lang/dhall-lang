--| Returns the number of elements in a list
let length
    : ∀(a : Type) → List a → Natural
    = List/length

let example0 = assert : length Natural [ 0, 1, 2 ] ≡ 3

let example1 = assert : length Natural ([] : List Natural) ≡ 0

in  length
