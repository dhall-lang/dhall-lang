--| Remove first `n` elements of a list
let drop
    : ∀(n : Natural) → ∀(a : Type) → List a → List a
    = List/drop

let example = assert : drop 2 Natural [ 2, 3, 5 ] ≡ [ 5 ]

let example = assert : drop 5 Natural [ 2, 3, 5 ] ≡ ([] : List Natural)

in  drop
