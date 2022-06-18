--| Truncate a list to the first `n` elements
let take
    : ∀(n : Natural) → ∀(a : Type) → List a → List a
    = List/take

let example = assert : take 2 Natural [ 2, 3, 5 ] ≡ [ 2, 3 ]

let example = assert : take 5 Natural [ 2, 3, 5 ] ≡ [ 2, 3, 5 ]

in  take
