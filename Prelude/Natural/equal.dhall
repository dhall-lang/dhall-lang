--| `equal` checks if two Naturals are equal.
let equal
    : Natural → Natural → Bool
    = Natural/equal

let example0 = assert : equal 5 5 ≡ True

let example1 = assert : equal 5 6 ≡ False

let property0 = λ(n : Natural) → assert : equal n n ≡ True

in  equal
