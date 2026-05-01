--| `lessThan` checks if one Natural is strictly less than another.
let lessThan
    : Natural → Natural → Bool
    = Natural/lessThan

let example0 = assert : lessThan 5 6 ≡ True

let example1 = assert : lessThan 5 5 ≡ False

let example2 = assert : lessThan 5 4 ≡ False

in  lessThan
