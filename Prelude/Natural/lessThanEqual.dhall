--| `lessThanEqual` checks if one Natural is less than or equal to another.
let lessThanEqual
    : Natural → Natural → Bool
    = λ(x : Natural) → λ(y : Natural) → Natural/isZero (Natural/subtract y x)

let example0 = assert : lessThanEqual 5 6 ≡ True

let example1 = assert : lessThanEqual 5 5 ≡ True

let example2 = assert : lessThanEqual 5 4 ≡ False

let property0 = λ(n : Natural) → assert : lessThanEqual 0 n ≡ True

let property1 = λ(n : Natural) → assert : lessThanEqual n n ≡ True

in  lessThanEqual
