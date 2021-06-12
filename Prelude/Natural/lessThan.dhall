--| `lessThan` checks if one Natural is strictly less than another.
let greaterThanEqual =
        ./greaterThanEqual.dhall
          sha256:30ebfab0febd7aa0ccccfdf3dc36ee6d50f0117f35dd4a9b034750b7e885a1a4
      ? ./greaterThanEqual.dhall

let Bool/not =
        ../Bool/not.dhall
          sha256:723df402df24377d8a853afed08d9d69a0a6d86e2e5b2bac8960b0d4756c7dc4
      ? ../Bool/not.dhall

let lessThan
    : Natural → Natural → Bool
    = λ(x : Natural) → λ(y : Natural) → Bool/not (greaterThanEqual x y)

let example0 = assert : lessThan 5 6 ≡ True

let example1 = assert : lessThan 5 5 ≡ False

let example2 = assert : lessThan 5 4 ≡ False

let property0 = λ(n : Natural) → assert : lessThan n 0 ≡ False

let property1 = λ(n : Natural) → assert : lessThan n n ≡ False

in  lessThan
