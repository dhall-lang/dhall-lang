--| `greaterThan` checks if one Natural is strictly greater than another.
let lessThan =
        missing
          sha256:11d199975930afe0a666095d43948588d4bc4aa52c4fd8dbbfe68c72dd3f1908
      ? ./lessThan.dhall

let greaterThan
    : Natural → Natural → Bool
    = λ(x : Natural) → λ(y : Natural) → lessThan y x

let example0 = assert : greaterThan 5 6 ≡ False

let example1 = assert : greaterThan 5 5 ≡ False

let example2 = assert : greaterThan 5 4 ≡ True

let property0 = λ(n : Natural) → assert : greaterThan n n ≡ False

in  greaterThan
