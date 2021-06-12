--| `greaterThan` checks if one Natural is strictly greater than another.
let lessThan =
        ./lessThan.dhall
          sha256:3381b66749290769badf8855d8a3f4af62e8de52d1364d838a9d1e20c94fa70c
      ? ./lessThan.dhall

let greaterThan
    : Natural → Natural → Bool
    = λ(x : Natural) → λ(y : Natural) → lessThan y x

let example0 = assert : greaterThan 5 6 ≡ False

let example1 = assert : greaterThan 5 5 ≡ False

let example2 = assert : greaterThan 5 4 ≡ True

let property0 = λ(n : Natural) → assert : greaterThan 0 n ≡ False

let property1 = λ(n : Natural) → assert : greaterThan n n ≡ False

in  greaterThan
