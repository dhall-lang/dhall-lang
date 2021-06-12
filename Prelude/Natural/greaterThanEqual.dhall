{-|
`greaterThanEqual` checks if one Natural is greater than or equal to another.
-}
let lessThanEqual =
        ./lessThanEqual.dhall
          sha256:1a5caa2b80a42b9f58fff58e47ac0d9a9946d0b2d36c54034b8ddfe3cb0f3c99
      ? ./lessThanEqual.dhall

let greaterThanEqual
    : Natural → Natural → Bool
    = λ(x : Natural) → λ(y : Natural) → lessThanEqual y x

let example0 = assert : greaterThanEqual 5 6 ≡ False

let example1 = assert : greaterThanEqual 5 5 ≡ True

let example2 = assert : greaterThanEqual 5 4 ≡ True

let property0 = λ(n : Natural) → assert : greaterThanEqual n 0 ≡ True

let property1 = λ(n : Natural) → assert : greaterThanEqual n n ≡ True

in  greaterThanEqual
