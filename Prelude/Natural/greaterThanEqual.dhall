{-|
`greaterThanEqual` checks if one Natural is greater than or equal to another.
-}
let lessThanEqual =
        missing
          sha256:0e3dd9c7f319dc4d88898d7b1811dfbf7ea55d4fc997fd0b455d1b0647991fad
      ? ./lessThanEqual.dhall

let greaterThanEqual
    : Natural → Natural → Bool
    = λ(x : Natural) → λ(y : Natural) → lessThanEqual y x

let example0 = assert : greaterThanEqual 5 6 ≡ False

let example1 = assert : greaterThanEqual 5 5 ≡ True

let example2 = assert : greaterThanEqual 5 4 ≡ True

in  greaterThanEqual
