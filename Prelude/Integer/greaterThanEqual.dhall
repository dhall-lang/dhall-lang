{-|
`greaterThanEqual` checks if one Integer is greater than or equal to another.
-}
let lessThanEqual =
        missing
          sha256:9325300826d708bdc1d86dabba599b93c375e2c8db20176bed272a8256482555
      ? ./lessThanEqual.dhall

let greaterThanEqual
    : Integer → Integer → Bool
    = λ(x : Integer) → λ(y : Integer) → lessThanEqual y x

let example0 = assert : greaterThanEqual +5 +6 ≡ False

let example1 = assert : greaterThanEqual +5 +5 ≡ True

let example2 = assert : greaterThanEqual +5 +4 ≡ True

let example3 = assert : greaterThanEqual -5 +8 ≡ False

let example4 = assert : greaterThanEqual -5 -3 ≡ False

let example5 = assert : greaterThanEqual -3 -5 ≡ True

let example6 = assert : greaterThanEqual -3 -3 ≡ True

in  greaterThanEqual
