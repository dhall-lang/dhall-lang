{-|
`greaterThanEqual` checks if one Integer is greater than or equal to another.
-}
let lessThanEqual =
        ./lessThanEqual.dhall
          sha256:e3cca9f3942f81fa78a2bea23c0c24519c67cfe438116c38e797e12dcd26f6bc
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
