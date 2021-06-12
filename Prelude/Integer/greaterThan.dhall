--| `greaterThan` checks if one Integer is greater than another.
let Bool/not =
        ../Bool/not.dhall
          sha256:723df402df24377d8a853afed08d9d69a0a6d86e2e5b2bac8960b0d4756c7dc4
      ? ../Bool/not.dhall

let lessThanEqual =
        ./lessThanEqual.dhall
          sha256:e3cca9f3942f81fa78a2bea23c0c24519c67cfe438116c38e797e12dcd26f6bc
      ? ./lessThanEqual.dhall

let greaterThan
    : Integer → Integer → Bool
    = λ(x : Integer) → λ(y : Integer) → Bool/not (lessThanEqual x y)

let example0 = assert : greaterThan +5 +6 ≡ False

let example1 = assert : greaterThan +5 +5 ≡ False

let example2 = assert : greaterThan +5 +4 ≡ True

let example3 = assert : greaterThan -5 +8 ≡ False

let example4 = assert : greaterThan -5 -3 ≡ False

let example5 = assert : greaterThan -3 -5 ≡ True

let example6 = assert : greaterThan -3 -3 ≡ False

in  greaterThan
