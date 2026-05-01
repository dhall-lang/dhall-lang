--| `lessThan` checks if one Integer is less than another.
let greaterThan =
        missing
          sha256:70e28fc451db0b4d7d4e845f71d40d441c939365023f68645f0cb0f70a6a9dee
      ? ./greaterThan.dhall

let lessThan
    : Integer → Integer → Bool
    = λ(x : Integer) → λ(y : Integer) → greaterThan y x

let example0 = assert : lessThan +5 +6 ≡ True

let example1 = assert : lessThan +5 +5 ≡ False

let example2 = assert : lessThan +5 +4 ≡ False

let example3 = assert : lessThan -5 +8 ≡ True

let example4 = assert : lessThan -5 -3 ≡ True

let example5 = assert : lessThan -3 -5 ≡ False

let example6 = assert : lessThan -3 -3 ≡ False

in  lessThan
