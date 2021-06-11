--| `lessThan` checks if one Integer is less than another.
let greaterThan =
        ./greaterThan.dhall
          sha256:d23affd73029fc9aaf867c2c7b86510ca2802d3f0d1f3e1d1a93ffd87b7cb64b
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
