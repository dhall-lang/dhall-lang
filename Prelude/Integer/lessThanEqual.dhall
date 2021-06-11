--| `lessThanEqual` checks if one Integer is less than or equal to another.
let Natural/greaterThanEqual =
        ../Natural/greaterThanEqual.dhall
          sha256:30ebfab0febd7aa0ccccfdf3dc36ee6d50f0117f35dd4a9b034750b7e885a1a4
      ? ../Natural/greaterThanEqual.dhall

let Natural/lessThanEqual =
        ../Natural/lessThanEqual.dhall
          sha256:1a5caa2b80a42b9f58fff58e47ac0d9a9946d0b2d36c54034b8ddfe3cb0f3c99
      ? ../Natural/lessThanEqual.dhall

let nonPositive =
        ./nonPositive.dhall
          sha256:e00a852eed5b84ff60487097d8aadce53c9e5301f53ff4954044bd68949fac3b
      ? ./nonPositive.dhall

let nonNegative =
        ./nonNegative.dhall
          sha256:b463373f070df6b1c8c7082051e0810fee38b360bab35256187c8c2b6af5c663
      ? ./nonNegative.dhall

let lessThanEqual
    : Integer → Integer → Bool
    = λ(x : Integer) →
      λ(y : Integer) →
        if    nonPositive x
        then      nonNegative y
              ||  Natural/greaterThanEqual
                    (Integer/clamp (Integer/negate x))
                    (Integer/clamp (Integer/negate y))
        else  Natural/lessThanEqual (Integer/clamp x) (Integer/clamp y)

let example0 = assert : lessThanEqual +5 +6 ≡ True

let example1 = assert : lessThanEqual +5 +5 ≡ True

let example2 = assert : lessThanEqual +5 +4 ≡ False

let example3 = assert : lessThanEqual -5 +8 ≡ True

let example4 = assert : lessThanEqual -5 -3 ≡ True

let example5 = assert : lessThanEqual -3 -5 ≡ False

let example6 = assert : lessThanEqual -3 -3 ≡ True

in  lessThanEqual
