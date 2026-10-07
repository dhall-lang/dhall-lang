--| `lessThanEqual` checks if one Integer is less than or equal to another.
let Natural/greaterThanEqual =
        missing
          sha256:33319c5defc5b556a2e0355cdc6369b0138c5d0de787a85c98e35dc307c855e7
      ? ../Natural/greaterThanEqual.dhall

let Natural/lessThanEqual =
        missing
          sha256:0e3dd9c7f319dc4d88898d7b1811dfbf7ea55d4fc997fd0b455d1b0647991fad
      ? ../Natural/lessThanEqual.dhall

let nonPositive =
        missing
          sha256:e00a852eed5b84ff60487097d8aadce53c9e5301f53ff4954044bd68949fac3b
      ? ./nonPositive.dhall

let nonNegative =
        missing
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
