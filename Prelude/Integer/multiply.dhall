--| `multiply m n` computes `m * n`.
let nonPositive =
        ./nonPositive.dhall
          sha256:e00a852eed5b84ff60487097d8aadce53c9e5301f53ff4954044bd68949fac3b
      ? ./nonPositive.dhall

let multiplyNonNegative =
      λ(x : Integer) →
      λ(y : Integer) →
        Natural/toInteger (Integer/clamp x * Integer/clamp y)

let multiply
    : Integer → Integer → Integer
    = λ(m : Integer) →
      λ(n : Integer) →
        if    nonPositive m
        then  if    nonPositive n
              then  multiplyNonNegative (Integer/negate m) (Integer/negate n)
              else  Integer/negate (multiplyNonNegative (Integer/negate m) n)
        else  if nonPositive n
        then  Integer/negate (multiplyNonNegative m (Integer/negate n))
        else  multiplyNonNegative m n

let example0 = assert : multiply +3 +5 ≡ +15

let example1 = assert : multiply -3 +5 ≡ -15

let example2 = assert : multiply -3 -5 ≡ +15

let example3 = assert : multiply +0 +5 ≡ +0

let example4 = assert : multiply +5 +0 ≡ +0

let example5 = assert : multiply +0 +0 ≡ +0

let example6 = assert : multiply +1 +5 ≡ +5

let example7 = assert : multiply +3 -1 ≡ -3

in  multiply
