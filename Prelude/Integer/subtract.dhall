--| `subtract m n` computes `n - m`.
let nonPositive =
        ./nonPositive.dhall
          sha256:e00a852eed5b84ff60487097d8aadce53c9e5301f53ff4954044bd68949fac3b
      ? ./nonPositive.dhall

let subtractNonNegative =
      λ(xi : Integer) →
      λ(yi : Integer) →
        let xn = Integer/clamp xi

        let yn = Integer/clamp yi

        let dn = Natural/subtract xn yn

        in  if    Natural/isZero dn
            then  Integer/negate (Natural/toInteger (Natural/subtract yn xn))
            else  Natural/toInteger dn

let subtract
    : Integer → Integer → Integer
    = λ(m : Integer) →
      λ(n : Integer) →
        if    nonPositive m
        then  if    nonPositive n
              then  subtractNonNegative (Integer/negate n) (Integer/negate m)
              else  Natural/toInteger
                      (Integer/clamp (Integer/negate m) + Integer/clamp n)
        else  if nonPositive n
        then  Integer/negate
                ( Natural/toInteger
                    (Integer/clamp m + Integer/clamp (Integer/negate n))
                )
        else  subtractNonNegative m n

let example0 = assert : subtract +3 +5 ≡ +2

let example1 = assert : subtract +4 +4 ≡ +0

let example2 = assert : subtract +5 +3 ≡ -2

let example3 = assert : subtract -3 -5 ≡ -2

let example4 = assert : subtract -4 -4 ≡ +0

let example5 = assert : subtract -5 -3 ≡ +2

let example6 = assert : subtract -3 +5 ≡ +8

let example7 = assert : subtract +3 -5 ≡ -8

let example8 = assert : subtract +0 -3 ≡ -3

in  subtract
