--| Returns the absolute value of an `Integer`, i.e. its non-negative value.
let abs
    : Integer → Natural
    = λ(n : Integer) →
        if    Natural/isZero (Integer/clamp n)
        then  Integer/clamp (Integer/negate n)
        else  Integer/clamp n

let example0 = assert : abs +7 ≡ 7

let example2 = assert : abs +0 ≡ 0

let example3 = assert : abs -3 ≡ 3

in  abs
