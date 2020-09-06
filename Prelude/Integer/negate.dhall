--| Invert the sign of an `Integer`, with zero remaining unchanged.
let negate
    : Integer → Integer
    = Integer/negate

let example0 = assert : negate -3 ≡ +3

let example2 = assert : negate +7 ≡ -7

let example3 = assert : negate +0 ≡ +0

in  negate
