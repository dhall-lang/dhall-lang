--| Convert an `Integer` to the corresponding `Double`
let toDouble
    : Integer → Double
    = Integer/toDouble

let example0 = assert : toDouble -3 ≡ -3.0

let example1 = assert : toDouble +2 ≡ 2.0

in  toDouble
