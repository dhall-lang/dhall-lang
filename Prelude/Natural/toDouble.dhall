--| Convert a `Natural` number to the corresponding `Double`
let toDouble
    : Natural → Double
    = λ(n : Natural) → Integer/toDouble (Natural/toInteger n)

let example0 = assert : toDouble 3 ≡ 3.0

let example1 = assert : toDouble 0 ≡ 0.0

in  toDouble
