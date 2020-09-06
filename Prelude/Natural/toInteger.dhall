--| Convert a `Natural` number to the corresponding `Integer`
let toInteger
    : Natural → Integer
    = Natural/toInteger

let example0 = assert : toInteger 3 ≡ +3

let example1 = assert : toInteger 0 ≡ +0

in  toInteger
