{-|
Convert an `Integer` to a `Natural` number, with negative numbers becoming zero.
-}
let clamp
    : Integer → Natural
    = Integer/clamp

let example0 = assert : clamp +7 ≡ 7

let example2 = assert : clamp +0 ≡ 0

let example3 = assert : clamp -3 ≡ 0

in  clamp
