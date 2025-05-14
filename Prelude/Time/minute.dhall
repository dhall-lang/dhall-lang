{-
Extract the minute component of `Time` as a `Natural` number.
-}
let minute
    : Time → Natural
    = Time/minute

let example0 = assert : minute 03:15:47.90 ≡ 15

let example1 = assert : minute 00:00:00 ≡ 0

let example2 = assert : minute 11:59:59 ≡ 59

in  minute
