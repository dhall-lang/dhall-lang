{-
Extract the hour component of `Time` as a `Natural` number.
-}
let hour
    : Time → Natural
    = Time/hour

let example0 = assert : hour 03:15:47.90 ≡ 3

let example1 = assert : hour 00:00:00 ≡ 0

let example2 = assert : hour 11:59:59 ≡ 11

in  hour
