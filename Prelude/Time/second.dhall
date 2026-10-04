{-
Extract the second component of `Time` as a `Natural` number.
-}
let second
    : Time → Natural
    = Time/second

let example0 = assert : second 03:15:47.90 ≡ 47

let example1 = assert : second 00:00:00 ≡ 0

let example2 = assert : second 11:59:59 ≡ 59

in  second
