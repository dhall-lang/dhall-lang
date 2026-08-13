{-
Extract the month component of a `Date` as a `Natural` number.
-}
let month
    : Date → Natural
    = Date/month

let example0 = assert : month 2025-03-17 ≡ 3

let example1 = assert : month 9999-12-31 ≡ 12

let example2 = assert : month 0000-01-01 ≡ 1

in  month
