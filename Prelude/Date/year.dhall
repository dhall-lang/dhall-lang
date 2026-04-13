{-
Extract the year component of a `Date` as a `Natural` number.
-}
let year
    : Date → Natural
    = Date/year

let example0 = assert : year 2025-03-17 ≡ 2025

let example1 = assert : year 9999-12-31 ≡ 9999

let example2 = assert : year 0000-01-01 ≡ 0

in  year
