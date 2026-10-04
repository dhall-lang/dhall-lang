{-
Extract the day component of a `Date` as a `Natural` number.
-}
let day
    : Date → Natural
    = Date/day

let example0 = assert : day 2025-03-17 ≡ 17

let example1 = assert : day 9999-12-31 ≡ 31

let example2 = assert : day 0000-01-01 ≡ 1

in  day
