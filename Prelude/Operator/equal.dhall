{-
`equal m n` compute `m == n`
-}
let equal
    : Bool → Bool → Bool
    = λ(m : Bool) → λ(n : Bool) → m == n

let example0 = assert : equal True False ≡ False

in  equal
