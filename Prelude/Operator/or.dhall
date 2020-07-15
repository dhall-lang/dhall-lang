{-
`or m n` compute `m || n`
-}
let or
    : Bool → Bool → Bool
    = λ(m : Bool) → λ(n : Bool) → m || n

let example0 = assert : or False True ≡ True

in  or
