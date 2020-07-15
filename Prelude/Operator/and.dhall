{-
`and m n` compute `m && n`
-}
let and
    : Bool → Bool → Bool
    = λ(m : Bool) → λ(n : Bool) → m && n

let example0 = assert : and False True ≡ False

in  and
