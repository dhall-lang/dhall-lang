{-
``||` m n` compute `m || n`
-}
let `||`
    : Bool → Bool → Bool
    = λ(m : Bool) → λ(n : Bool) → m || n

let example0 = assert : `||` False True ≡ True

in  `||`
