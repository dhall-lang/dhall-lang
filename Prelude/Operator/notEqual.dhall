{-
`notEqual m n` compute `m != n`
-}
let notEqual
    : Bool → Bool → Bool
    = λ(m : Bool) → λ(n : Bool) → m != n

let example0 = assert : notEqual True False ≡ True

in  notEqual
