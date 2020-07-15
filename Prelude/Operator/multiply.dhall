{-
`mul m n` compute `m * n`
-}
let mul
    : Natural → Natural → Natural
    = λ(m : Natural) → λ(n : Natural) → m * n

let example0 = assert : mul 21 2 ≡ 42

in  mul
