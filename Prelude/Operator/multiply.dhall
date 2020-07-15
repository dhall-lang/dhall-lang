{-
`multiply m n` compute `m * n`
-}
let multiply
    : Natural → Natural → Natural
    = λ(m : Natural) → λ(n : Natural) → m * n

let example0 = assert : multiply 21 2 ≡ 42

in  multiply
