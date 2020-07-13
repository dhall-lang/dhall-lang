{-
`add m n` compute `m + n`
-}
let add
    : Natural → Natural → Natural
    = λ(m : Natural) → λ(n : Natural) → m + n

let example0 = assert : add 2 1 ≡ 3

in  add
