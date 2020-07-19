{-
``*` m n` compute `m * n`
-}
let `*`
    : Natural → Natural → Natural
    = λ(m : Natural) → λ(n : Natural) → m * n

let example0 = assert : `*` 21 2 ≡ 42

in  `*`
