{-
``#` Type m n` compute `m # n`
-}
let `#`
    : ∀(type : Type) → List type → List type → List type
    = λ(type : Type) → λ(m : List type) → λ(n : List type) → m # n

let example0 = assert : `#` Natural [ 1, 2 ] [ 3 ] ≡ [ 1, 2, 3 ]

in  `#`
