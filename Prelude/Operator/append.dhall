{-
`append Type m n` compute `m # n`
-}
let append
    : ∀(type : Type) → List type → List type → List type
    = λ(type : Type) → λ(m : List type) → λ(n : List type) → m # n

let example0 = assert : append Natural [ 1, 2 ] [ 3 ] ≡ [ 1, 2, 3 ]

in  append
