{-
`concat m n` compute `m ++ n`
-}
let concat
    : Text → Text → Text
    = λ(m : Text) → λ(n : Text) → m ++ n

let example0 = assert : concat "Hello" "Dhall" ≡ "HelloDhall"

in  concat
