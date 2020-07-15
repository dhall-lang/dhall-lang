{-
`textAppend m n` compute `m ++ n`
-}
let textAppend
    : Text → Text → Text
    = λ(m : Text) → λ(n : Text) → m ++ n

let example0 = assert : textAppend "Hello" "Dhall" ≡ "HelloDhall"

in  textAppend
