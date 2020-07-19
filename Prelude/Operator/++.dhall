{-
``++` m n` compute `m ++ n`
-}
let `++`
    : Text → Text → Text
    = λ(m : Text) → λ(n : Text) → m ++ n

let example0 = assert : `++` "Hello" "Dhall" ≡ "HelloDhall"

in  `++`
