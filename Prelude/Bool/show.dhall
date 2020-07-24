{-|
Render a `Bool` as `Text` using the same representation as Dhall source code
(i.e. beginning with a capital letter)
-}
let show
    : Bool → Text
    = λ(b : Bool) → if b then "True" else "False"

let example0 = assert : show True ≡ "True"

let example1 = assert : show False ≡ "False"

in  show
