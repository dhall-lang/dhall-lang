{-|
Render an `Integer` as `Text` using the same representation as Dhall source
code (i.e. a decimal number with a leading `-` sign if negative and a leading
`+` sign if non-negative)
-}
let show
    : Integer → Text
    = Integer/show

let example0 = assert : show -3 ≡ "-3"

let example1 = assert : show +0 ≡ "+0"

in  show
