{-|
Render a `Double` as `Text` using the same representation as Dhall source
code (i.e. a decimal floating point number with a leading `-` sign if negative)
-}
let show
    : Double → Text
    = Double/show

let example0 = assert : show -3.1 ≡ "-3.1"

let example1 = assert : show 0.4 ≡ "0.4"

in  show
