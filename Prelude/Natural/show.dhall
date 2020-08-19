{-|
Render a `Natural` number as `Text` using the same representation as Dhall
source code (i.e. a decimal number)
-}
let show
    : Natural → Text
    = Natural/show

let example0 = assert : show 3 ≡ "3"

let example1 = assert : show 0 ≡ "0"

in  show
