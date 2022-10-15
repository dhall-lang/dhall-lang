{-
Render a `Time` as `Text` using the same representation as Dhall source code
(i.e. `hh:mm:ss`)
-}
let show
      : Time → Text
      = Time/show

let example0 = assert : show 03:15:47.90 ≡ "03:15:47.90"

let example1 = assert : show 00:00:00 ≡ "00:00:00"

let example2 = assert : show 11:59:59 ≡ "11:59:59"

in  show
