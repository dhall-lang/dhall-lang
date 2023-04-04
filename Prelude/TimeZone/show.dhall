{-
Render a `TimeZone` as `Text` using the same representation as Dhall source code
(i.e. `±HH:MM`)
-}
let show
    : TimeZone → Text
    = TimeZone/show

let example0 = assert : show +07:00 ≡ "+07:00"

let example1 = assert : show +00:00 ≡ "+00:00"

let example2 = assert : show -05:00 ≡ "-05:00"

in  show
