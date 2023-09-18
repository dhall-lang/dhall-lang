{-|
Render a `Natural` number as `Text` using the hexadecimal
notation (including the `0x` prefix and using uppercase letters).
-}
let showHex
    : Natural → Text
    = Natural/showHex

let example0 = assert : showHex 6899 ≡ "0x1AF3"

let example1 = assert : showHex 0 ≡ "0x0"

in  showHex
