{-|
Render a `Natural` number as `Text` in hexadecimal base.
-}
let showInBase =
        missing
          sha256:392e1c620374180c1c57524589d34fd47685515caca5f6a622e14f6b2b8a245b
      ? ./showInBase.dhall

let showHex
    : Natural → Text
    = λ(n : Natural) →
        "0x" ++ (if Natural/isZero n then "0" else showInBase 16 {=} {=} n)

let _ = assert : showHex 0 ≡ "0x0"

let _ = assert : showHex 1 ≡ "0x1"

let _ = assert : showHex 10 ≡ "0xA"

let _ = assert : showHex 16 ≡ "0x10"

let _ = assert : showHex 64 ≡ "0x40"

let _ = assert : showHex 1023 ≡ "0x3FF"

let _ = assert : showHex 1025 ≡ "0x401"

let _ = assert : showHex 12345 ≡ "0x3039"

in  showHex
