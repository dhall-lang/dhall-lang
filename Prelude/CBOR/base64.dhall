{-|
Create a tagged CBOR item from a base64 `Text` value

A Dhall to CBOR encoder should decode the text into a CBOR byte string,
see RFC7049 section 2.4.4.2 and 2.4.4.3 for more information.
-}
let CBOR = ./Type.dhall

let tag = ./tag.dhall

let text = ./text.dhall

let base64
    : Text → CBOR
    = λ(b64 : Text) → tag 34 (text b64)

in  base64
