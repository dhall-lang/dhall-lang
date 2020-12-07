{-|
Create a tagged CBOR item from a base64url `Text` value

A Dhall to CBOR encoder should decode the text into a CBOR byte string,
see RFC7049 section 2.4.4.2 and 2.4.4.3 for more information.
-}
let tag = ./tag.dhall

let text = ./text.dhall

let base64url = λ(b64u : Text) → tag 33 (text b64u)

in  base64url
