{-|
Create a CBOR item that incapsulates CBOR data

A Dhall to CBOR encoder should convert the inner item to a byte string
and tag it as enclosed CBOR data. This is for the benefit of a decoding
process that is not concerned with the semantics of the inner data.
See RFC7049 section 2.4.4.1 for more information.
-}
let tag = ./tag.dhall let data = tag 24 in data
