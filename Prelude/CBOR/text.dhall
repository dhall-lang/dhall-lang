{-|
Create a CBOR byte string from a `Text` value
-}
let CBOR = ./Type.dhall

let text
    : Text → CBOR
    = λ(x : Text) →
      λ(CBOR : Type) →
      λ ( cbor
        : { array : List CBOR → CBOR
          , bool : Bool → CBOR
          , double : Double → CBOR
          , integer : Integer → CBOR
          , map : List { mapKey : CBOR, mapValue : CBOR } → CBOR
          , null : CBOR
          , simple : Natural → CBOR
          , tag : Natural → CBOR → CBOR
          , text : Text → CBOR
          , undefined : CBOR
          }
        ) →
        cbor.text x

in  text
