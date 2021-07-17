{-|
Create a CBOR tagged item from a `Natural` tag value and a `CBOR` value

See ./base64.dhall as an example, see RFC7049 section 2.4 for more information.
-}
let CBOR = ./Type.dhall

let tag
    : Natural → CBOR → CBOR
    = λ(tag : Natural) →
      λ(value : CBOR) →
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
        cbor.tag tag (value CBOR cbor)

in  tag
