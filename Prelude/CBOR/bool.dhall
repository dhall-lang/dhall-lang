{-|
Create a CBOR boolean item from a `Bool` value
-}
let CBOR/Type = ./Type.dhall

let bool
    : Bool → CBOR/Type
    = λ(x : Bool) →
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
        cbor.bool x

in  bool
