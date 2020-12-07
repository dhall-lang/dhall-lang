{-|
Create a CBOR floating-point number item from a `Double` value
-}
let CBOR/Type = ./Type.dhall

let double
    : Double → CBOR/Type
    = λ(x : Double) →
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
        cbor.double x

in  double
