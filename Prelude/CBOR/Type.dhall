{-|
Dhall encoding of an arbitrary CBOR value
-}
let CBOR/Type
    : Type
    = ∀(CBOR : Type) →
      ∀ ( cbor
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
        CBOR

in  CBOR/Type
