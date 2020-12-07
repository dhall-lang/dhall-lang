{-|
Create a CBOR integer item from an `Integer` value

Note that Dhall integers can be arbitrary size. For integers
that exceed the capacity of a CBOR integer item a Dhall to CBOR
converter may produce a CBOR bignum byte string instead.
-}
let CBOR/Type = ./Type.dhall

let integer
    : Integer → CBOR/Type
    = λ(x : Integer) →
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
        cbor.integer x

in  integer
