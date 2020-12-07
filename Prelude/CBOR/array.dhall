{-|
Create a CBOR array item from a `List` of `CBOR` values

See ./toArray.dhall for an example.
-}
let List/map = ../List/map.dhall

let CBOR = ./Type.dhall

let array
    : List CBOR → CBOR
    = λ(x : List CBOR) →
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
        cbor.array
          (List/map CBOR@1 CBOR (λ(cbor : CBOR@1) → cbor CBOR cbor@1) x)

in  array
