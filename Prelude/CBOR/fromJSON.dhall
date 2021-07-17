{-|
Translate a `JSON` value to a `CBOR` value
-}

let List/map = ../List/map.dhall

let JSON = ../JSON/Type.dhall

let CBOR = ./Type.dhall

let fromJSON
    : JSON → CBOR
    = λ(json : JSON) →
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
        json
          CBOR
          { array = cbor.array
          , bool = cbor.bool
          , double = cbor.double
          , integer = cbor.integer
          , null = cbor.null
          , object =
              let Entry = { mapKey : Text, mapValue : CBOR }

              in  λ(m : List Entry) →
                    cbor.map
                      ( List/map
                          Entry
                          { mapKey : CBOR, mapValue : CBOR }
                          (λ(e : Entry) → e with mapKey = cbor.text e.mapKey)
                          m
                      )
          , string = cbor.text
          }

in  fromJSON
