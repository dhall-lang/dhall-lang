{-|
Create a CBOR map item from a Dhall `Map` of `CBOR` values

See ./textMap.dhall for an example.
-}

let List/map = ../List/map.dhall

let Map/Entry = ../Map/Entry.dhall

let CBOR = ./Type.dhall

let CBOR/Entry = Map/Entry CBOR CBOR

let CBOR/Map = List CBOR/Entry

let map
    : CBOR/Map → CBOR
    = λ(x : CBOR/Map) →
      λ(CBOR : Type) →
        let CBOR/Entry = Map/Entry CBOR CBOR

        in  λ ( cbor
              : { array : List CBOR → CBOR
                , bool : Bool → CBOR
                , double : Double → CBOR
                , integer : Integer → CBOR
                , map : List CBOR/Entry → CBOR
                , null : CBOR
                , simple : Natural → CBOR
                , tag : Natural → CBOR → CBOR
                , text : Text → CBOR
                , undefined : CBOR
                }
              ) →
              cbor.map
                ( List/map
                    CBOR/Entry@1
                    CBOR/Entry
                    ( λ(e : CBOR/Entry@1) →
                        { mapKey = e.mapKey CBOR cbor
                        , mapValue = e.mapValue CBOR cbor
                        }
                    )
                    x
                )

in  map
