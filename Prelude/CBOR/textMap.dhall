{-|
Create a CBOR map item from a Dhall `Map` of `CBOR` values

See ./diagnostic.dhall for an example.
-}

let List/map = ../List/map.dhall

let CBOR = ./Type.dhall

let CBOR/map = ./map.dhall

let CBOR/text = ./text.dhall

let TextEntry = { mapKey : Text, mapValue : CBOR }

let textMap
    : List TextEntry → CBOR
    = λ(xs : List TextEntry) →
        CBOR/map
          ( List/map
              TextEntry
              { mapKey : CBOR, mapValue : CBOR }
              (λ(x : TextEntry) → x with mapKey = CBOR/text x.mapKey)
              xs
          )

in  textMap
