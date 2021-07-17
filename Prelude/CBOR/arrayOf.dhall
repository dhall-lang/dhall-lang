{-|
Create a CBOR array item from a `List` of values and a conversion function

See ./diagnostic.dhall for an example.
-}

let List/map = ../List/map.dhall

let CBOR = ./Type.dhall

let CBOR/array = ./array.dhall

let arrayOf
    : ∀(a : Type) → (a → CBOR) → List a → CBOR
    = λ(a : Type) →
      λ(f : a → CBOR) →
      λ(xs : List a) →
        CBOR/array (List/map a CBOR f xs)

in  arrayOf
