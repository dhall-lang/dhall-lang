{-|
Render a `CBOR` value to a diagnostic `Text` value

The diagnostic format is not formally defined and should not be parsed.
-}
let CBOR = ./Type.dhall

let Map/Type = ../Map/Type.dhall

let Text/concatSep = ../Text/concatSep.dhall

let Text/concatMapSep = ../Text/concatMapSep.dhall

let renderDiag
    : CBOR → Text
    = λ(cbor : CBOR) →
        cbor
          Text
          { array = λ(xs : List Text) → "[ " ++ Text/concatSep ", " xs ++ " ]"
          , bool = λ(x : Bool) → if x then "true" else "false"
          , double = Double/show
          , integer = Integer/show
          , map =
              λ(m : Map/Type Text Text) →
                    "{ "
                ++  Text/concatMapSep
                      ", "
                      { mapKey : Text, mapValue : Text }
                      ( λ(e : { mapKey : Text, mapValue : Text }) →
                          "${e.mapKey}: ${e.mapValue}"
                      )
                      m
                ++  " }"
          , null = "null"
          , simple = λ(x : Natural) → "simple(${Natural/show x})"
          , tag = λ(x : Natural) → λ(y : Text) → "${Natural/show x}(${y})"
          , text = Text/show
          , undefined = "undefined"
          }

let CBOR/base64 = ./base64.dhall

let CBOR/data = ./data.dhall

let CBOR/integer = ./integer.dhall

let CBOR/arrayOf = ./arrayOf.dhall

let CBOR/textMap = ./textMap.dhall

let CBOR/null = ./null.dhall

let example0 =
        assert
      :   ''
          ${renderDiag
              ( CBOR/textMap
                  ( toMap
                      { a = CBOR/integer +1
                      , b = CBOR/arrayOf Integer CBOR/integer [ +2, +3 ]
                      , c = CBOR/base64 "EjRWeA"
                      , d = CBOR/data CBOR/null
                      }
                  )
              )}
          ''
        ≡ ''
          { "a": +1, "b": [ +2, +3 ], "c": 34("EjRWeA"), "d": 24(null) }
          ''

in  renderDiag
