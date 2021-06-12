--| This renders JSON on a single line
let JSON =
        ./core.dhall
          sha256:5dc1135d5481cfd6fde625aaed9fcbdb7aa7c14f2e76726aa5fdef028a5c10f5
      ? ./core.dhall

let Text/concatMapSep =
        ../Text/concatMapSep
          sha256:c272aca80a607bc5963d1fcb38819e7e0d3e72ac4d02b1183b1afb6a91340840
      ? ../Text/concatMapSep

let renderInteger =
        ./renderInteger.dhall
          sha256:15b8d2ae46d5002832741927af787761df49798c911e2bf85db7a7b9cb5c078c
      ? ./renderInteger.dhall

let renderCompact
    : JSON.Type → Text
    = λ(j : JSON.Type) →
        j
          Text
          { string = Text/show
          , double = Double/show
          , integer = renderInteger
          , object =
              λ(x : List { mapKey : Text, mapValue : Text }) →
                let body =
                      Text/concatMapSep
                        ","
                        { mapKey : Text, mapValue : Text }
                        ( λ(e : { mapKey : Text, mapValue : Text }) →
                            " ${Text/show e.mapKey}: ${e.mapValue}"
                        )
                        x

                in  "{${body} }"
          , array =
              λ(x : List Text) →
                let body = Text/concatMapSep "," Text (λ(y : Text) → " ${y}") x

                in  "[${body} ]"
          , bool = λ(x : Bool) → if x then "true" else "false"
          , null = "null"
          }

let example =
        assert
      :   renderCompact
            ( JSON.array
                [ JSON.bool True
                , JSON.string "Hello"
                , JSON.object
                    [ { mapKey = "foo", mapValue = JSON.null }
                    , { mapKey = "bar", mapValue = JSON.double 1.1 }
                    , { mapKey = "baz", mapValue = JSON.integer +2 }
                    ]
                ]
            )
        ≡ "[ true, \"Hello\", { \"foo\": null, \"bar\": 1.1, \"baz\": 2 } ]"

in  renderCompact
