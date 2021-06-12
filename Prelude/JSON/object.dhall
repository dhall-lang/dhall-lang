{-|
Create a JSON object from a Dhall `Map`

```
let JSON = ./package.dhall
in  JSON.render
    ( JSON.object
      [ { mapKey = "foo", mapValue = JSON.double 1.0 }
      , { mapKey = "bar", mapValue = JSON.bool True  }
      ]
    )
= "{ \"foo\": 1.0, \"bar\": true }"

let JSON/Type = ./Type
let JSON = ./package.dhall
in  JSON.render
    (JSON.object ([] : List { mapKey : Text, mapValue : JSON/Type }))
= "{ }"
```
-}
let JSON =
        ./Type.dhall
          sha256:40edbc9371979426df63e064333b02689b969c4cfbbccfa481216d2d1a6e9759
      ? ./Type.dhall

let List/map =
        ../List/map.dhall
          sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680
      ? ../List/map.dhall

let object
    : List { mapKey : Text, mapValue : JSON } → JSON
    = λ(x : List { mapKey : Text, mapValue : JSON }) →
      λ(JSON : Type) →
      λ ( json
        : { array : List JSON → JSON
          , bool : Bool → JSON
          , double : Double → JSON
          , integer : Integer → JSON
          , null : JSON
          , object : List { mapKey : Text, mapValue : JSON } → JSON
          , string : Text → JSON
          }
        ) →
        json.object
          ( List/map
              { mapKey : Text, mapValue : JSON@1 }
              { mapKey : Text, mapValue : JSON }
              ( λ(kv : { mapKey : Text, mapValue : JSON@1 }) →
                  { mapKey = kv.mapKey, mapValue = kv.mapValue JSON json }
              )
              x
          )

in  object
