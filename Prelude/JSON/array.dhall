{-|
Create a JSON array from a `List` of JSON values

```
let JSON = ./package.dhall
in  JSON.render (JSON.array [ JSON.double 1.0, JSON.bool True ])
= "[ 1.0, true ]"

let JSON/Type = ./Type
let JSON = ./package.dhall
in  JSON.render (JSON.array ([] : List JSON/Type))
= "[ ]"
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

let array
    : List JSON → JSON
    = λ(x : List JSON) →
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
        json.array (List/map JSON@1 JSON (λ(j : JSON@1) → j JSON json) x)

in  array
