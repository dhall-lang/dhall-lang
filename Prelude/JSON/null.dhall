{-|
Create a JSON null

```
let JSON = ./package.dhall
in  JSON.render JSON.null
= "null"
```
-}
let JSON =
        ./Type.dhall
          sha256:40edbc9371979426df63e064333b02689b969c4cfbbccfa481216d2d1a6e9759
      ? ./Type.dhall

let null
    : JSON
    = λ(JSON : Type) →
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
        json.null

in  null
