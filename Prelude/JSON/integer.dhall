{-|
Create a JSON number from a Dhall `Integer`

```
let JSON = ./package.dhall
in  JSON.render (JSON.integer -1)
= "-1"

let JSON = ./package.dhall
in  JSON.render (JSON.integer +2)
= "+2"
```
-}
let JSON =
        ./Type.dhall
          sha256:40edbc9371979426df63e064333b02689b969c4cfbbccfa481216d2d1a6e9759
      ? ./Type.dhall

let integer
    : Integer → JSON
    = λ(x : Integer) →
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
        json.integer x

in  integer
