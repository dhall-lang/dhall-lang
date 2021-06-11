{-|
Create a JSON number from a Dhall `Double`

```
let JSON = ./package.dhall
in  JSON.render (JSON.double 42.0)
= "42.0"

let JSON = ./package.dhall
in  JSON.render (JSON.double -1.5e-10)
= "-1.5e-10"
```
-}
let JSON =
        ./Type.dhall
          sha256:40edbc9371979426df63e064333b02689b969c4cfbbccfa481216d2d1a6e9759
      ? ./Type.dhall

let double
    : Double → JSON
    = λ(x : Double) →
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
        json.double x

in  double
