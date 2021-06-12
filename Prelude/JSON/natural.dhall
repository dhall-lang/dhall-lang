{-|
Create a JSON number from a Dhall `Natural`

```
let JSON = ./package.dhall
in  JSON.render (JSON.natural 42)
= "42"
```
-}
let JSON =
        ./Type.dhall
          sha256:40edbc9371979426df63e064333b02689b969c4cfbbccfa481216d2d1a6e9759
      ? ./Type.dhall

let natural
    : Natural → JSON
    = λ(x : Natural) →
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
        json.integer (Natural/toInteger x)

in  natural
