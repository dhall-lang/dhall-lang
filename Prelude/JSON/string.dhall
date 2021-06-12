{-|
Create a JSON string from Dhall `Text`

```
let JSON = ./package.dhall
in  JSON.render (JSON.string "ABC $ \" 🙂")
= "\"ABC \\u0024 \\\" 🙂\""

let JSON = ./package.dhall
in  JSON.render (JSON.string "")
= "\"\""
```
-}
let JSON =
        ./Type.dhall
          sha256:40edbc9371979426df63e064333b02689b969c4cfbbccfa481216d2d1a6e9759
      ? ./Type.dhall

let string
    : Text → JSON
    = λ(x : Text) →
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
        json.string x

in  string
