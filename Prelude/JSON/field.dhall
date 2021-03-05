{-|
Create a JSON field for a JSON object from a Dhall `Text` key and a JSON value

```
let JSON = ./package.dhall
in  JSON.render
      ( JSON.object
          [ JSON.field "foo" (JSON.double 1.0)
          , JSON.field "bar" (JSON.bool True)
          ]
      )
= "{ \"foo\": 1.0, \"bar\": true }"
```
-}
let JSON =
        ./Type.dhall sha256:40edbc9371979426df63e064333b02689b969c4cfbbccfa481216d2d1a6e9759
      ? ./Type.dhall

let Map/keyValue =
        ../Map/keyValue.dhall sha256:a0a97199d280c4cce72ffcbbf93b7ceda0a569cf4d173ac98e0aaaa78034b98c
      ? ../Map/keyValue.dhall

let field
    : Text → JSON → { mapKey : Text, mapValue : JSON }
    = λ(mapKey : Text) → λ(mapValue : JSON) → Map/keyValue JSON mapKey mapValue

in  field
