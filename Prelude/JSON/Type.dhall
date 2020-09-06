{-|
Dhall encoding of an arbitrary JSON value

For example, the following JSON value:

```
[ { "foo": null, "bar": [ 1.0, true ] } ]
```

... corresponds to the following Dhall expression:

```
λ(JSON : Type) →
λ ( json
  : { array : List JSON → JSON
    , bool : Bool → JSON
    , null : JSON
    , double : Double → JSON
    , integer : Integer → JSON
    , object : List { mapKey : Text, mapValue : JSON } → JSON
    , string : Text → JSON
    }
  ) →
  json.object
    [ { mapKey = "foo", mapValue = json.null }
    , { mapKey = "bar"
      , mapValue = json.array [ json.double 1.0, json.bool True ]
      }
    ]
```

  You do not need to create these values directly, though.  You can use
  the utilities exported by `./package.dhall` to create values of this type,
  such as:

```
let JSON = ./package.dhall

in  JSON.object
      [ { mapKey = "foo", mapValue = JSON.null }
      , { mapKey = "bar"
        , mapValue = JSON.array [ JSON.double 1.0, JSON.bool True ]
        }
      ]
```

-}
let JSON/Type
    : Type
    = ∀(JSON : Type) →
      ∀ ( json
        : { array : List JSON → JSON
          , bool : Bool → JSON
          , double : Double → JSON
          , integer : Integer → JSON
          , null : JSON
          , object : List { mapKey : Text, mapValue : JSON } → JSON
          , string : Text → JSON
          }
        ) →
        JSON

in  JSON/Type
