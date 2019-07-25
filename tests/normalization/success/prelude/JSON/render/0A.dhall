let JSON = ../../../../../../Prelude/JSON/package.dhall

in  JSON.render
    ( JSON.array
      [ JSON.bool True
      , JSON.string "Hello"
      , JSON.object
        [ { mapKey = "foo", mapValue = JSON.null       }
        , { mapKey = "bar", mapValue = JSON.number 1.0 }
        ]
      ]
    )
