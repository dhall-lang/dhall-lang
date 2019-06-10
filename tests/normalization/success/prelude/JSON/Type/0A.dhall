let JSON = ../../../../../../Prelude/JSON/package.dhall

in  JSON.object
    [ { mapKey = "foo", mapValue = JSON.null }
    , { mapKey =
          "bar"
      , mapValue =
          JSON.array [ JSON.number 1.0, JSON.bool True ]
      }
    ]
