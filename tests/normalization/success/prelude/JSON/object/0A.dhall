let JSON = ../../../../../../Prelude/JSON/package.dhall
in  JSON.render
    ( JSON.object
      [ { mapKey = "foo", mapValue = JSON.number 1.0 }
      , { mapKey = "bar", mapValue = JSON.bool True  }
      ]
    )
