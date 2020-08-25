{-|
Render a `JSON` value as `Text`

This is useful for debugging `JSON` values or for tests.  For anything
more sophisticated you should use `dhall-to-json` or `dhall-to-yaml`
-}
let JSON =
        ./core.dhall sha256:5dc1135d5481cfd6fde625aaed9fcbdb7aa7c14f2e76726aa5fdef028a5c10f5
      ? ./core.dhall

let renderAs =
        ./renderAs.dhall sha256:5576473c02bc447d40d08bf103aaeca9637c1040367fdf07ff70032ba3e28043
      ? ./renderAs.dhall

let Format =
        ./Format.dhall sha256:d7936b510cfc091faa994652af0eb5feb889cd44bc989edbe4f1eb8c5623caac
      ? ./Format.dhall

let render
    : JSON.Type → Text
    = renderAs Format.JSON

let example0 =
      let data =
              assert
            :   render
                  ( JSON.array
                      [ JSON.bool True
                      , JSON.string "Hello"
                      , JSON.object
                          [ { mapKey = "foo", mapValue = JSON.null }
                          , { mapKey = "bar", mapValue = JSON.double 1.0 }
                          ]
                      ]
                  )
              ≡ ''
                [
                  true,
                  "Hello",
                  {
                    "foo": null,
                    "bar": 1.0
                  }
                ]
                ''

      in  True

in  render
