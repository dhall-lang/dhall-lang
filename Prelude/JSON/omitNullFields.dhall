{-|
This utility omits all `null` record fields, which is often the idiomatic way
for a configuration to encode absent fields
-}
let JSON =
        ./core.dhall
          sha256:5dc1135d5481cfd6fde625aaed9fcbdb7aa7c14f2e76726aa5fdef028a5c10f5
      ? ./core.dhall

let List/concatMap =
        ../List/concatMap.dhall
          sha256:3b2167061d11fda1e4f6de0522cbe83e0d5ac4ef5ddf6bb0b2064470c5d3fb64
      ? ../List/concatMap.dhall

let List/map =
        ../List/map.dhall
          sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680
      ? ../List/map.dhall

let omitNullFields
    : JSON.Type → JSON.Type
    = λ(old : JSON.Type) →
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
        let result =
              old
                { value : JSON, isNull : Bool }
                { string =
                    λ(x : Text) → { value = json.string x, isNull = False }
                , double =
                    λ(x : Double) → { value = json.double x, isNull = False }
                , integer =
                    λ(x : Integer) → { value = json.integer x, isNull = False }
                , object =
                    λ ( keyValues
                      : List
                          { mapKey : Text
                          , mapValue : { value : JSON, isNull : Bool }
                          }
                      ) →
                      let value =
                            json.object
                              ( List/concatMap
                                  { mapKey : Text
                                  , mapValue : { value : JSON, isNull : Bool }
                                  }
                                  { mapKey : Text, mapValue : JSON }
                                  ( λ ( keyValue
                                      : { mapKey : Text
                                        , mapValue :
                                            { value : JSON, isNull : Bool }
                                        }
                                      ) →
                                      if    keyValue.mapValue.isNull
                                      then  [] : List
                                                   { mapKey : Text
                                                   , mapValue : JSON
                                                   }
                                      else  [   keyValue.{ mapKey }
                                              ∧ { mapValue =
                                                    keyValue.mapValue.value
                                                }
                                            ]
                                  )
                                  keyValues
                              )

                      in  { value, isNull = False }
                , array =
                    λ(xs : List { value : JSON, isNull : Bool }) →
                      let value =
                            json.array
                              ( List/map
                                  { value : JSON, isNull : Bool }
                                  JSON
                                  ( λ(x : { value : JSON, isNull : Bool }) →
                                      x.value
                                  )
                                  xs
                              )

                      in  { value, isNull = False }
                , bool = λ(x : Bool) → { value = json.bool x, isNull = False }
                , null = { value = json.null, isNull = True }
                }

        in  result.value

let property =
      λ(a : Text) →
      λ(b : Double) →
      λ(c : Bool) →
          assert
        :   omitNullFields
              ( JSON.object
                  ( toMap
                      { string = JSON.string a
                      , double = JSON.double b
                      , bool = JSON.bool c
                      , null = JSON.null
                      }
                  )
              )
          ≡ JSON.object
              ( toMap
                  { string = JSON.string a
                  , double = JSON.double b
                  , bool = JSON.bool c
                  }
              )

let example =
        assert
      :   omitNullFields
            ( JSON.object
                ( toMap
                    { array =
                        JSON.array [ JSON.object (toMap { null = JSON.null }) ]
                    }
                )
            )
        ≡ JSON.object
            ( toMap
                { array =
                    JSON.array
                      [ JSON.object
                          ([] : List { mapKey : Text, mapValue : JSON.Type })
                      ]
                }
            )

let example =
        assert
      : omitNullFields (JSON.array [ JSON.null ]) ≡ JSON.array [ JSON.null ]

in  omitNullFields
