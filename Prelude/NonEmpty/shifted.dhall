{-|
Combine a `NonEmpty` list of `NonEmpty` lists, offsetting the `index` of each
element by the number of elements in preceding lists
-}
let NonEmpty = ./Type.dhall

let NonEmpty/toList = ./toList.dhall

let List/map = ../List/map.dhall

let List/shifted = ../List/shifted.dhall

let shifted
    : ∀(a : Type) →
      NonEmpty (NonEmpty { index : Natural, value : a }) →
        NonEmpty { index : Natural, value : a }
    = λ(a : Type) →
      λ(kvss : NonEmpty (NonEmpty { index : Natural, value : a })) →
        { head = kvss.head.head
        , tail =
            List/shifted
              a
              (   [ kvss.head.tail ]
                # List/map
                    (NonEmpty { index : Natural, value : a })
                    (List { index : Natural, value : a })
                    ( λ(kvs : NonEmpty { index : Natural, value : a }) →
                        List/map
                          { index : Natural, value : a }
                          { index : Natural, value : a }
                          ( λ(kv : { index : Natural, value : a }) →
                              { index = kv.index + 1, value = kv.value }
                          )
                          (NonEmpty/toList { index : Natural, value : a } kvs)
                    )
                    kvss.tail
              )
        }

let example0 =
        assert
      :   shifted
            Bool
            { head =
              { head = { index = 0, value = True }
              , tail =
                [ { index = 1, value = True }, { index = 2, value = True } ]
              }
            , tail =
              [ { head = { index = 0, value = False }
                , tail = [ { index = 1, value = False } ]
                }
              , { head = { index = 0, value = True }
                , tail =
                  [ { index = 1, value = True }
                  , { index = 2, value = True }
                  , { index = 3, value = True }
                  ]
                }
              ]
            }
        ≡ { head = { index = 0, value = True }
          , tail =
            [ { index = 1, value = True }
            , { index = 2, value = True }
            , { index = 3, value = False }
            , { index = 4, value = False }
            , { index = 5, value = True }
            , { index = 6, value = True }
            , { index = 7, value = True }
            , { index = 8, value = True }
            ]
          }

let example1 =
        assert
      :   shifted
            Bool
            { head =
              { head = { index = 0, value = True }
              , tail = [] : List { index : Natural, value : Bool }
              }
            , tail = [] : List (NonEmpty { index : Natural, value : Bool })
            }
        ≡ { head = { index = 0, value = True }
          , tail = [] : List { index : Natural, value : Bool }
          }

in  shifted
