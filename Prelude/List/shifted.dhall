{-|
Combine a `List` of `List`s, offsetting the `index` of each element by the
number of elements in preceding lists
-}
let shifted
    : ∀(a : Type) →
      List (List { index : Natural, value : a }) →
        List { index : Natural, value : a }
    = λ(a : Type) →
      λ(kvss : List (List { index : Natural, value : a })) →
        List/build
          { index : Natural, value : a }
          ( λ(list : Type) →
            λ(cons : { index : Natural, value : a } → list → list) →
            λ(nil : list) →
              let result =
                    List/fold
                      (List { index : Natural, value : a })
                      kvss
                      { count : Natural, diff : Natural → list }
                      ( λ(kvs : List { index : Natural, value : a }) →
                        λ(y : { count : Natural, diff : Natural → list }) →
                          let length =
                                List/length { index : Natural, value : a } kvs

                          in  { count = y.count + length
                              , diff =
                                  λ(n : Natural) →
                                    List/fold
                                      { index : Natural, value : a }
                                      kvs
                                      list
                                      ( λ ( kvOld
                                          : { index : Natural, value : a }
                                          ) →
                                        λ(z : list) →
                                          let kvNew =
                                                { index = kvOld.index + n
                                                , value = kvOld.value
                                                }

                                          in  cons kvNew z
                                      )
                                      (y.diff (n + length))
                              }
                      )
                      { count = 0, diff = λ(_ : Natural) → nil }

              in  result.diff 0
          )

let example0 =
        assert
      :   shifted
            Bool
            [ [ { index = 0, value = True }
              , { index = 1, value = True }
              , { index = 2, value = True }
              ]
            , [ { index = 0, value = False }, { index = 1, value = False } ]
            , [ { index = 0, value = True }
              , { index = 1, value = True }
              , { index = 2, value = True }
              , { index = 3, value = True }
              ]
            ]
        ≡ [ { index = 0, value = True }
          , { index = 1, value = True }
          , { index = 2, value = True }
          , { index = 3, value = False }
          , { index = 4, value = False }
          , { index = 5, value = True }
          , { index = 6, value = True }
          , { index = 7, value = True }
          , { index = 8, value = True }
          ]

let example1 =
        assert
      :   shifted Bool ([] : List (List { index : Natural, value : Bool }))
        ≡ ([] : List { index : Natural, value : Bool })

in  shifted
