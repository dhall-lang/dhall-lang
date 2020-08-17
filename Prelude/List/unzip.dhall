--| Unzip a `List` into two separate `List`s
let unzip
    : ∀(a : Type) →
      ∀(b : Type) →
      List { _1 : a, _2 : b } →
        { _1 : List a, _2 : List b }
    = λ(a : Type) →
      λ(b : Type) →
      λ(xs : List { _1 : a, _2 : b }) →
        { _1 =
            List/build
              a
              ( λ(list : Type) →
                λ(cons : a → list → list) →
                  List/fold
                    { _1 : a, _2 : b }
                    xs
                    list
                    (λ(x : { _1 : a, _2 : b }) → cons x._1)
              )
        , _2 =
            List/build
              b
              ( λ(list : Type) →
                λ(cons : b → list → list) →
                  List/fold
                    { _1 : a, _2 : b }
                    xs
                    list
                    (λ(x : { _1 : a, _2 : b }) → cons x._2)
              )
        }

let example0 =
        assert
      :   unzip
            Text
            Bool
            [ { _1 = "ABC", _2 = True }
            , { _1 = "DEF", _2 = False }
            , { _1 = "GHI", _2 = True }
            ]
        ≡ { _1 = [ "ABC", "DEF", "GHI" ], _2 = [ True, False, True ] }

let example1 =
        assert
      :   unzip Text Bool ([] : List { _1 : Text, _2 : Bool })
        ≡ { _1 = [] : List Text, _2 = [] : List Bool }

in  unzip
