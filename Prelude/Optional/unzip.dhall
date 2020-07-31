--| Unzip an `Optional` value into two separate `Optional` values
let unzip
    : ∀(a : Type) →
      ∀(b : Type) →
      Optional { _1 : a, _2 : b } →
        { _1 : Optional a, _2 : Optional b }
    = λ(a : Type) →
      λ(b : Type) →
      λ(xs : Optional { _1 : a, _2 : b }) →
        { _1 =
            merge
              { Some = λ(x : { _1 : a, _2 : b }) → Some x._1, None = None a }
              xs
        , _2 =
            merge
              { Some = λ(x : { _1 : a, _2 : b }) → Some x._2, None = None b }
              xs
        }

let example0 =
        assert
      :   unzip Text Bool (Some { _1 = "ABC", _2 = True })
        ≡ { _1 = Some "ABC", _2 = Some True }

let example1 =
        assert
      :   unzip Text Bool (None { _1 : Text, _2 : Bool })
        ≡ { _1 = None Text, _2 = None Bool }

in  unzip
