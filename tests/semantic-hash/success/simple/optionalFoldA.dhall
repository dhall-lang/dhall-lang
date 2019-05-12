    let f =
            λ(o : Optional Text)
          → Optional/fold Text o Natural (λ(j : Text) → 1) 2

in  { example0 = f (Some "foo")
    , example1 = f (None Text)
    }
