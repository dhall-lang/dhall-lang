    let f =
            λ(o : Optional Text)
          → Optional/fold Text o Natural (λ(j : Text) → 1) 2

in  { example0 = f ([ "foo" ] : Optional Text)
    , example1 = f ([] : Optional Text)
    }
