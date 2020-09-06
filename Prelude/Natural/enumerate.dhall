{-|
Generate a list of numbers from `0` up to but not including the specified
number
-}
let enumerate
    : Natural → List Natural
    = λ(n : Natural) →
        List/build
          Natural
          ( λ(list : Type) →
            λ(cons : Natural → list → list) →
              List/fold
                { index : Natural, value : {} }
                ( List/indexed
                    {}
                    ( List/build
                        {}
                        ( λ(list : Type) →
                          λ(cons : {} → list → list) →
                            Natural/fold n list (cons {=})
                        )
                    )
                )
                list
                (λ(x : { index : Natural, value : {} }) → cons x.index)
          )

let example0 = assert : enumerate 10 ≡ [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]

let example1 = assert : enumerate 0 ≡ ([] : List Natural)

in  enumerate
