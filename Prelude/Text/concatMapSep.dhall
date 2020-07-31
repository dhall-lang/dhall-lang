{-|
Transform each value in a `List` to `Text` and then concatenate them with a
separator in between each value
-}
let Status = < Empty | NonEmpty : Text >

let concatMapSep
    : ∀(separator : Text) → ∀(a : Type) → (a → Text) → List a → Text
    = λ(separator : Text) →
      λ(a : Type) →
      λ(f : a → Text) →
      λ(elements : List a) →
        let status =
              List/fold
                a
                elements
                Status
                ( λ(x : a) →
                  λ(status : Status) →
                    merge
                      { Empty = Status.NonEmpty (f x)
                      , NonEmpty =
                          λ(result : Text) →
                            Status.NonEmpty (f x ++ separator ++ result)
                      }
                      status
                )
                Status.Empty

        in  merge { Empty = "", NonEmpty = λ(result : Text) → result } status

let example0 =
      assert : concatMapSep ", " Natural Natural/show [ 0, 1, 2 ] ≡ "0, 1, 2"

let example1 =
      assert : concatMapSep ", " Natural Natural/show ([] : List Natural) ≡ ""

in  concatMapSep
