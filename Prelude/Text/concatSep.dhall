--| Concatenate a `List` of `Text` values with a separator in between each value
let Status = < Empty | NonEmpty : Text >

let concatSep
    : ∀(separator : Text) → ∀(elements : List Text) → Text
    = λ(separator : Text) →
      λ(elements : List Text) →
        let status =
              List/fold
                Text
                elements
                Status
                ( λ(element : Text) →
                  λ(status : Status) →
                    merge
                      { Empty = Status.NonEmpty element
                      , NonEmpty =
                          λ(result : Text) →
                            Status.NonEmpty (element ++ separator ++ result)
                      }
                      status
                )
                Status.Empty

        in  merge { Empty = "", NonEmpty = λ(result : Text) → result } status

let example0 = assert : concatSep ", " [ "ABC", "DEF", "GHI" ] ≡ "ABC, DEF, GHI"

let example1 = assert : concatSep ", " ([] : List Text) ≡ ""

in  concatSep
