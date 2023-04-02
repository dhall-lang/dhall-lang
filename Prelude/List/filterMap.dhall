--| Transform a list by applying a function to each element and omitting None results.
let List/concatMap =
        missing
          sha256:3b2167061d11fda1e4f6de0522cbe83e0d5ac4ef5ddf6bb0b2064470c5d3fb64
      ? ./concatMap.dhall

let Optional/toList =
        missing
          sha256:d78f160c619119ef12389e48a629ce293d69f7624c8d016b7a4767ab400344c4
      ? ../Optional/toList.dhall

let filterMap
    : ∀(a : Type) → ∀(b : Type) → (a → Optional b) → List a → List b
    = λ(a : Type) →
      λ(b : Type) →
      λ(f : a → Optional b) →
        List/concatMap a b (λ(x : a) → Optional/toList b (f x))

let example0 =
        assert
      :   filterMap
            (List Natural)
            Natural
            (List/head Natural)
            [ [ 1 ], [] : List Natural, [ 3, 4 ] ]
        ≡ [ 1, 3 ]

let example1 =
      let Example = < Left : Bool | Right : Natural >

      in    assert
          :   filterMap
                Example
                Natural
                ( λ(x : Example) →
                    merge
                      { Left = λ(_ : Bool) → None Natural
                      , Right = λ(n : Natural) → Some n
                      }
                      x
                )
                [ Example.Left False, Example.Right 2, Example.Left True ]
            ≡ [ 2 ]

in  filterMap
