--| Concatenate a `List` of `List`s into a single `List`
let concat
    : ∀(a : Type) → List (List a) → List a
    = λ(a : Type) →
      λ(xss : List (List a)) →
        List/build
          a
          ( λ(list : Type) →
            λ(cons : a → list → list) →
            λ(nil : list) →
              List/fold
                (List a)
                xss
                list
                (λ(xs : List a) → λ(ys : list) → List/fold a xs list cons ys)
                nil
          )

let example0 =
        assert
      :   concat Natural [ [ 0, 1, 2 ], [ 3, 4 ], [ 5, 6, 7, 8 ] ]
        ≡ [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ]

let example1 =
        assert
      :   concat
            Natural
            [ [] : List Natural, [] : List Natural, [] : List Natural ]
        ≡ ([] : List Natural)

let example2 =
      assert : concat Natural ([] : List (List Natural)) ≡ ([] : List Natural)

in  concat
