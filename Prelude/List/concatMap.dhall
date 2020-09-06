{-|
Transform a list by applying a function to each element and flattening the
results
-}
let concatMap
    : ∀(a : Type) → ∀(b : Type) → (a → List b) → List a → List b
    = λ(a : Type) →
      λ(b : Type) →
      λ(f : a → List b) →
      λ(xs : List a) →
        List/build
          b
          ( λ(list : Type) →
            λ(cons : b → list → list) →
              List/fold a xs list (λ(x : a) → List/fold b (f x) list cons)
          )

let example0 =
        assert
      :   concatMap Natural Natural (λ(n : Natural) → [ n, n ]) [ 2, 3, 5 ]
        ≡ [ 2, 2, 3, 3, 5, 5 ]

let example1 =
        assert
      :   concatMap
            Natural
            Natural
            (λ(n : Natural) → [ n, n ])
            ([] : List Natural)
        ≡ ([] : List Natural)

in  concatMap
