--| Transform each value in a `List` into `Text` and concatenate the result
let concatMap
    : ∀(a : Type) → (a → Text) → List a → Text
    = λ(a : Type) →
      λ(f : a → Text) →
      λ(xs : List a) →
        List/fold a xs Text (λ(x : a) → λ(y : Text) → f x ++ y) ""

let example0 =
        assert
      :   concatMap Natural (λ(n : Natural) → "${Natural/show n} ") [ 0, 1, 2 ]
        ≡ "0 1 2 "

let example1 =
        assert
      :   concatMap
            Natural
            (λ(n : Natural) → "${Natural/show n} ")
            ([] : List Natural)
        ≡ ""

in  concatMap
