{-|
`foldLeft` is like `List/fold` except that the accumulation starts from the left

If you treat the list `[ x, y, z ]` as `cons (cons (cons nil x) y) z`, then
`foldLeft` just replaces each `cons` and `nil` with something else
-}
let foldLeft
    : ∀(a : Type) →
      List a →
      ∀(list : Type) →
      ∀(cons : list → a → list) →
      ∀(nil : list) →
        list
    = λ(a : Type) →
      λ(xs : List a) →
      λ(list : Type) →
      λ(cons : list → a → list) →
      λ(nil : list) →
        List/fold
          a
          xs
          (list → list)
          (λ(x : a) → λ(f : list → list) → λ(l : list) → f (cons l x))
          (λ(l : list) → l)
          nil

let example0 =
        assert
      :   foldLeft
            Natural
            [ 2, 3, 5 ]
            Text
            (λ(x : Text) → λ(y : Natural) → x ++ Natural/show y)
            "0"
        ≡ "0235"

let example1 =
        assert
      :   ( λ(nil : Text) →
              foldLeft
                Natural
                [ 2, 3, 5 ]
                Text
                (λ(x : Text) → λ(y : Natural) → x ++ Natural/show y)
                nil
          )
        ≡ (λ(nil : Text) → nil ++ "2" ++ "3" ++ "5")

let example2 =
        assert
      :   ( λ(cons : Text → Natural → Text) →
            λ(nil : Text) →
              foldLeft Natural [ 2, 3, 5 ] Text cons nil
          )
        ≡ ( λ(cons : Text → Natural → Text) →
            λ(nil : Text) →
              cons (cons (cons nil 2) 3) 5
          )

in  foldLeft
