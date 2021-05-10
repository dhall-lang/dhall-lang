{-|
`fold` is the primitive function for consuming `List`s

If you treat the list `[ x, y, z ]` as `cons x (cons y (cons z nil))`, then a
`fold` just replaces each `cons` and `nil` with something else
-}
let fold
    : ∀(a : Type) →
      List a →
      ∀(list : Type) →
      ∀(cons : a → list → list) →
      ∀(nil : list) →
        list
    = List/fold

let example0 =
        assert
      :   fold
            Natural
            [ 2, 3, 5 ]
            Text
            (λ(x : Natural) → λ(y : Text) → Natural/show x ++ y)
            "0"
        ≡ "2350"

let example1 =
      λ(nil : Text) →
          assert
        :   fold
              Natural
              [ 2, 3, 5 ]
              Text
              (λ(x : Natural) → λ(y : Text) → Natural/show x ++ y)
              nil
          ≡ "2" ++ ("3" ++ ("5" ++ nil))

let example2 =
      λ(cons : Natural → Text → Text) →
      λ(nil : Text) →
          assert
        : fold Natural [ 2, 3, 5 ] Text cons nil ≡ cons 2 (cons 3 (cons 5 nil))

in  fold
