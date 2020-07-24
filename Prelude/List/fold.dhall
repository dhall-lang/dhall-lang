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
            Natural
            (λ(x : Natural) → λ(y : Natural) → x + y)
            0
        ≡ 10

let example1 =
        assert
      :   ( λ(nil : Natural) →
              fold
                Natural
                [ 2, 3, 5 ]
                Natural
                (λ(x : Natural) → λ(y : Natural) → x + y)
                nil
          )
        ≡ (λ(nil : Natural) → 2 + (3 + (5 + nil)))

let example2 =
        assert
      :   ( λ(list : Type) →
            λ(cons : Natural → list → list) →
            λ(nil : list) →
              fold Natural [ 2, 3, 5 ] list cons nil
          )
        ≡ ( λ(list : Type) →
            λ(cons : Natural → list → list) →
            λ(nil : list) →
              cons 2 (cons 3 (cons 5 nil))
          )

in  fold
