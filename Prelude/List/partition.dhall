{-|
`partition` divides a `List` of elements into those that satisfy a predicate
and those that do not
-}
let Partition
    : Type → Type
    = λ(a : Type) → { true : List a, false : List a }

let partition
    : ∀(a : Type) → (a → Bool) → List a → Partition a
    = λ(a : Type) →
      λ(f : a → Bool) →
      λ(xs : List a) →
        List/fold
          a
          xs
          (Partition a)
          ( λ(x : a) →
            λ(p : Partition a) →
              if    f x
              then  { true = [ x ] # p.true, false = p.false }
              else  { true = p.true, false = [ x ] # p.false }
          )
          { true = [] : List a, false = [] : List a }

let example0 =
        assert
      :   partition Natural Natural/even [ 0, 1, 2, 3 ]
        ≡ { true = [ 0, 2 ], false = [ 1, 3 ] }

in  partition
