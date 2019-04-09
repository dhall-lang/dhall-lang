{ example0 =
    Optional/build
    Natural
    (   λ(optional : Type)
      → λ(just : Natural → optional)
      → λ(nothing : optional)
      → just 1
    )
, example1 =
    Optional/build
    Integer
    (λ(optional : Type) → λ(x : Integer → optional) → λ(x : optional) → x@1 +1)
, example2 =
      λ(id : ∀(a : Type) → a → a)
    → Optional/build
      Bool
      (   λ(optional : Type)
        → λ(just : Bool → optional)
        → λ(nothing : optional)
        → id optional (just True)
      )
, example3 =
      λ(a : Type)
    → λ(x : a)
    → Optional/build
      a
      (   λ(optional : Type)
        → λ(just : a → optional)
        → λ(nothing : optional)
        → just x
      )
}
