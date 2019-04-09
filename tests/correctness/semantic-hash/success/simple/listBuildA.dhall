{ example0 =
    List/build
    Bool
    (   λ(list : Type)
      → λ(cons : Bool → list → list)
      → λ(nil : list)
      → cons True (cons False nil)
    )
, example1 =
    List/build
    Bool
    (λ(x : Type) → λ(x : Bool → x → x) → λ(x : x@1) → x@1 True (x@1 False x))
, example2 =
      λ(id : ∀(a : Type) → a → a)
    → List/build
      Bool
      (   λ(list : Type)
        → λ(cons : Bool → list → list)
        → λ(nil : list)
        → id list (cons True (cons False nil))
      )
}
