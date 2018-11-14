{ example0 =
    Natural/build
    (   λ(natural : Type)
      → λ(succ : natural → natural)
      → λ(zero : natural)
      → succ zero
    )
, example1 =
    Natural/build (λ(x : Type) → λ(x : x → x) → λ(x : x@1) → x@1 x)
, example2 =
      λ(id : ∀(a : Type) → a → a)
    → Natural/build
      (   λ(natural : Type)
        → λ(succ : natural → natural)
        → λ(zero : natural)
        → id natural (succ zero)
      )
}
