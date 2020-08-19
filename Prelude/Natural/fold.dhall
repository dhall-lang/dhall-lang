{-|
`fold` is the primitive function for consuming `Natural` numbers

If you treat the number `3` as `succ (succ (succ zero))` then a `fold` just
replaces each `succ` and `zero` with something else
-}
let fold
    : Natural →
      ∀(natural : Type) →
      ∀(succ : natural → natural) →
      ∀(zero : natural) →
        natural
    = Natural/fold

let example0 = assert : fold 3 Natural (λ(x : Natural) → 5 * x) 1 ≡ 125

let example1 =
        assert
      :   (λ(zero : Natural) → fold 3 Natural (λ(x : Natural) → 5 * x) zero)
        ≡ (λ(zero : Natural) → 5 * (5 * (5 * zero)))

let example2 =
        assert
      :   ( λ(natural : Type) →
            λ(succ : natural → natural) →
            λ(zero : natural) →
              fold 3 natural succ zero
          )
        ≡ ( λ(natural : Type) →
            λ(succ : natural → natural) →
            λ(zero : natural) →
              succ (succ (succ zero))
          )

in  fold
