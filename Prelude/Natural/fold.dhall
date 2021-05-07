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

let example0 = assert : fold 3 Text (λ(x : Text) → "0" ++ x) "1" ≡ "0001"

let example1 =
        assert
      :   (λ(zero : Text) → fold 3 Text (λ(x : Text) → "0" ++ x) zero)
        ≡ (λ(zero : Text) → "0" ++ ("0" ++ ("0" ++ zero)))

let example2 =
        assert
      :   (λ(succ : Text → Text) → λ(zero : Text) → fold 3 Text succ zero)
        ≡ (λ(succ : Text → Text) → λ(zero : Text) → succ (succ (succ zero)))

in  fold
