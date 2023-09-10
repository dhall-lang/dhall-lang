--| Compose two functions into one.
let compose
    : ∀(a : Type) → ∀(b : Type) → ∀(c : Type) → (a → b) → (b → c) → a → c
    = λ(a : Type) →
      λ(b : Type) →
      λ(c : Type) →
      λ(f : a → b) →
      λ(g : b → c) →
      λ(x : a) →
        g (f x)

let example0 =
        assert
      :   compose Natural Natural Bool (λ(n : Natural) → n + n) Natural/even 3
        ≡ True

in  compose
