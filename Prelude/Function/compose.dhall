--| Compose two functions into one.
let compose
    : ∀(a : Type) → ∀(b : Type) → ∀(c : Type) → (a → b) → (b → c) → a → c
    = λ(A : Type) →
      λ(B : Type) →
      λ(C : Type) →
      λ(f : A → B) →
      λ(g : B → C) →
      λ(x : A) →
        g (f x)

let example0 =
        assert
      :   compose Natural Natural Bool (λ(n : Natural) → n + n) Natural/even 3
        ≡ True

in  compose
