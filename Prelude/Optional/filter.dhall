--| Only keep an `Optional` element if the supplied function returns `True`
let filter
    : ∀(a : Type) → (a → Bool) → Optional a → Optional a
    = λ(a : Type) →
      λ(f : a → Bool) →
      λ(xs : Optional a) →
        ( λ(a : Type) →
          λ ( build
            : ∀(optional : Type) →
              ∀(some : a → optional) →
              ∀(none : optional) →
                optional
            ) →
            build (Optional a) (λ(x : a) → Some x) (None a)
        )
          a
          ( λ(optional : Type) →
            λ(some : a → optional) →
            λ(none : optional) →
              merge
                { Some = λ(x : a) → if f x then some x else none, None = none }
                xs
          )

let example0 = assert : filter Natural Natural/even (Some 2) ≡ Some 2

let example1 = assert : filter Natural Natural/odd (Some 2) ≡ None Natural

in  filter
