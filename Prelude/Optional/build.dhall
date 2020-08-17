--| `build` is the inverse of `fold`
let build
    : ∀(a : Type) →
      ( ∀(optional : Type) →
        ∀(some : a → optional) →
        ∀(none : optional) →
          optional
      ) →
        Optional a
    = λ(a : Type) →
      λ ( build
        : ∀(optional : Type) →
          ∀(some : a → optional) →
          ∀(none : optional) →
            optional
        ) →
        build (Optional a) (λ(x : a) → Some x) (None a)

let example0 =
        assert
      :   build
            Natural
            ( λ(optional : Type) →
              λ(some : Natural → optional) →
              λ(none : optional) →
                some 1
            )
        ≡ Some 1

let example1 =
        assert
      :   build
            Natural
            ( λ(optional : Type) →
              λ(some : Natural → optional) →
              λ(none : optional) →
                none
            )
        ≡ None Natural

in  build
