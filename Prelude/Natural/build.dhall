--| `build` is the inverse of `fold`
let build
    : ( ∀(natural : Type) →
        ∀(succ : natural → natural) →
        ∀(zero : natural) →
          natural
      ) →
        Natural
    = Natural/build

let example0 =
        assert
      :   build
            ( λ(natural : Type) →
              λ(succ : natural → natural) →
              λ(zero : natural) →
                succ (succ (succ zero))
            )
        ≡ 3

let example1 =
        assert
      :   build
            ( λ(natural : Type) →
              λ(succ : natural → natural) →
              λ(zero : natural) →
                zero
            )
        ≡ 0

in  build
