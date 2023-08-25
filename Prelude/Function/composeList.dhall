{-| Composes a list of functions starting from the left

`composeList a [ f, g, h ]` is equivalent to `\(x : a) -> h (g (f x))`
-}
let compose =
        missing
          sha256:65ad8bbea530b3d8968785a7cf4a9a7976b67059aa15e3b61fcba600a40ae013
      ? ./compose.dhall

let identity =
        missing
          sha256:f78b96792b459cb664f41c6119bd8897dd04353a3343521d436cd82ad71cb4d4
      ? ./identity.dhall

let composeList
    : ∀(a : Type) → List (a → a) → a → a
    = λ(A : Type) →
      λ(functions : List (A → A)) →
        List/fold (A → A) functions (A → A) (compose A A A) (identity A)

let example0 =
        assert
      :   composeList
            Natural
            [ λ(x : Natural) → x * 2, λ(x : Natural) → x + 1 ]
            1
        ≡ 3

in  composeList
