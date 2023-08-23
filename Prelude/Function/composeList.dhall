{-| Composes a list of functions starting from the left

`composeList a [ f, g, h ]` is equivalent to `\(x : a) -> h (g (f x))`
-}
let compose = ./compose.dhall

let identity = ./identity.dhall

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
