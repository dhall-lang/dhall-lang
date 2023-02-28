--| Transform a list by applying a function to each element with its index
let List/map = ./map.dhall

let List/WithIndex = ./WithIndex.dhall

let List/mapWithIndex
    : ∀(a : Type) → ∀(b : Type) → (List/WithIndex a → b) → List a → List b
    = λ(a : Type) →
      λ(b : Type) →
      λ(f : List/WithIndex a → b) →
      λ(list : List a) →
        List/map (List/WithIndex a) b f (List/indexed a list)

let List/empty = ./empty.dhall

let List/replicate = ./replicate.dhall

let example0 =
        assert
      :   List/mapWithIndex
            Text
            (List Text)
            ( λ(indexedText : List/WithIndex Text) →
                List/replicate indexedText.index Text indexedText.value
            )
            [ "A", "B", "C" ]
        ≡ [ List/empty Text, [ "B" ], [ "C", "C" ] ]

in  List/mapWithIndex
