--| Transform a list by applying a function to each element with its index
let List/map =
        ./map.dhall
          sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680
      ? ./map.dhall

let List/WithIndex =
        ./WithIndex.dhall
          sha256:fd62c3fb18b64bde754288b7e1243115a51dd3b6ab2294b1c99fc7207f53fc78
      ? ./WithIndex.dhall

let List/mapWithIndex
    : ∀(a : Type) → ∀(b : Type) → (List/WithIndex a → b) → List a → List b
    = λ(a : Type) →
      λ(b : Type) →
      λ(f : List/WithIndex a → b) →
      λ(list : List a) →
        List/map (List/WithIndex a) b f (List/indexed a list)

let List/empty =
        ./empty.dhall
          sha256:b2f561f35098c457353723c93a22bd5de28d26ecc5370814bef9dfda421e0147
      ? ./empty.dhall

let List/replicate =
        ./replicate.dhall
          sha256:d4250b45278f2d692302489ac3e78280acb238d27541c837ce46911ff3baa347
      ? ./replicate.dhall

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
