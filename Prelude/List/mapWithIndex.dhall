--| Transform a list by applying a function to each element with its index
let List/map =
        ./map.dhall
          sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680
      ? ./map.dhall

let List/mapWithIndex
    : ∀(a : Type) → ∀(b : Type) → (Natural → a → b) → List a → List b
    = λ(a : Type) →
      λ(b : Type) →
      λ(f : Natural → a → b) →
      λ(xs : List a) →
        List/map
          { index : Natural, value : a }
          b
          (λ(i : { index : Natural, value : a }) → f i.index i.value)
          (List/indexed a xs)

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
            ( λ(index : Natural) →
              λ(value : Text) →
                List/replicate index Text value
            )
            [ "A", "B", "C" ]
        ≡ [ List/empty Text, [ "B" ], [ "C", "C" ] ]

in  List/mapWithIndex
