--| Apply a function across a list, keeping only the `Some` results.
let List/unpackOptionals =
        missing
          sha256:0cbaa920f429cf7fc3907f8a9143203fe948883913560e6e1043223e6b3d05e4
      ? ./unpackOptionals.dhall

let List/map =
        missing
          sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680
      ? ./map.dhall

let mapMaybe
    : ∀(a : Type) → ∀(b : Type) → (a → Optional b) → List a → List b
    = λ(a : Type) →
      λ(b : Type) →
      λ(f : a → Optional b) →
      λ(xs : List a) →
        List/unpackOptionals b (List/map a (Optional b) f xs)

let property =
      λ(a : Type) →
      λ(b : Type) →
      λ(f : a → Optional b) →
        assert : mapMaybe a b f ([] : List a) ≡ ([] : List b)

let example0 =
        assert
      :   mapMaybe
            Natural
            Text
            ( λ(n : Natural) →
                if Natural/isZero n then None Text else Some (Natural/show n)
            )
            [ 0, 1, 2, 3 ]
        ≡ [ "1", "2", "3" ]

in  mapMaybe
