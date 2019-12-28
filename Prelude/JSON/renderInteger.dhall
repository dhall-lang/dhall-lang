{- Render an `Integer` value as a `JSON number`, according to the JSON
   standard, in which a number may not start with a plus sign (`+`).
-}

let renderInteger
    : Integer → Text
    =   λ(integer : Integer)
      → let `Optional/fold` =
                ../Optional/fold sha256:62139ff410ca84302acebe763a8a1794420dd472d907384c7fb80df2a2180302
              ? ../Optional/fold

        let Integer/toNatural =
                ../Integer/toNatural sha256:4d128730d74e7f832e53873cb5204aa91b79758be5ce4e1aa991fe1951304a0e
              ? ../Integer/toNatural

        let `Natural/show` =
                ../Natural/show sha256:684ed560ad86f438efdea229eca122c29e8e14f397ed32ec97148d578ca5aa21
              ? ../Natural/show

        let `Integer/show` =
                ../Integer/show sha256:ecf8b0594cd5181bc45d3b7ea0d44d3ba9ad5dac6ec17bb8968beb65f4b1baa9
              ? ../Integer/show

        in  `Optional/fold`
              Natural
              (Integer/toNatural integer)
              Text
              (λ(natural : Natural) → `Natural/show` natural)
              (`Integer/show` integer)

let positive = assert : renderInteger +1 ≡ "1"

let zero = assert : renderInteger +0 ≡ "0"

let negative = assert : renderInteger -1 ≡ "-1"

in  renderInteger
