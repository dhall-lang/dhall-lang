{-|
Render an `Integer` value as a `JSON number`, according to the JSON standard, in
which a number may not start with a plus sign (`+`).
-}
let Integer/nonNegative =
        ../Integer/nonNegative.dhall
          sha256:b463373f070df6b1c8c7082051e0810fee38b360bab35256187c8c2b6af5c663
      ? ../Integer/nonNegative.dhall

let renderInteger
    : Integer → Text
    = λ(integer : Integer) →
        if    Integer/nonNegative integer
        then  Natural/show (Integer/clamp integer)
        else  Integer/show integer

let positive = assert : renderInteger +1 ≡ "1"

let zero = assert : renderInteger +0 ≡ "0"

let negative = assert : renderInteger -1 ≡ "-1"

in  renderInteger
