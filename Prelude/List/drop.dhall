--| Remove first `n` elements of a list
let Natural/greaterThanEqual =
        ../Natural/greaterThanEqual.dhall
          sha256:30ebfab0febd7aa0ccccfdf3dc36ee6d50f0117f35dd4a9b034750b7e885a1a4
      ? ../Natural/greaterThanEqual.dhall

let drop
    : ∀(n : Natural) → ∀(a : Type) → List a → List a
    = λ(n : Natural) →
      λ(a : Type) →
      λ(xs : List a) →
        List/fold
          { index : Natural, value : a }
          (List/indexed a xs)
          (List a)
          ( λ(x : { index : Natural, value : a }) →
            λ(xs : List a) →
              if    Natural/greaterThanEqual x.index n
              then  [ x.value ] # xs
              else  xs
          )
          ([] : List a)

let example = assert : drop 2 Natural [ 2, 3, 5 ] ≡ [ 5 ]

let example = assert : drop 5 Natural [ 2, 3, 5 ] ≡ ([] : List Natural)

in  drop
