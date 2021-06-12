--| Truncate a list to the first `n` elements
let Natural/lessThan =
        ../Natural/lessThan.dhall
          sha256:3381b66749290769badf8855d8a3f4af62e8de52d1364d838a9d1e20c94fa70c
      ? ../Natural/lessThan.dhall

let take
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
              if Natural/lessThan x.index n then [ x.value ] # xs else xs
          )
          ([] : List a)

let example = assert : take 2 Natural [ 2, 3, 5 ] ≡ [ 2, 3 ]

let example = assert : take 5 Natural [ 2, 3, 5 ] ≡ [ 2, 3, 5 ]

in  take
