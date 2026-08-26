--| `max a b` returns the larger of `a` or `b`
let lessThanEqual =
        missing
          sha256:0e3dd9c7f319dc4d88898d7b1811dfbf7ea55d4fc997fd0b455d1b0647991fad
      ? ./lessThanEqual.dhall

let max
    : Natural → Natural → Natural
    = λ(a : Natural) → λ(b : Natural) → if lessThanEqual a b then b else a

let example0 = assert : max 1 2 ≡ 2

let example1 = assert : max 2 1 ≡ 2

let property0 = λ(n : Natural) → assert : max n n ≡ n

in  max
