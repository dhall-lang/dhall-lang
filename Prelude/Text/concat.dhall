--| Concatenate all the `Text` values in a `List`
let concat
    : List Text → Text
    = λ(xs : List Text) →
        List/fold Text xs Text (λ(x : Text) → λ(y : Text) → x ++ y) ""

let example0 = assert : concat [ "ABC", "DEF", "GHI" ] ≡ "ABCDEFGHI"

let example1 = assert : concat ([] : List Text) ≡ ""

in  concat
