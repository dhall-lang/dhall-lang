{-
Replace a section of `Text` with another inside a `Text` literal.
-}
let replace
    : ∀(needle : Text) → ∀(replacement : Text) → ∀(haystack : Text) → Text
    = Text/replace

let example0 = assert : replace "-" "_" "foo-bar" ≡ "foo_bar"

let example1 = assert : replace "💣" "💥" "💣💣💣" ≡ "💥💥💥"

let example2 = assert : replace "👨" "👩" "👨‍👩‍👧‍👦" ≡ "👩‍👩‍👧‍👦"

in  replace
