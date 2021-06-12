{-|
Return a Text with the number of spaces specified.

This function is particularly helpful when trying to generate Text where
whitespace is significant, i.e. with nested indentation.
-}
let replicate =
        ./replicate.dhall
          sha256:1b398b1d464b3a6c7264a690ac3cacb443b5683b43348c859d68e7c2cb925c4f
      ? ./replicate.dhall

let spaces
    : Natural → Text
    = λ(a : Natural) → replicate a " "

let example0 = assert : spaces 1 ≡ " "

let example1 = assert : spaces 0 ≡ ""

in  spaces
