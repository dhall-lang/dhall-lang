{-|
Lowercase all ASCII characters
-}
let lowerASCII
    : Text → Text
    = List/fold
        (Text → Text)
        [ Text/replace "A" "a"
        , Text/replace "B" "b"
        , Text/replace "C" "c"
        , Text/replace "D" "d"
        , Text/replace "E" "e"
        , Text/replace "F" "f"
        , Text/replace "G" "g"
        , Text/replace "H" "h"
        , Text/replace "I" "i"
        , Text/replace "J" "j"
        , Text/replace "K" "k"
        , Text/replace "L" "l"
        , Text/replace "M" "m"
        , Text/replace "N" "n"
        , Text/replace "O" "o"
        , Text/replace "P" "p"
        , Text/replace "Q" "q"
        , Text/replace "R" "r"
        , Text/replace "S" "s"
        , Text/replace "T" "t"
        , Text/replace "U" "u"
        , Text/replace "V" "v"
        , Text/replace "W" "w"
        , Text/replace "X" "x"
        , Text/replace "Y" "y"
        , Text/replace "Z" "z"
        ]
        Text
        (λ(replacement : Text → Text) → replacement)

let example0 = assert : lowerASCII "ABCdef" ≡ "abcdef"

let -- This does not lowercase Unicode
    example1 =
      assert : "Á" ≡ "Á"

in  lowerASCII
