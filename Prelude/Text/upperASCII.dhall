{-|
Uppercase all ASCII characters
-}
let upperASCII
    : Text → Text
    = List/fold
        (Text → Text)
        [ Text/replace "a" "A"
        , Text/replace "b" "B"
        , Text/replace "c" "C"
        , Text/replace "d" "D"
        , Text/replace "e" "E"
        , Text/replace "f" "F"
        , Text/replace "g" "G"
        , Text/replace "h" "H"
        , Text/replace "i" "I"
        , Text/replace "j" "J"
        , Text/replace "k" "K"
        , Text/replace "l" "L"
        , Text/replace "m" "M"
        , Text/replace "n" "N"
        , Text/replace "o" "O"
        , Text/replace "p" "P"
        , Text/replace "q" "Q"
        , Text/replace "r" "R"
        , Text/replace "s" "S"
        , Text/replace "t" "T"
        , Text/replace "u" "U"
        , Text/replace "v" "V"
        , Text/replace "w" "W"
        , Text/replace "x" "X"
        , Text/replace "y" "Y"
        , Text/replace "z" "Z"
        ]
        Text
        (λ(replacement : Text → Text) → replacement)

let example0 = assert : upperASCII "ABCdef" ≡ "ABCDEF"

let -- This does not uppercase Unicode
    example1 =
      assert : upperASCII "á" ≡ "á"

in  upperASCII
