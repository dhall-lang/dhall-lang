{-|
Uppercase all ASCII characters

Note that this will also uppercase decomposed Unicode characters that contain
codepoints in the ASCII range
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

let -- This does not uppercase precomposed Unicode characters
    --
    -- • The `á` in the following example is U+00E1
    example1 =
      assert : upperASCII "á" ≡ "á"

let -- … but this does uppercase decomposed Unicode characters
    --
    -- • The `Á` in the following example is U+0041 U+0301
    -- • The `á` in the following example is U+0061 U+0301
    example1 =
      assert : upperASCII "á" ≡ "Á"

in  upperASCII
