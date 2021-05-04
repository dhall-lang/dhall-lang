{-|
Lowercase all ASCII characters
-}
let lowerASCII
    : Text → Text
    = List/fold
        { before : Text, after : Text }
        [ { before = "A", after = "a" }
        , { before = "B", after = "b" }
        , { before = "C", after = "c" }
        , { before = "D", after = "d" }
        , { before = "E", after = "e" }
        , { before = "F", after = "f" }
        , { before = "G", after = "g" }
        , { before = "H", after = "h" }
        , { before = "I", after = "i" }
        , { before = "J", after = "j" }
        , { before = "K", after = "k" }
        , { before = "L", after = "l" }
        , { before = "M", after = "m" }
        , { before = "N", after = "n" }
        , { before = "O", after = "o" }
        , { before = "P", after = "p" }
        , { before = "Q", after = "q" }
        , { before = "R", after = "r" }
        , { before = "S", after = "s" }
        , { before = "T", after = "t" }
        , { before = "U", after = "u" }
        , { before = "V", after = "v" }
        , { before = "W", after = "w" }
        , { before = "X", after = "x" }
        , { before = "Y", after = "y" }
        , { before = "Z", after = "z" }
        ]
        Text
        ( λ(replacement : { before : Text, after : Text }) →
            Text/replace replacement.before replacement.after
        )

let example0 = assert : lowerASCII "ABCdef" ≡ "abcdef"

let -- This does not lowercase Unicode
    example1 =
      assert : "Á" ≡ "Á"

in  lowerASCII
