{-
Render a `Text` literal as its own representation as Dhall source code (i.e. a
double-quoted string literal)
-}
let show
    : Text → Text
    = Text/show

let example0 = assert : show "ABC" ≡ "\"ABC\""

let example1 =
        assert
      :   show
            ''
            ${"\u0000"} $ \ 
            ${" "}☺''
        ≡ "\"\\u0000 \\u0024 \\\\ \\n ☺\""

in  show
