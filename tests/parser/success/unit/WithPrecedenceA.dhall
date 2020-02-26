{-  The purpose of this test is to illustrate that `with` has higher precedence
    than function application, since it's probably not going to be immediately
    intuitive to people which one should be higher precedence
-}
\(f : { a : Natural } -> Bool) -> f { a = 1 } with { a = 2 }
