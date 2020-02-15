{-  At one point the Haskell implementation permitted this idiom, which was not
    standards-compliant

    The problem with the following expression is that the argument to `using`
    is parsed as a `completionExpression`, which means that any use of `toMap`
    within a `using` clause must be parenthesized
-}
https://example.com using toMap { Foo = "Bar" }
