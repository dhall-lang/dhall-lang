{-  The purpose of this test is to illustrate that function application has
    higher precedence than `with` so that chained with expressions parse
    correctly

    The following expression should parse as:

        ({ a = Some 1 } with a = Some 2) with a = Some 3

    ... and not parse as:

        { a = Some 1 } with a = (Some 2 with a = Some 3)
-}
{ a = Some 1 } with a = Some 2 with a = Some 3
