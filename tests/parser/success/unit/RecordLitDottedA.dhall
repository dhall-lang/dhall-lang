{-  The purpose of this test is to illustrate that dotted fields are syntactic
    sugar that does not survive the parsing stage.  The underlying expression is
    actually represented and encoded as:

    { x = { y = { z = 1 } } }
-}
{ x.y.z = 1 }
