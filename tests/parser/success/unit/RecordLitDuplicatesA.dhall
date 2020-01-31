{-  The purpose of this test is to illustrate that duplicate fields are
    syntactic sugar that does not survive the parsing stage.  The underlying
    expression is actually represented and encoded as:

    { x = { y = 1 } ∧ { z = 1 } }
-}
{ x = { y = 1 }, x = { z = 1 } }
