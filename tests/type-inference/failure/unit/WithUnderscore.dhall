{-  The purpose of this test is to verify that an implementation correctly
    desugars `with` in a way that avoids capture of variables named `_`.
    Specifically, a well-behaved implementation will reject the following program
-}
{ a.b = 1 } with a.c = _
