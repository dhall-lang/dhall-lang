{-  The purpose of this test is to verify that an implementation correctly
    desugars the base case of a non-nested update.  Specifically, an
    implementation should not use an intermediate `let` when desugaring this
    base case.
-}
{ a = 1 } with a = 2
