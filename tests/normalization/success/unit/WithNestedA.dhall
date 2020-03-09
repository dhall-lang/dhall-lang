{-  This test verifies that an implementation handles `with` expressions that
    update nested labels correctly

    A common mistake an implementation might make is to produce the following
    result:

        { a.c = 2 }

    ... due to the nested update clobbering the inner record.  A compliant
    implementation extends inner records.
-}
{ a.b = 1 } with a.c = 2
