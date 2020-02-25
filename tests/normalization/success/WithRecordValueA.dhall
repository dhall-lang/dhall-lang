{-  This test verifies that an implementation does not treat the update clause
    of a `with` keyword the same as a real record.

    Specifically, if the value in an update is a record then the `with` does not
    descend into that record for update purposes.

    Compare this to the `WithNested` test to see how the dotted-label syntax is
    given special treatment for the update syntax, unlike with normal records.
-}
{ a.b = 1 } with { a = { c = 2 } }
