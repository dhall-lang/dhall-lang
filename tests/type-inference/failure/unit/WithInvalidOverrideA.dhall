{-  The `with` keyword cannot "descend" past a field that is not a record

    In the following example, the `a` field is not a record, therefore there is
    no possibility of adding or overriding `a` with an inner `b` field
-}
{ a = 1 } with a.b = 2
