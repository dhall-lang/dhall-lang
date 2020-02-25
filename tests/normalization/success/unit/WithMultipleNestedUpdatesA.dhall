{-  This test protects against a likely implementation mistake which would return
    the following incorrect result:

        { a = { b = 1, d = 3 } }

    This can happen if an implementation incorrectly desugars the code to
    something that is equivalent to:

        let r = { a.b = 1 }

        in  r // { a = r.a // { b = 1 } }
              // { a = r.a // { d = 3 } }

    ... when the correct behavior is to desugar to something equivalent to:

        let r0 = { a.b = 1 }

        let r1 = r0 // { a = r0.a // { b = 1 } }

        in  r1 // { a = r1.a // { d = 3 } }

    In other words, updates have to be properly chained so that latter updates
    don't mistakenly delete older updates.  The standard covers this by
    specifying that multiple updates are equivalent to chaining the `with`
    keyword, which produces the correct behavior.
-}
{ a.b = 1 } with { a.c = 2, a.d = 3 }
