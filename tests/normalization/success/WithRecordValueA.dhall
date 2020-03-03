{-  This test illustrates that `with a = { c = 2 }` is not the same thing as
    `with a.c = 2`:

    * `with a = { c = 2 }` overrides the entire value of the field `a` with a new
      record containing only `c = 2`

    * `with a.c = 2` extends or updates the record stored underneath `a` to
      set the field `c` to `2`.

    Compare this to the `WithNested` test, which contains the `a.c = 2` case
-}
{ a.b = 1 } with a = { c = 2 }
