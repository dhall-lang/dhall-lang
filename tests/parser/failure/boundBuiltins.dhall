{-
    Builtin names are disallowed in bound variables.
    The grammar doesn't explicitly disallow this, but the implementation should
    refuse it. See the comments above the `nonreserved-label` rule.
-}
let Bool : Natural = 1 in Bool
