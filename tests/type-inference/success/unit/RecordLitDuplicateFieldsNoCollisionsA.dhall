{-  This type-checks because the expression desugars to:

        { x = { y = 0 } ∧ { z = 0 } }

    ... which in turn type-checks due to the absence of a field collision.

    This expression is analogous to the following common Nix/TOML/Cue idiom,
    which we would like to support:

        { x.y = 0, x.z = 0 }
-}
{ x = { y = 0 }, x = { z = 0 } }
