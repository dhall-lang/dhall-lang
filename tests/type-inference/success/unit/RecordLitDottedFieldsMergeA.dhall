{-  This example verifies that a Dhall interpreter correctly handles a corner
    case that other languages sometimes misbehave on

    The expression should desugar like this:

        λ(r : { z : Natural }) → { x.y = 1, x = r }

        λ(r : { z : Natural }) → { x = { y = 1 }, x = r }

        λ(r : { z : Natural }) → { x = { y = 1 } ∧ r }

    ... which type-checks since `{ y = 1 }` and `r` do not collide
-}
λ(r : { z : Natural }) → { x.y = 1, x = r }
