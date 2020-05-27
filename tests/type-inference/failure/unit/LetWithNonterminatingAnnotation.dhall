-- When you check if an inferred type is equivalent to an annotation,
-- you must alpha-beta-normalize both sides first.  But it is not safe
-- to beta-normalise an expression which hasn't first been
-- typechecked.
--
-- This test contains an annotation which doesn't typecheck, and
-- which, when beta-normalized, doesn't terminate.  It exists to
-- verify that implementations typecheck the annotation before
-- checking equivalence.
let a
    : (λ(x : Natural) → x x) (λ(x : Natural) → x x)
    = 3

in  5
