{-
    This test has free variables, so it doesn't typecheck
-}
\(x: Bool) -> \(x: Bool) -> [x, x@0, x@1, x@2, x@3]
