{- This test verifies that header-forwarding works correctly for relative
   imports within the same domain

   `test.dhall-lang.org/foo` returns `./bar` and `test.dhall-lang.org/foo`
   returns `True`, and both URLs reject all requests without a `Test` header.

   This test requires that the initial import to
   `https://localhost:18443/foo` forwards the `Test` header
   to the transitive relative import of `https://localhost:18443/bar` in
   order to succeed.
-}
https://localhost:18443/foo using (toMap { Test = "Example" })
