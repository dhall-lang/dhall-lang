{- This test verifies that header-forwarding works correctly for relative
   imports within the same domain

   `test.dhall-lang.org/foo` returns `./bar` and `test.dhall-lang.org/foo`
   returns `True`, and both URLs reject all requests without a `Test` header.

   This test requires that the initial import to
   `https://test.dhall-lang.org/foo` forwards the `Test` header
   to the transitive relative import of `https://test.dhall-lang.org/bar` in
   order to succeed.
-}
https://test.dhall-lang.org/foo using (toMap { Test = "Example" })
