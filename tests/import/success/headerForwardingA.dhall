{- This test verifies that header-forwarding works correctly for relative
   imports within the same domain

   `test.dhall-lang.org/foo` returns `./bar` and `test.dhall-lang.org/foo`
   returns `True`, and both URLs reject all requests without a `Test` header.

   This test requires that the initial import to
   `http://localhost:18080/foo` forwards the `Test` header
   to the transitive relative import of `http://localhost:18080/bar` in
   order to succeed.
-}
http://localhost:18080/foo using (toMap { Test = "Example" })
