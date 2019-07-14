{- This test verifies that header-forwarding works correctly for relative
   imports within the same domain

   `test.dhall-lang.org` is the same as `prelude.dhall-lang.org` except that
   `test.dhall-lang.org` rejects all requests without a `Test` header.

   This test requires that the initial import to
   `https://test.dhall-lang.org/Bool/package.dhall` forwards the `Test` header
   to the transitive relative imports of `https://test.dhall-lang.org/Bool/*` in
   order to succeed.

   Note: You will need to update this test whenever the `Bool` package in the
   Prelude changes (sorry)
-}
https://test.dhall-lang.org/Bool/package.dhall
  using [ { mapKey = "Test", mapValue = "Example" } ]
