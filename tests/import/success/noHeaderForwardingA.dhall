{- The purpose of this test is to verify that the custom headers supplied to
   this import are not forwarded to the transitive import of
   https://httpbin.org/user-agent
-}
-- https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/tests/import/success/customHeadersA.dhall
https://raw.githubusercontent.com/dhall-lang/dhall-lang/b81b5d9957d45374e4847421e6551e3d9387d93d/tests/import/success/customHeadersA.dhall
  using [ { header = "User-Agent", value = "Secret" } ]
