{- The purpose of this test is to verify that the custom headers supplied to
   this import are not forwarded to the transitive import of
   https://httpbin.org/user-agent
-}
https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/tests/import/success/customHeadersA.dhall
  using [ { mapKey = "User-Agent", mapValue = "Secret" } ]
