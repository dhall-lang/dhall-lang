{- The purpose of this test is to verify that the custom headers supplied to
   this import are not forwarded to the transitive import of
   http://localhost:18080/user-agent
-}
http://localhost:18080/tests/import/success/customHeadersA.dhall
  using [ { mapKey = "User-Agent", mapValue = "Secret" } ]
