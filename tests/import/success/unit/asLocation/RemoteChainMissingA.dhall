{- This test verifies that `missing as Location` succeeds when chained since:
   * The `missing` is never actually resolved (due to the `as Location`)
   * The `missing` should be treated as referentially transparent (and therefore
     be a valid transitive dependency of a remote import)
-}
http://localhost:18080/tests/import/success/unit/asLocation/MissingA.dhall
