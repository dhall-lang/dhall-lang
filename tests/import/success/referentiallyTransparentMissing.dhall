{- This test verifies that `missing` is treated as a referentially transparent
   import.  The following import contains a `missing as Location` in its
   test assertion that should succeed since:

   * The `missing` is never actually resolved (due to the `as Location`)
   * The `missing` should be treated as referentially transparent (and therefore
     be a valid transitive dependency of a remote import)
-}
https://prelude.dhall-lang.org/v11.0.0/Location/Type
