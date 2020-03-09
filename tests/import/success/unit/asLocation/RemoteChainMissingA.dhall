{- This test verifies that `missing as Location` succeeds when chained since:
   * The `missing` is never actually resolved (due to the `as Location`)
   * The `missing` should be treated as referentially transparent (and therefore
     be a valid transitive dependency of a remote import)
-}
https://raw.githubusercontent.com/Nadrieril/dhall-rust/f7d8c64a9799f139ad65427c2518376adb9e2e2f/dhall/tests/import/success/unit/asLocation/MissingA.dhall
