{- The purpose of this test is to verify that the custom headers supplied to
   this import are not forwarded to the transitive import of
   https://localhost:18443/user-agent
-}
https://localhost:18443/tests/import/success/customHeadersA.dhall using ([ { mapKey =
                                                                               "User-Agent"
                                                                           , mapValue =
                                                                               "Secret"
                                                                           }
                                                                         ])
