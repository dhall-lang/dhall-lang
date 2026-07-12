{-
    This test verifies that `env:VAR as Location` isn't rejected as referentially opaque,
    as `env:VAR` on its own would.
-}
http://localhost:18080/tests/import/success/unit/asLocation/EnvHomeA.dhall
