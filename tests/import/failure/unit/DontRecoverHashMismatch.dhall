-- This fails because the `?` operator does not recover from integrity check
-- failures
../../data/simple.dhall sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa ? 0
