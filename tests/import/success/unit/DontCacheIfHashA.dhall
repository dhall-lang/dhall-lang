-- The let triggers a loading of `simple.dhall` in the cache,
-- but the second import uses a hash that corresponds to a different file already in the content-addressed store.
-- This test ensures that we retrieve the value corresponding to the hash and not the path even if the path is in cache.
let x = ../../data/simple.dhall
in ../../data/simple.dhall sha256:3871180b87ecaba8b53fffb2a8b52d3fce98098fab09a6f759358b9e8042eedc
