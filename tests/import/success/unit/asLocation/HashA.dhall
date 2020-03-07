{-
  If you have set XDG_CACHE_HOME as described in tests/README.md, this hash will
  be available in the cache. Since this is an `as Location` test however, you
  mustn't use the cache value here.
-}
./some/import.dhall sha256:efc43103e49b56c5bf089db8e0365bbfc455b8a2f0dc6ee5727a3586f85969fd as Location
