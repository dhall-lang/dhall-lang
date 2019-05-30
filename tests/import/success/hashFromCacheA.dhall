{-
An import with an integrity check MUST be fetched from the cache if possible.

If you have set XDG_CACHE_HOME as described in tests/README.md, this hash will
be available in the cache, so even though this is a `missing` import, it can be
resolved by its hash.

(If you're interested, the value is a fully-αβ-normalized copy of
Prelude/Optional/null at time of test creation)
-}
missing sha256:efc43103e49b56c5bf089db8e0365bbfc455b8a2f0dc6ee5727a3586f85969fd
