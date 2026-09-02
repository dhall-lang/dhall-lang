# B31 — Referential sanity and semantic cache

Depends on: B29, B30  
Read first: `prompts/any-language/00-shared.md`

## Spec

Notes + `imports.md`: remote parent → only remote, `missing`, or any
`as Location`. Semantic cache files `1220${hex}` under `XDG_CACHE_HOME/dhall`
or `~/.cache/dhall`. Cache hit: decode bytes, verify hash, skip fetch.
Hash mismatch is hard. Frozen + missing resource + missing cache is soft.

Do not persist writes into committed `tests/import/cache`.

## Tests that must pass

`hashFromCache`, `nestedHash`, `normalCachingOfProtected`, unit `Hash`,
`SimpleHash`, `DontCacheIfHash`, `IgnorePoisonedCache`; failures
`HashMismatch*`, `DontRecoverHashMismatch`, `EnvFromRemote`.

## Done when

Those tests pass without requiring CORS cases.
