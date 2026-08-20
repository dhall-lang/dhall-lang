# A24 — Referential sanity, integrity checks, semantic cache

Depends on: A22 (A23 if hashes interact with Source)  
Read first: `prompts/haskell/00-shared.md`

## Spec

- Notes: referential sanity; `as Location` always allowed; cycle detection
  (already A22); semantic cache paths `1220${hex}` under
  `XDG_CACHE_HOME/dhall/` or `~/.cache/dhall/`
- `imports.md`: Referential sanity check; hash-protected imports

Remote parents may not import local files or `env:` (after chaining).
Hard failure if cached bytes fail the SHA-256 check (`DontRecoverHashMismatch`).
Soft failure if frozen import is absent from cache **and** the resource is
absent.

Do **not** write a semi-semantic cache. Tests must not persist new cache
entries into the committed `tests/import/cache` (use a temp copy or treat
as read-only).

## Tests that must pass

```text
tests/import/success/hashFromCacheA.dhall
tests/import/success/nestedHashA.dhall
tests/import/success/normalCachingOfProtectedA.dhall
tests/import/success/unit/HashA.dhall
tests/import/success/unit/SimpleHashA.dhall
tests/import/success/unit/DontCacheIfHashA.dhall
tests/import/success/unit/IgnorePoisonedCacheA.dhall
tests/import/failure/unit/HashMismatch.dhall
tests/import/failure/unit/HashMismatch2.dhall
tests/import/failure/unit/DontRecoverHashMismatch.dhall
tests/import/failure/unit/EnvFromRemote.dhall
tests/import/failure/originHeadersFromRemote.dhall   # skip if needs HTTP
```

Plus A21–A22.

## Done when

Cache and sanity tests listed pass without HTTP.
