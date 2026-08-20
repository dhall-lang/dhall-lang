# A25 — HTTP(S) imports, headers, CORS

Depends on: A20, A24  
Read first: `prompts/haskell/00-shared.md`

## Spec

- `imports.md` — CORS, headers path
  (`DHALL_HEADERS` / XDG config / `~/.config/dhall/headers.dhall`)
- Notes: CORS as hard failure; header forwarding
- `tests/README.md` — Local Web server (already vendored)

Accept the self-signed cert from A20. Use both `localhost` and `127.0.0.1`
origins as the tests do.

## Tests that must pass

```text
tests/import/success/unit/cors/**/*A.dhall
tests/import/success/unit/Remote*A.dhall
tests/import/success/unit/SimpleRemoteA.dhall
tests/import/success/customHeadersA.dhall
tests/import/success/customHeadersImportedA.dhall
tests/import/success/headerForwardingA.dhall
tests/import/success/noHeaderForwardingA.dhall
tests/import/success/originHeaders*A.dhall
tests/import/failure/unit/cors/**/*.dhall
tests/import/failure/unit/404.dhall
tests/import/failure/customHeadersUsingBoundVariable.dhall
```

Plus all earlier import tests. That should complete
`tests/import/success` (75) and `tests/import/failure` (25) except any
still blocked on `as Source`.

## Done when

Full import suite (minus not-yet-landed as-Source extras) passes with the
vendored server running.
