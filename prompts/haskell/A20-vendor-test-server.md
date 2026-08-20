# A20 — Vendor the dhall-haskell test HTTP(S) server

Depends on: A01 (tasty driver)  
Read first: `prompts/haskell/00-shared.md`

## Goal

Copy the **test server only** from sibling `dhall-haskell` so import tests
can run later. This is the sole allowed copy from that repo.

## Source

Sibling checkout `../dhall-haskell` (or clone
`https://github.com/dhall-lang/dhall-haskell`) paths:

- `dhall-test-server/src/Dhall/Test/Server.hs`
- `dhall-test-server/cert/cert.pem`
- `dhall-test-server/cert/key.pem`
- `dhall-test-server/dhall-test-server.cabal` (dependencies: `wai`, `warp`,
  `warp-tls`, `async`, `http-types`, …)

## Do

1. Place the server in this repo, e.g. `standard/test-server/` or
   `tests/server/`, with certs.
2. Rewrite fixture path lookup so `GET /tests/import/...` serves
   `tests/import/...` **from this repository root**, not
   `dhall/dhall-lang/tests/import`. Keep Unix newlines. Keep CORS `*` on
   those static responses.
3. Keep ports `127.0.0.1:18080` (HTTP) and `127.0.0.1:18443` (HTTPS),
   endpoints `/foo`, `/bar`, `/user-agent`, `/random-string`, `/cors/*`
   exactly as in the copied file / `tests/README.md`.
4. Depend on the server from the `tasty` test-suite. Call `withServers`
   around `defaultMain` (see `dhall-haskell/dhall/tests/Dhall/Test/Main.hs`
   for the pattern, not for language code).
5. Nix: add `warp` / `warp-tls` / `wai` / `async` to the `standard` test
   deps if cabal2nix does not pick them up.

## Do not

- Copy `Dhall.Import` or any language module.
- Change endpoint semantics.

## Tests that must pass

No new acceptance tests required. Existing tasty suites must still pass
with the server started (it should be idle). Optionally add a tiny smoke
test that `GET http://127.0.0.1:18080/random-string` returns 200.

## Done when

`cabal test` still green; certs are in the tree; README or cabal comment
states the code is vendored from dhall-test-server.
