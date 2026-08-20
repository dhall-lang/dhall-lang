# Haskell slices (set A)

Run in this order. Shared preamble: [`00-shared.md`](./00-shared.md).

| File | Summary | Tests unblocked |
|---|---|---|
| [A00](./A00-md-only-packaging.md) | Delete `.lhs` copies; symlink at build | `cabal build` |
| [A01](./A01-tasty-parser-failure.md) | Tasty helpers; parser failure | parser success+failure |
| [A02](./A02-wire-alpha-normalization.md) | Wire existing α | alpha-normalization |
| [A03](./A03-wire-beta-normalization-unit.md) | Wire existing β unit | normalization/unit |
| [A04](./A04-wire-beta-normalization-rest.md) | β without imports | simple/simplifications/… |
| [A05](./A05-binary-decode-core.md) | decode vars/app/λ/ops | binary-decode subset |
| [A06](./A06-binary-decode-collections.md) | decode list/Optional/merge | more binary-decode |
| [A07](./A07-binary-decode-scalars.md) | decode records/numbers/text | more binary-decode |
| [A08](./A08-binary-decode-imports-rest.md) | decode imports/let/time/tags | all binary-decode success |
| [A09](./A09-binary-decode-failure.md) | reject bad CBOR | binary-decode/failure |
| [A10](./A10-semantic-hash.md) | sha256(encode(α(β(e)))) | semantic-hash simple |
| [A11](./A11-typecheck-universes.md) | TypeInference module | Type/Kind |
| [A12](./A12-typecheck-scalars.md) | Bool/Nat/Text/time | matching unit tests |
| [A13](./A13-typecheck-list-optional.md) | List/Optional | matching unit tests |
| [A14](./A14-typecheck-records.md) | records/merges | matching unit tests |
| [A15](./A15-typecheck-unions.md) | unions/merge/toMap | matching unit tests |
| [A16](./A16-typecheck-with.md) | `with` | With* |
| [A17](./A17-typecheck-functions-let.md) | λ/let/assert | matching unit tests |
| [A18](./A18-typecheck-freevars.md) | freeVars | MergeHandlerFreeVar |
| [A19](./A19-typecheck-simple-failure.md) | remaining import-free TI | simple/regression/failures |
| [A20](./A20-vendor-test-server.md) | copy dhall-test-server | (infra) |
| [A21](./A21-imports-chain.md) | chain/canonicalize/Location | asLocation/path tests |
| [A22](./A22-imports-fetch-modes.md) | Code/Text/Bytes/env/`?` | local import tests |
| [A23](./A23-imports-as-source.md) | `as Source` | as-Source fixtures |
| [A24](./A24-imports-sanity-cache.md) | sanity + semantic cache | hash tests |
| [A25](./A25-imports-http.md) | HTTP/CORS/headers | rest of import/ |
| [A26](./A26-full-acceptance-suite.md) | Prelude + leftovers | entire `tests/` |
| [A27](./A27-interpret-pipeline.md) | Interpret.hs pipeline | still full tasty |
| [A28](./A28-fixture-generator.md) | encode+diag; drop Ruby | test-files-lint |
