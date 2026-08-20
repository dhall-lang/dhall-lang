# Language-agnostic slices (set B)

Run in this order. Shared preamble: [`00-shared.md`](./00-shared.md).
These prompts are **not** Haskell-specific. Implement in any language.
Point the harness at this repository’s `tests/` directory.

For a concrete language, also attach one file from
[`overlays/`](./overlays/) (Java, Scala, Rust, Go, Python, TypeScript,
Lean 4, C++, Haskell, Swift, C#, F#, Erlang, Kotlin, OCaml). How to combine them:
[`overlays/README.md`](./overlays/README.md).

| File | Summary |
|---|---|
| [B00](./B00-architecture.md) | Skeleton + test discovery |
| [B01](./B01-ast.md) | AST including `as Source` |
| [B02](./B02-parser-comments.md) | Whitespace, comments, labels |
| [B03](./B03-parser-literals.md) | Numbers, time, bytes |
| [B04](./B04-parser-text.md) | Text + multiline desugar |
| [B05](./B05-parser-imports.md) | Import syntax |
| [B06](./B06-parser-records.md) | Records/unions; parse-time sugar |
| [B07](./B07-parser-expression.md) | Full expression parser |
| [B08](./B08-parser-failure.md) | Parser failure suite |
| [B09](./B09-binary-encode.md) | CBOR encode; parser success |
| [B10](./B10-binary-decode.md) | CBOR decode suite |
| [B11](./B11-shift.md) | De Bruijn shift |
| [B12](./B12-substitution.md) | Substitution |
| [B13](./B13-alpha.md) | α-normalization tests |
| [B14](./B14-beta-scalars.md) | β Bool/Nat/Int/… |
| [B15](./B15-beta-text-list.md) | β Text/List/Optional |
| [B16](./B16-beta-records.md) | β records/unions/with |
| [B17](./B17-beta-functions.md) | β λ/let; rest of unit |
| [B18](./B18-equiv-functioncheck.md) | ≡ and PTS function check |
| [B19](./B19-typecheck-universes.md) | Type/Kind/Sort |
| [B20](./B20-typecheck-scalars.md) | Scalar typing |
| [B21](./B21-typecheck-list-optional.md) | List/Optional typing |
| [B22](./B22-typecheck-records.md) | Record typing |
| [B23](./B23-typecheck-unions.md) | Union/merge/toMap |
| [B24](./B24-typecheck-with.md) | `with` typing |
| [B25](./B25-typecheck-functions.md) | λ/let/assert |
| [B26](./B26-typecheck-freevars.md) | freeVars; import-free TI |
| [B27](./B27-imports-model.md) | Stack, Γ, modes |
| [B28](./B28-imports-chain.md) | Chain/canonicalize/Location |
| [B29](./B29-imports-fetch.md) | Code/Text/Bytes/`?` |
| [B30](./B30-imports-as-source.md) | `as Source` two-phase |
| [B31](./B31-imports-cache.md) | Sanity + semantic cache |
| [B32](./B32-imports-http.md) | HTTP/CORS; full `tests/` |
| [B33](./B33-semantic-hash.md) | sha256 encode αβ |
| [B34](./B34-driver.md) | CLI/library pipeline |
| [B35](./B35-test-harness.md) | One command, full suite |
| [B36](./B36-bindings-overview.md) | Decoder/Encoder API |
| [B37](./B37-bindings-primitives.md) | Scalars, List, Optional |
| [B38](./B38-bindings-records-unions.md) | Records and unions |
| [B39](./B39-bindings-functions.md) | Dhall functions as host callables |
| [B40](./B40-bindings-encode.md) | Host → Dhall encoders |
