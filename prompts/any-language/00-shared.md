# Shared instructions — any language (set B)

Read this file at the start of every `B*` slice. If you are targeting a
specific language, also read the matching overlay in
[`overlays/`](./overlays/) (`java`, `scala`, `rust`, `golang`, `python`,
`typescript`, `lean4`, `cpp`, `haskell`, `swift`, `csharp`, `fsharp`,
`erlang`, `kotlin`, `ocaml`). Spec and test globs in `B*.md` win over an
overlay; host types and libraries in the overlay win over vague wording
in `B*.md`.

## Goal

Implement Dhall, including import resolution and a host-language bindings
library, so that:

1. The acceptance suite in `tests/` of **this** `dhall-lang` repository passes, as specified in `tests/README.md`.
2. A small bindings suite (described in `B36`–`B40`) can decode normalized Dhall values into host values and encode host values back.

Performance is irrelevant. Correctness and fidelity to `standard/` matter.

## Authoritative sources (in this order)

1. `standard/dhall.abnf` — concrete syntax. No lexer. Backtracking is required. When alternatives both match, take the **first** (left-to-right).
2. Judgment documents under `standard/*.md` — meaning of parse desugaring, shift, substitution, α, β, ≡, function check, type inference, binary encode/decode, imports.
3. `standard/imports-implementation-notes.md` (or the copy on git branch `feature/winitzki/lang-1185-import-as-source`) — practical import algorithm, including **`as Source`** (upcoming standard; implement it).
4. `tests/README.md` — how each suite is executed, including the local HTTP(S) server contract.

`dhall-haskell` may be read for inspiration. Do **not** translate it line-by-line. Do **not** implement its extra semi-semantic cache.

## Phase order

```text
parse  →  resolve imports  →  type-check  →  α-normalize (when required)
                                           →  β-normalize
                                           →  encode (hash / binary tests)
```

Totality: only a successfully type-checked expression is guaranteed to normalize. Guard `type-inference/failure` with a timeout.

Import resolution type-checks each resolved **Code** import in the empty context. `as Source` cache products may still contain hash-protected import nodes; the value returned to the parent is import-free but not fully normalized.

## Expression comparison

Do not compare pretty-printed source. Compare:

- parser tests: CBOR bytes of `encode(parse(A))` vs `B.dhallb`
- α / β / type-inference success: `encode` of both sides (handle CBOR NaN)
- semantic-hash: SHA-256 of `encode(α(β(resolved e)))` for ordinary Code imports, formatted as in `*.hash`

## `as Source`

Treat as part of the language even if `master` ABNF still says
`as (Text / Location / Bytes)`. Extend the grammar with `Source`. CBOR import
mode integers: 0=Code, 1=Text, 2=Location, 3=Bytes; use **4** for Source unless
`binary.md` on the as-Source PR specifies otherwise.

## Bindings (B36–B40 only)

The language standard does **not** specify marshaling (`standard/README.md`).
Those slices define a portable Decoder/Encoder so a library can turn a
type-checked, normalized expression into a host value. They are not required
for `tests/` to pass.

## Local import tests

Reimplement the HTTP/HTTPS server in `tests/README.md`
(`127.0.0.1:18080` and `127.0.0.1:18443`). The Haskell reference (set A)
vendors dhall-haskell’s server. Set B Haskell may do the same
(`dhall-test-server` only; see the haskell overlay). Every other language
should follow the **contract**, not that Haskell code.

Set `XDG_CACHE_HOME` to `tests/import/cache`, `HOME` to `tests/import/home`,
and `DHALL_TEST_VAR` to `6 * 7`. Do not persist writes to the committed cache.

## Done when (every slice)

Named test globs pass; earlier slices still pass; no speculative features
beyond the slice.
