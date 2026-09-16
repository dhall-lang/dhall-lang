# Haskell reference implementation (this branch)

This is the changelog and operator’s guide for the literate Haskell reference
on branch `complete-reference-implementation`.  It describes **every** change
from `master` through the Set A slices (`prompts/haskell/A00` … `A28`): what
was implemented, where it lives, how to run it, and what is still uncommitted.

The formal judgments remain in the kebab-case `standard/*.md` documents.  This
file is not a substitute for those documents; it records how the *executable*
reference was completed and how it differs from a production interpreter.

## Status

**Not everything on this branch is committed.**

| State | What |
|---|---|
| Committed (`master`..`HEAD`) | Prompts (`prompts/`), slices A00–A22 |
| **Uncommitted working tree** | Slices A23–A28: `as Source`, hash/cache, HTTP/CORS, full tasty suite, `Interpret` pipeline, fixture generator, new test fixtures |
| Do not commit | `.DS_Store`, `.idea/`, `.kilo/` |

`HEAD` is `c5aa1ce` (`standard: fetch local/env Code, Text, and Bytes imports (prompt A22)`).

A23–A28 exist only as unstaged edits plus untracked fixtures under
`tests/import/success/unit/AsSource*` and
`tests/parser/success/unit/import/asSource*`.

Last measured run (naive evaluator, Prelude type-inference and semantic-hash
trees skipped — see [Skipped tests](#skipped-tests)):

```text
cd standard
cabal test --with-compiler=ghc-9.10.3 --test-show-details=direct
# Test suite tasty: PASS
# All 1259 tests passed (~74s)
```

## Why this exists

Set A in [`prompts/haskell/`](../prompts/haskell/README.md) is a script to turn
the literate `standard/` package into a **complete reference**: parse, encode,
decode, shift, substitute, α, β, equivalence, type inference, import
resolution, and a small `dhall` executable that drives the same pipeline.

It is intentionally naive.  It follows the judgments even when that is
verbose or slow.  It must not copy production modules from
[`dhall-haskell`](https://github.com/dhall-lang/dhall-haskell) (the only
allowed copy is the HTTP test-server used by A20).  Expression equality in
tests is **CBOR `encode` byte equality** (NaN equals itself; `-0.0` ≠ `+0.0`).

## How to build and test

From `standard/`:

```bash
./link-literate.sh   # or let Cabal Setup.hs / Nix postPatch do it
cabal build --with-compiler=ghc-9.10.3
cabal test  --with-compiler=ghc-9.10.3 --test-show-details=direct
```

`nix-shell` from `standard/shell.nix` matches CI’s toolchain if you use Nix.

GHC compiles `Module.lhs` via `markdown-unlit`.  After A00 those `.lhs` names
are **untracked symlinks** to kebab-case `.md` files (`syntax.md` →
`Syntax.lhs`, …).  `Setup.hs` runs `./link-literate.sh` in `preConf`.  Do not
commit the `.lhs` copies.

The test driver (`tasty/Main.hs`) starts the vendored HTTP(S) fixture server
on **18080 / 18443** for the whole run (`NumThreads 1`, because the server
and some tests share process-wide environment).  Do not run two `tasty`
processes at once: they deadlock on those ports.

Smoke the interpreter:

```bash
printf 'λ(x : Bool) → x' | cabal run exe:dhall --with-compiler=ghc-9.10.3 --verbose=0
# TList [TInt 1,TString "x",TString "Bool",TList [TString "x",TInt 0]]
```

## Layout

| Path | Role |
|---|---|
| `standard/*.md` | Spec + literate Haskell (judgments) |
| `standard/Parser.hs` | Parser from `dhall.abnf` (not literate) |
| `standard/Interpret.hs` | `dhall` CLI pipeline (not literate) |
| `standard/imports-implementation.md` | Literate `Imports` module |
| `standard/imports-implementation-notes.md` | Prose algorithm (`as Source`, cache, CORS) |
| `standard/imports.md` | Formal import judgments |
| `standard/tasty/Main.hs` | Acceptance-test driver |
| `standard/tasty/TestServer.hs` | Vendored `dhall-test-server` |
| `standard/test-server/cert/` | Self-signed cert for HTTPS fixtures |
| `standard/dhall/Main.hs` | Thin `exe:dhall` wrapper around `Interpret` |
| `standard/link-literate.sh` | `.md` → `.lhs` symlinks |
| `tests/` | Language acceptance suite |
| `scripts/generate-test-files.sh` | Regenerates `*.dhallb` / `*.diag` from this package |
| `prompts/haskell/` | Slice script that produced this work |

Exposed library modules: `Syntax`, `Parser`, `Shift`, `Substitution`,
`AlphaNormalization`, `BetaNormalization`, `Equivalence`, `FunctionCheck`,
`TypeInference`, `Binary`, `Multiline`, `Imports`, `Interpret`.

## Interpreter CLI (`Interpret.hs`)

Default pipeline:

```text
stdin → parse → resolve imports (cwd as fake root) → inferType → β-normalize
```

Prints the CBOR `Term` of the result.  Type-check failure exits non-zero.
Printing Dhall source is **not** implemented.

| Flag | Effect |
|---|---|
| *(none)* | Full pipeline; print Haskell `Term` |
| `--parse-only` | Parse + encode only (parser fixtures) |
| `--from-cbor` | Stdin is raw CBOR, not Dhall text |
| `--diag` | RFC 8949 diagnostic notation instead of `Term` / raw CBOR |
| `[outfile]` | Write CBOR bytes (or diag text) there |

Examples:

```bash
dhall --parse-only out.dhallb < in.dhall
dhall --parse-only --diag out.diag < in.dhall
dhall --from-cbor --diag out.diag < in.dhallb
```

## Fixture generation (A28)

`scripts/generate-test-files.sh` no longer uses Nix + Ruby `cbor2diag.rb`.

1. For each `tests/parser/success/**/*A.dhall`: parse + encode → `*B.dhallb`
   and `*B.diag`.
2. For every committed `*.dhallb` under `tests/`: write a matching `*.diag`
   (`--from-cbor --diag`).  Hand-crafted binary-decode `*A.dhallb` files are
   **not** rewritten; only their `.diag` is.

Nix `expected-test-files` in `nixops/overlay.nix` uses
`${haskellPackages.standard}/bin/dhall --from-cbor --diag`.
`.github/CONTRIBUTING.md` and `tests/README.md` document this.

### Diagnostic notation (`Binary.diag`)

Golden output is the committed `*.diag` files.  Notable choices that match
those goldens (not a generic CBOR pretty-printer):

- Byte strings: uppercase hex, `h'ABCD'`.
- BEL / VT: `\a` / `\v`.
- `$` and `/` are **not** escaped (JSON-style `\"` and `\\` still are).
- Unicode: `\uXXXX` / `\u{…}` as in the goldens.
- Floats: `printf "%.16f"` then strip trailing zeros (not Haskell `show`
  scientific notation).
- `NaN`, `Infinity`, `-Infinity`, `-0.0` as those tokens.

## Acceptance-test driver (`tasty/Main.hs`)

Discovery walks `tests/` and names each case with a path **relative to
`tests/`** so flattened names like `0A.dhall` stay unique.

Per-test import environment (`withImportEnvironment`):

- `HOME` = `tests/import/home`
- `XDG_CACHE_HOME` = a **temp copy** of `tests/import/cache` (tests must not
  write the committed cache)
- `DHALL_TEST_VAR=6 * 7`
- Unsets `DHALL_HEADERS`, `USER_AGENT`, `XDG_CONFIG_HOME`
- Applies sibling `ENV.dhall` maps when present
- Import resolution uses a fake root above the repo so quoted `..` paths work

Suites wired:

| Group | What it does |
|---|---|
| parser success | parse `A` → `encode` → bytes equal `B.dhallb` |
| parser failure | parse must fail |
| α-normalization | α(`A`) encode-equals `B` |
| β-normalization | resolve imports if present; β(`A`) encode-equals `B` |
| binary-decode success | deserialise `A.dhallb` → `decode` → encode-equals parsed `B` |
| binary-decode failure | `decode` must fail |
| semantic-hash | `sha256:` + hex of encode(α(β(e))) equals `B.hash` |
| type-inference success | resolve if needed; inferred type encode-equals `B` |
| type-inference failure | infer must fail (30s timeout: ill-typed terms need not terminate) |
| import success / failure | `resolveExpression`; compare to `B` or expect an error |

Timeouts: type-inference **success** 10 minutes; β-normalization and
semantic-hash 2 minutes (covers `remoteSystems`); other TI failure 30s.

Equality: `Binary.encode` bytes, not `==` on `Double`.

### Skipped tests

The naive importer **type-checks and β-normalizes** every Code import.  Prelude
files with `assert` examples are too slow (e.g. `Prelude/List/shifted`
exceeded 10 minutes).  The driver therefore **omits**:

- `tests/type-inference/success/preludeA.dhall` (full `Prelude/package.dhall`)
- `tests/type-inference/success/prelude/**`
- `tests/semantic-hash/success/prelude/**`

Import-suite files such as `tests/import/success/cors/PreludeA.dhall` are
**not** skipped.  `CacheImports*` type-inference tests still run (disk
semantic cache disabled for TI, matching `--no-cache`).

This is a **test-harness skip**, not a spec change.  A production evaluator
is expected to pass those Prelude cases.

## Spec / grammar changes on this branch

These are language-surface changes needed for `as Source` (not yet on
`master`):

### ABNF (`standard/dhall.abnf`)

```abnf
import = import-hashed [ whsp1 as whsp1 (Text / Location / Bytes / Source) ]
```

### Syntax (`standard/syntax.md`)

- `ImportMode` gained `Source` and `Ord`.
- `Scheme` gained `Eq` (CORS same-origin).

### Binary (`standard/binary.md`)

Import mode integers: `0` Code, `1` Text, `2` Location, `3` Bytes, **`4`
Source**.  The as-Source standard PR has not assigned a CBOR integer yet;
this reference uses `4` pending that (comment next to `encode`).

`decode` also accepts self-describe tag `55799` on nested terms.

### Parser (`standard/Parser.hs`)

- `_Source` / `as Source`.
- `keywordToken` requires an identifier boundary
  (`notFollowedBy simpleLabelNextChar`) so `in` does not steal the prefix of
  `indexed`, and similarly for `if` / `then` / `else` / `let` / `as` /
  `using` / `merge` / `Infinity` / `NaN` / `Some` / `toMap` / `assert` /
  `forall` / `with` / `showConstructor` / `Location` / `Source`.
- Out-of-range `Double` literals are parse failures; `using` requires the
  whitespace the ABNF mandates.

### New fixtures (untracked until committed)

Parser:

- `tests/parser/success/unit/import/asSourceA.dhall` — `./foo.dhall as Source`
- matching `asSourceB.dhallb` / `asSourceB.diag`

Import resolution:

- `AsSourceUnhashed` — child `let x = 1 in x as Source` inlines **without**
  β-normalizing the `let` (B is `let x = 1 in x`).
- `AsSourceHashed` — parent imports a hashed child as Source; phase 2 B is
  still `let x = 1 in x` (hashed child validated/normalized to `1`).  Phase 1
  cache product would keep the hashed import node; documented in the A-file
  comment.  Child integrity hash:
  `sha256:d60d8415e36e86dae7f42933d3b0c4fe3ca238f057fba206c7e9fbf5d784fe15`
  (`1`).

## Import resolver (A21–A25)

Literate module: `standard/imports-implementation.md` (`Imports`).

Algorithm (high level):

1. Chain + canonicalize against the parent on the stack.
2. `as Location` returns a location encoding immediately (no fetch, **no
   hash/cache**).
3. Cycle detection (import already on the stack) and referential sanity
   (a remote parent may not import a local file or `env:`).
4. Hashed imports look up the semantic cache **first**
   (`XDG_CACHE_HOME/dhall/1220${hex}` only — no dhall-haskell
   “semi-semantic” cache).
5. Unhashed imports reuse an in-memory map keyed by
   `(pretty import type, ImportMode)` so `./foo` ≠ `./foo as Source`.
6. Fetch: local file, `env:`, or HTTP(S) with CORS.
7. Interpret bytes by mode:
   - `RawText` / `RawBytes` — literals
   - `Code` / `Source` — parse, walk children, then (Code) infer + β-nf
8. Integrity: hash is SHA-256 of encode(α(term)).  Mismatch is a **hard**
   failure (not recovered by `?`).
9. Code **return** value is β-nf; the value **stored** in the semantic cache
   is α(β).  Returning α(β) as the runtime value broke
   `remoteSystemsA` (binder names).
10. `?` alternatives: left **soft** failure continues; **hard** failure
    (CORS, hash mismatch, referential sanity, cycles, ill-typed import)
    does not.

### `as Source` (A23)

Two-phase walk (`ChildPolicy`):

- **Phase 1 (cache product):** `PreserveHashed` — unhashed children inlined
  without normalization; hash-protected children left as import nodes after
  validation.  That product is what a frozen parent stores.
- **Phase 2 (parent value):** `InlineEverything` — remaining hashed imports
  expanded; result is import-free and type-checked, **not** fully
  β-normalized.

### HTTP / CORS / headers (A20, A25)

- Vendored server serves `tests/import/` on `http://localhost:18080` and
  `https://127.0.0.1:18443` (self-signed cert accepted by an insecure
  manager).
- CORS failure is **hard**.
- Request headers: `DHALL_HEADERS`, else `XDG_CONFIG_HOME/dhall/headers.dhall`,
  else `~/.config/dhall/headers.dhall`.  Origin-map keys win over `using`
  headers.
- `case-insensitive` is a cabal dependency for header names.

## Slice-by-slice changelog

Prompts live in `prompts/haskell/`.  Shared rules:
[`00-shared.md`](../prompts/haskell/00-shared.md).

### Committed

| Slice | Commit | What changed |
|---|---|---|
| *(prompts)* | `03a0d62` | Set A / Set B agent scripts under `prompts/` |
| **A00** | `c599ef8` | Delete committed `.lhs` copies; `link-literate.sh` + `Setup.hs`; kebab-case `.md` is the only literate source; Bytes literals decoded via `memory` for `base16-1.0` |
| **A01** | `ff3aa60` | Tasty helpers; parser **failure** suite (94); reject out-of-range Doubles; `using` needs ABNF whitespace |
| **A02** | `871a2eb` | α-normalization suite; shift uses `Integer` so `n+d` does not underflow `Natural` |
| **A03** | `e169620` | Unit β-normalization (246). Fixes: `Forall` recursion, trivial `if`, `Text/show` control escapes, `List/fold` missing type, `List/build` empty-list type, `merge` type-annotation normalization |
| **A04** | `a85fde1` | Remaining import-free β (`simple`, `simplifications`, `haskell-tutorial`, `regression`, top-level). Then skipped `remoteSystemsA` / `issue661A` because they import; those run now that imports exist |
| **A05–A09** | `f6ca830` | `Binary.decode` for the full binary-decode success + failure suites; tag `55799` |
| **A10** | `09d9933` | Semantic hash = SHA-256 of encode(α(β(e))) for import-free fixtures |
| **A11–A19** | `6af5d89` | Literate `TypeInference` following `type-inference.md`; import-free success + failure; encode-byte comparison |
| **A20** | `e124afd` | Vendor `dhall-test-server` as `tasty/TestServer.hs`; serve this repo’s `tests/import` on 18080/18443 |
| **A21** | `2b7c8dc` | Literate `Imports`: chain, canonicalize, `as Location`, local/env/HTTP fetch skeleton |
| **A22** | `c5aa1ce` | Fetch `as Bytes`; type-check + normalize **Code** imports; local/env `?` alternatives. Hash and CORS left to A24–A25 |

### Uncommitted (A23–A28)

| Slice | What changed |
|---|---|
| **A23** | `as Source` in ABNF, `Syntax.ImportMode`, parser, encode/decode mode `4`; two-phase resolve; parser + import fixtures above |
| **A24** | Referential sanity; integrity hash; semantic cache `1220${hex}`; hashed lookup-before-fetch; Location ignores hash/cache |
| **A25** | HTTP CORS (hard failure); origin + `using` headers; remaining `tests/import/**` |
| **A26** | Full `tests/` tree in tasty (parser, α, β including import cases, binary-decode, hash, TI, import). Prelude TI/hash skipped as above |
| **A27** | `Interpret.hs`: stdin → parse → resolve → infer → β; flags `--parse-only` / `--from-cbor` / `--diag` |
| **A28** | `Binary.diag`; bash `generate-test-files.sh`; Nix overlay drops Ruby `cbor2diag`; CONTRIBUTING + `tests/README.md` |

## Notable correctness bugs fixed along the way

These are implementation pitfalls, not spec amendments:

- **Keyword prefixing:** `in` vs `indexed` (Prelude parse).  Keywords must
  not match as a prefix of a label.
- **Location + hash:** `as Location` must not fetch or check a hash
  (`asLocation/HashA`).
- **Code cache vs runtime value:** cache stores α(β); Code imports **return**
  β-nf so binder names in `remoteSystemsA` match.
- **Duplicate tasty names:** `testName` is `makeRelative testsRoot path`.
  Directory-only suffixes like `0A` collided and hid failures.
- **Temp cache:** `createTempDirectory` is not used (not exported the way
  the first draft assumed).  Each import test builds a unique temp dir from
  a sanitized path and copies the committed cache.
- **CORS `Scheme`:** needed `Eq` for same-origin.
- **`-Werror`:** avoided name shadowing (`path`, `product`) and overlapping
  `ImportMode` patterns.

## What this reference does *not* do

- Performance work (explicitly out of scope).  Prelude package import is
  unusably slow; those tests are skipped rather than optimized.
- Dhall pretty-printing / `dhall-haskell` CLI compatibility.
- Semi-semantic cache under `.cache/dhall-haskell/`.
- Host-language bindings (that is Set B).
- Changing judgment **meaning** to make a test pass.

## Files touched vs `master` (map)

**Packaging / literate:** `standard/Setup.hs`, `link-literate.sh`,
`.gitignore`, `standard.cabal`, `standard/README.md`, deleted committed
`*.lhs` (replaced by symlinks at build).

**Judgments filled in:** `syntax.md`, `shift.md`, `beta-normalization.md`,
`equivalence.md`, `binary.md`, `type-inference.md`,
`imports-implementation.md`, `imports-implementation-notes.md`.

**Ordinary Haskell:** `Parser.hs`, `Interpret.hs`, `tasty/Main.hs`,
`tasty/TestServer.hs`.

**Tooling:** `scripts/generate-test-files.sh`, `nixops/overlay.nix`,
`.github/CONTRIBUTING.md`, `tests/README.md`.

**New tests:** `as Source` parser + import fixtures (untracked until
committed).

**Prompts:** entire `prompts/` tree (Set A and Set B).
