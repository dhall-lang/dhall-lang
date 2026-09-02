# Overlay — Lean 4

Read after `prompts/any-language/00-shared.md` on every Lean 4 set-B session.

## Toolchain

- Lean **4** (current stable / elan). Package manager: **Lake**.
- Tests: a `lakefile` target or `#eval`/`main` walker that exits 1 on failure. `lean --run` is acceptable. Prefer a `DhallTest` executable that prints TAP or a simple pass/fail per file.
- `DHALL_LANG_TESTS` in the environment. Read files with `IO.FS` as UTF-8.
- Parser input: `String` is Unicode; index by `String.Pos` / `String.Iterator`, not by UTF-8 bytes as “characters”.

Lean is a theorem prover; **this overlay does not require proofs of soundness**. Executable `def`s that match the judgments are enough. You may add theorems later; do not block slices on `sorry`-free metatheory.

## Forbidden implementations

Do not FFI-call `dhall-haskell` or `dhall-rust` as the implementation. Do not parse Dhall by sending it to a subprocess `dhall`. C FFI is allowed only for **TLS/sockets** if Lean cannot do HTTPS alone (see HTTP).

## Project layout

```text
Dhall/Syntax.lean
Dhall/Parser.lean
Dhall/Binary.lean
Dhall/Shift.lean Dhall/Substitute.lean Dhall/Alpha.lean Dhall/Beta.lean
Dhall/Equiv.lean Dhall/FunctionCheck.lean Dhall/Typecheck.lean
Dhall/Import.lean Dhall/Hash.lean
Dhall/Bind.lean
DhallTest/Suite.lean
DhallTest/Bind.lean
lakefile.lean
```

## AST (B01)

```lean
inductive Expr where
  | lam (name : String) (ty body : Expr)
  | nat (n : Nat)
  | int (i : Int)
  | ...
  deriving Repr, Inhabited, BEq  -- BEq on Expr is not Dhall ≡; still useful
```

Use `Nat` for Natural (unbounded). Use `Int` for Integer. Recursion is native.

```lean
inductive ImportMode where | code | text | bytes | location | source
```

Do not use `Lean.Json` as the Dhall AST.

## Parser (B02–B08)

- Recursive descent on `String.Iterator`. Backtrack by restoring the iterator. Alternatives in ABNF order.
- Lean’s **syntax parser** (`Lean.Parser`) is for Lean source, not Dhall. Do not try to embed Dhall in Lean’s parser tables.
- A small `Parsec`-style combinator module in Lean is fine (`Lean.Data.Parsec` exists for strings in some versions — use it only if it backtracks correctly).
- Nested comments: explicit depth counter.

## Numbers, text, time

| Dhall | Lean 4 |
|---|---|
| Bool | `Bool` |
| Natural | `Nat` |
| Integer | `Int` |
| Double | `Float` (document IEEE limitations of Lean `Float`; still encode per `binary.md` using bit patterns if needed) |
| Text | `String` |
| Bytes | `ByteArray` |
| Date | structure `YMD` |
| Time | structure + `precision : Nat` |
| TimeZone | `Int` minutes |

`Nat`/`Int` are unbounded — good. B37 `2^64` is `2 ^ 64` in Lean.

`Float` equality: via CBOR bytes. If Lean `Float` cannot represent a CBOR payload you decode, fail decode (do not coerce silently).

## CBOR (B09–B10)

There is no standard-library Dhall CBOR. **Implement the Dhall CBOR subset** in Lean (`ByteArray` builder/parser) following `binary.md`. This is expected and in scope. Do not depend on a vague `npm` CBOR via FFI.

SHA-256: if Lean stdlib lacks it, use a small pure implementation or a well-known Lean crypto package; correctness matters for semantic-hash tests.

## HTTP client and test server (B32)

Lean 4 I/O can do TCP (`IO.TCP` / socket APIs vary by version). **HTTP/1.1 client** for `http://127.0.0.1:18080` should be written in Lean.

**HTTPS** (`https://127.0.0.1:18443`) and TLS are often missing in pure Lean:

1. Preferred: a thin **OpenSSL/rustls C or Rust stub** linked from Lake, used only as a TLS byte pipe; HTTP framing and CORS stay in Lean.
2. Acceptable documented deviation: HTTP import tests pass in Lean; HTTPS/CORS tests run against the same Lean HTTP handler behind a local TLS terminator **you start from the test executable** (still your process). Do not skip CORS *logic*.
3. Not acceptable: calling `dhall` or skipping `tests/import` HTTPS cases without recording the deviation.

**Test server:** implement GET routes from `tests/README.md` in Lean on port 18080; wrap 18443 with the same TLS stub. Unix newlines. Bind `127.0.0.1`.

## Errors

```lean
inductive Error where
  | parse (msg : String)
  | typecheck (msg : String)
  | import (soft : Bool) (msg : String)
```

Use `Except Error` in the library. `IO` only at the edges (filesystem, sockets). Type-inference failure tests: `IO.timeout` or a fuel parameter **in addition** to a wall-clock timeout (fuel alone is not the suite’s notion of timeout).

## Bindings (B36–B40)

Lean bindings are typeclass-based:

```lean
class FromDhall (α : Type) where
  expectedType : Expr
  fromExpr : Expr → Except DecodeError α

class ToDhall (α : Type) where
  dhallType : Expr
  toExpr : α → Expr
```

| Dhall | Lean |
|---|---|
| Bool, Text, Bytes | `Bool`, `String`, `ByteArray` |
| Natural, Integer | `Nat`, `Int` |
| Double | `Float` |
| Optional a | `Option α` |
| List a | `List α` |
| records | structures with field names = labels |
| unions | `inductive` with one constructor per alternative |
| `A → B` | `α → Except _ β` that encode/applies/normalizes/decodes |

Do not bind `Expr` itself as the only “binding.” `Type`/`Kind`/`Sort` stay unbound.

Encoding Lean functions to Dhall is optional (B40). Metaprogramming (`deriving FromDhall`) is optional.

## CLI (B34)

Lake `@[default_target] lean_exe dhall`. Read stdin / `--file`. `IO.Process.exit 1`.

## Do not

- Prove normalization in this prompt series (optional extra)
- Use `Lean.Json` as Dhall
- `unsafe` hacks to smash `Float` bits unless documented next to `binary.md` float tests
