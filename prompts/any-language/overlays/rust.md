# Overlay — Rust 2021

Read after `prompts/any-language/00-shared.md` on every Rust set-B session.

## Toolchain

- Edition **2021**, current stable Rust.
- `cargo test` / `cargo build`. Workspace optional (`dhall` lib + `dhall-cli` + `dhall-test-server` binary).
- Env: `DHALL_LANG_TESTS` = absolute `dhall-lang/tests`.
- Files: UTF-8. Use `String` / `&str` for Text; iterate **chars** (`char` is Unicode scalar) or decoded bytes, not `u8` as a Dhall character.

## Forbidden implementations

Do **not** implement by wrapping or forking:

- [`dhall-rust`](https://github.com/Nadrieril/dhall-rust) / `serde_dhall`
- `dhall-python` (Rust FFI)
- the Haskell `dhall` CLI

You may read `dhall-rust` for ideas (especially CBOR edge cases). Do not paste modules.

## Project layout

```text
src/syntax.rs
src/parser/
src/binary.rs
src/shift.rs src/substitute.rs src/alpha.rs src/beta.rs
src/equiv.rs src/function_check.rs src/typecheck.rs
src/import.rs src/hash.rs
src/bind.rs
tests/suite.rs          # walks DHALL_LANG_TESTS
tests/bind.rs           # B36–B40
```

`lib.rs` re-exports phases. Import tests: set `HOME`, `XDG_CACHE_HOME`, `DHALL_TEST_VAR` in the test process (`std::env::set_var` is process-wide — prefer a mutex or `tempfile` cache copy).

## AST (B01)

```rust
enum Expr { /* one variant per syntax.md constructor */ }
```

Recursive variants hold `Box<Expr>` or `Rc<Expr>` (either is fine; no performance mandate). Records: `Vec<(String, Expr)>` or `BTreeMap<String, Expr>` (sorted keys help binary.md).

```rust
enum ImportMode { Code, Text, Bytes, Location, Source }
```

Do not use `serde_json::Value` as the AST.

## Parser (B02–B08)

- Recursive descent, **nom**, or **winnow** on `&str` / char indices. Combinators must **backtrack**; `alt` order = ABNF order.
- **No** `logos`/`pest` lexer-first design that cannot nest comments and interpolations. Pest is allowed only if the grammar is character-based and matches ABNF (usually harder than descent).
- Functions named after ABNF rules.

## Numbers, text, time

| Dhall | Rust |
|---|---|
| Bool | `bool` |
| Natural | `num_bigint::BigUint` |
| Integer | `num_bigint::BigInt` |
| Double | `f64` |
| Text | `String` |
| Bytes | `Vec<u8>` |
| Date | `time::Date` or `chrono::NaiveDate` |
| Time | time-of-day + `u32` precision |
| TimeZone | minutes east of UTC (`i32`) |

**Never** use `u64`/`i64` as the only Natural/Integer. B37 requires `2^64`.

`f64`: equality via CBOR, not `==` (NaN). `-0.0` vs `+0.0` follows `binary.md`.

## CBOR (B09–B10)

Implement terms then serialize. `ciborium`, `serde_cbor`, or `minicbor` may emit bytes **if** you control major types, bignums (tag 2/3), and float widths per `binary.md`. Do not `#[derive(Serialize)]` the AST and hope.

SHA-256: `sha2::Sha256`.

## HTTP client and test server (B32)

**Client:** `reqwest` (blocking is simpler for a naive resolver) or `ureq`. HTTPS tests: accept the self-signed cert (`danger_accept_invalid_certs` **only** in the test resolver config, not production defaults). Implement CORS in **your** import logic using headers; reqwest will not do Dhall CORS for you.

**Server:** `hyper`/`axum`/`warp` or even `tiny_http` + `rustls`/`native-tls` for `:18443`. Bind `127.0.0.1:18080` and `:18443`. Implement `tests/README.md` routes. Serve files with `\n` newlines. Start with `tokio::spawn` or a thread in `#[tokio::test]` / suite setup.

Do not invoke the Haskell `dhall-test-server` binary.

## Errors

`thiserror` or a hand-written `enum Error { Parse, Type, Import { soft: bool, .. } }`. Use `#[ignore]` only if a slice says skip; type-inference failures: `#[timeout]` via `ntest` or a watchdog thread.

## Bindings (B36–B40)

```rust
struct Decoder<T> { expected_type: Expr, extract: fn(&Expr) -> Result<T, DecodeError> }
// or a trait:
trait FromDhall: Sized {
    fn dhall_type() -> Expr;
    fn from_expr(e: &Expr) -> Result<Self, DecodeError>;
}
trait ToDhall {
    fn dhall_type() -> Expr;
    fn to_expr(&self) -> Expr;
}
```

A trait is closer to `serde_dhall`; keep extraction **after** type-check+normalize.

| Dhall | Rust |
|---|---|
| Bool, Text, Bytes | `bool`, `String`, `Vec<u8>` |
| Natural, Integer | `BigUint`, `BigInt` |
| Double | `f64` |
| Optional a | `Option<A>` |
| List a | `Vec<A>` |
| records | `struct` with fields named as labels (`r#type` if needed) |
| unions | `enum { Left(A), Right }` (empty alt = variant without payload) |
| `A → B` | a struct `DhallFn<A,B>` implementing `Fn(A) -> Result<B, _>` via encode/apply/normalize/decode |

Do not default-decode records from `HashMap<String, Value>`. Extra/missing fields: error.

`#[derive(FromDhall)]` is optional (B40). Encoding Rust closures to Dhall is optional.

## CLI (B34)

`clap` or `argh`. stdin / `--file`. `std::process::ExitCode`.

## Do not

- `unwrap` on parse errors in the library API (tests may unwrap)
- `serde_json` as Dhall
- Re-export `serde_dhall`
