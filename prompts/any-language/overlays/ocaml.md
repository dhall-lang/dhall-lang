# Overlay — OCaml 4.14 / 5.x

Read after `prompts/any-language/00-shared.md` on every OCaml set-B session.

## Toolchain

- OCaml **4.14 or 5.x**. Build: **Dune**. Tests: Alcotest or Dune `cram`
  plus a driver. `DHALL_LANG_TESTS`.
- UTF-8 files. OCaml `string` is **bytes**. Parser must decode UTF-8 to
  **Uchar.t** (use `uutf` / `uuu` / `string` + a UTF-8 decoder). Do not
  treat `s.[i]` as a Dhall character.

```ocaml
(* iterate Uchar.t over a UTF-8 string *)
```

`int` is 63-bit (or 31-bit on some arch). **Do not** use `int` for
Natural/Integer. Use **Zarith** `Z.t`.

## Forbidden implementations

Do not wrap the `dhall` CLI or dhall-haskell via C stubs as the
implementation. If an `ocaml-dhall` exists, do not paste it; you may read
it.

## Project layout

```text
lib/syntax.ml
lib/parser.ml
lib/binary.ml
lib/shift.ml substitute.ml alpha.ml beta.ml
lib/typecheck.ml import.ml bind.ml
test/suite.ml
test/bind.ml
dune-project
```

Import-test env: `Unix.putenv` for `HOME`, `XDG_CACHE_HOME`,
`DHALL_TEST_VAR` at suite start.

## AST (B01)

```ocaml
type import_mode = Code | Text | Bytes | Location | Source

type expr =
  | Lam of string * expr * expr
  | Natural of Z.t
  | ...
```

Do not use `Yojson.t` as the AST.

## Parser (B02–B08)

- Recursive descent on a `Uchar.t Seq.t` or Angstrom on bytes **if** you
  decode UTF-8 inside (Angstrom is byte-oriented — easy to get wrong).
  Hand-written descent on a `uchar list` or indexed UTF-8 decoder is
  safer.
- Backtrack: save offset. Alternatives in ABNF order.
- **No** ocamllex **lexer** for Dhall. Nested comments and interpolations.
- Menhir is a **token** parser — do not lex then Menhir unless tokens are
  Unicode scalars *and* interpolations still work (they usually will not).
  Prefer descent.

## Numbers, text, time

| Dhall | OCaml |
|---|---|
| Bool | `bool` |
| Natural | `Z.t` ≥ 0 |
| Integer | `Z.t` |
| Double | `float` |
| Text | `string` (UTF-8) |
| Bytes | `string` or `bytes` — keep a distinct AST constructor from Text |
| Date | `{ y:int; m:int; d:int }` |
| Time | clock + `precision: int` |
| TimeZone | minutes `int` |

B37: `Z.(pow (of_int 2) 64)`. Double equality: via CBOR, not `=`.

## CBOR (B09–B10)

Implement Dhall CBOR on `bytes` / `Buffer`. `cbor` OPAM packages may help
as writers; **layout** from `binary.md`. SHA-256: `digestif` or
`mirage-crypto`.

## HTTP client and test server (B32)

**Client:** `cohttp-lwt-unix` / `httpun` / `curl` bindings. HTTPS tests:
`Tls` / `conduit` with a test-only authenticator that accepts the
self-signed cert. Implement CORS in the resolver.

**Server:** `cohttp-lwt-unix` server or `httpaf` on `127.0.0.1:18080` and
TLS `:18443`. `tests/README.md`. Alcotest `run` fixture start/stop. Unix
`\n`.

Do not start the Haskell test server.

Lwt vs Eio: pick one IO library for import + server and stay with it.

## Errors

```ocaml
type soft = bool
type error =
  | Parse of string
  | Typecheck of string
  | Import of soft * string
```

`('a, error) result`. Timeouts: `Lwt.pick` vs sleep, or `Unix.alarm`
where it works.

## Bindings (B36–B40)

```ocaml
type 'a decoder = { expected_type : expr; extract : expr -> ('a, decode_error) result }
```

| Dhall | OCaml |
|---|---|
| Bool, Text, Bytes | `bool`, `string`, `string`/`bytes` |
| Natural, Integer | `Z.t` |
| Double | `float` |
| Optional a | `'a option` |
| List a | `'a list` |
| records | `{ x: Z.t; y: bool }` field names = labels |
| unions | algebraic type; empty alt = nullary constructor |
| `A → B` | `'a -> ('b, _) result` via encode/apply/normalize/decode |

PPX deriving (`ppx_deriving`) is optional and must still reject extra
fields. Encoding OCaml functions to Dhall is optional (B40).

## CLI (B34)

`cmdliner` or `arg`. stdin / `--file`. `exit 1`.

## Do not

- `int` as Natural
- `s.[i]` as a Unicode character
- `Yojson` as Dhall
- ocamllex+Menhir as the default plan
