# Overlay — Erlang / OTP 26+

Read after `prompts/any-language/00-shared.md` on every Erlang set-B session.

## Toolchain

- **OTP 26+**. Rebar3 or Mix is not required; prefer **Rebar3** + `eunit`
  or `common_test`. Elixir is a different overlay (not this file).
- `DHALL_LANG_TESTS`. File IO: UTF-8 binaries. Parser: iterate Unicode
  **code points** with `unicode:characters_to_nfc_list/1` or
  `string:next_codepoint` on a UTF-8 binary. Do not treat a `string()`
  (list of code points) and a UTF-8 `binary()` as interchangeable without
  conversion. Prefer one representation internally (code-point lists for
  the parser, UTF-8 binaries for Text values).

Erlang **integers are unbounded** — use them for Natural and Integer.

## Forbidden implementations

No standard Erlang Dhall. Do not `open_port` to the `dhall` CLI. Do not
embed Haskell via NIFs as the implementation.

## Project layout

```text
src/dhall_syntax.erl
src/dhall_parser.erl
src/dhall_binary.erl
src/dhall_shift.erl dhall_subst.erl dhall_alpha.erl dhall_beta.erl
src/dhall_typecheck.erl dhall_import.erl dhall_bind.erl
test/dhall_suite_tests.erl
test/dhall_bind_tests.erl
```

Import-test env: `os:putenv` for `HOME`, `XDG_CACHE_HOME`,
`DHALL_TEST_VAR` in the suite `init_per_suite`.

## AST (B01)

Tagged tuples (idiomatic) or maps with a `tag` key. Tuples are enough:

```erlang
{lam, Name, Type, Body}
{natural, N}          %% integer() >= 0
{import, Type, Mode, Hash}
```

```erlang
-type import_mode() :: code | text | bytes | location | source.
```

Do not use `jsx` JSON terms as the AST.

## Parser (B02–B08)

- Recursive descent over a list of code points or a binary+index that
  decodes UTF-8 one scalar at a time.
- Backtrack: save the rest of the input. Try ABNF alternatives in order
  (`case` / nested functions).
- **No** `leex`/`yecc` **lexer** for Dhall. Nested comments and
  interpolations.
- Function names ≈ ABNF (`complete_expression/1`).

## Numbers, text, time

| Dhall | Erlang |
|---|---|
| Bool | `true` / `false` atoms |
| Natural | `integer()` ≥ 0 |
| Integer | `integer()` |
| Double | `float()` (IEEE-754 on typical BEAM) |
| Text | UTF-8 `binary()` |
| Bytes | `binary()` (raw) — keep Text vs Bytes distinct in the AST |
| Date | `{Y,M,D}` |
| Time | `{H,Min,S, Prec}` |
| TimeZone | minutes integer |

`2^64` is a normal Erlang integer (B37). Double equality: via CBOR, not
`==` / `=:=` on floats.

## CBOR (B09–B10)

Implement Dhall’s CBOR subset on binaries (`<<>>`). Libraries like
`cbor` on Hex may write items; **layout** follows `binary.md`. SHA-256:
`crypto:hash(sha256, Bin)`.

## HTTP client and test server (B32)

**Client:** `httpc` (`inets`) or `gun`. HTTPS tests: `ssl` options that
accept the test cert **only** in tests (`verify_none` scoped to the test
client). Implement CORS in the resolver.

**Server:** `cowboy` (or `inets` httpd) on `{127,0,0,1}:18080` and TLS
`:18443` with the test PEM. Implement `tests/README.md`. Start in
`init_per_suite`, stop in `end_per_suite`. Bodies: `<<"...\n">>` Unix
newlines.

Do not start the Haskell test server.

## Errors

```erlang
{error, {parse, Msg}}
{error, {typecheck, Msg}}
{error, {import, soft | hard, Msg}}
```

Type-inference failures: `timer:kill_after` / `ct:timetrap`.

## Bindings (B36–B40)

Erlang has no static generics. Provide functions:

```erlang
decode_bool(Expr) -> {ok, boolean()} | {error, _}.
decode_natural(Expr) -> {ok, integer()} | {error, _}.
```

and a record decoder that takes a list of `{Label, Decoder}` and returns
a `map()` with **exactly** those keys.

| Dhall | Erlang |
|---|---|
| Bool | `boolean()` |
| Text, Bytes | UTF-8 `binary()`, raw `binary()` |
| Natural, Integer | `integer()` |
| Double | `float()` |
| Optional a | `{some, V}` / `none` **or** `undefined` — **pick one** (`none` tagged is clearer than `undefined`) |
| List a | `[T]` |
| records | `#{ <<"x">> => Nat, <<"y">> => Bool }` with binary keys = labels, exact keys |
| unions | `{left, Nat}` / `{empty_alt}` tagged tuples; tag = alternative name |
| `A → B` | `fun((A) -> {ok, B} | {error, _})` via encode/apply/normalize/decode |

Extra/missing map keys: error. Encoding funs to Dhall is optional (B40).

## CLI (B34)

`escript` or a `dhall` release. stdin / `-file`. halt with status 1.

## Do not

- Confuse `string()` lists with UTF-8 binaries
- `list_to_integer` size limits (there are none — good)
- `jsx:decode` Dhall
