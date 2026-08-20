# Overlay — F# / .NET 8

Read after `prompts/any-language/00-shared.md` on every F# set-B session.

## Toolchain

- **.NET 8+**, F# 8/9. `sdk` style `.fsproj`. Tests: Expecto, FsUnit+xUnit,
  or Unquote.
- `DHALL_LANG_TESTS`. UTF-8 IO.
- Parser: iterate **`System.Text.Rune`** / `s.EnumerateRunes()`, not
  `seq<char>` as UTF-16 units and not grapheme clusters.

F# is a good fit (ADTs). Do not implement Dhall by calling a C# Dhall
library you are writing in parallel unless that library *is* this
implementation (shared project). Do not wrap Hackage `dhall`.

## Forbidden implementations

Do not P/Invoke `libdhall` or shell out to `dhall`. No official F# Dhall
to copy.

## Project layout

```text
src/Syntax.fs
src/Parser.fs
src/Binary.fs
src/Shift.fs Substitute.fs Alpha.fs Beta.fs
src/TypeCheck.fs Import.fs Bind.fs
tests/Suite.fs
tests/Bind.fs
```

Modules map to B00. Import-test env: `HOME`/`USERPROFILE`,
`XDG_CACHE_HOME`, `DHALL_TEST_VAR`.

## AST (B01)

```fsharp
type ImportMode = Code | Text | Bytes | Location | Source

type Expr =
  | Lam of name: string * typ: Expr * body: Expr
  | NaturalLit of bigint  // see integers
  | ...
```

F# `bigint` is `System.Numerics.BigInteger` — unbounded. Use it for
Natural (with `>= 0I`) and Integer. Do not use `int64`.

Do not use `JsonValue` as the AST.

## Parser (B02–B08)

- Recursive descent or FParsec **on a rune stream**. If you use FParsec
  `CharStream`, know that `char` is UTF-16; for non-BMP scalars you must
  handle surrogates or decode to runes first. Safest: `ResizeArray<Rune>`
  or a custom stream.
- Backtracking: FParsec `<|>` / `attempt`. Alternative order = ABNF.
- **No** lexer pass. Nested comments and interpolations.

## Numbers, text, time

| Dhall | F# |
|---|---|
| Bool | `bool` |
| Natural | `bigint` ≥ 0I |
| Integer | `bigint` |
| Double | `float` (`double`) |
| Text | `string` |
| Bytes | `byte[]` |
| Date | `DateOnly` |
| Time | `TimeOnly` + `precision: int` |
| TimeZone | minutes `int` |

Double equality: via CBOR, not `=`.

## CBOR (B09–B10)

`System.Formats.Cbor` from F#. Term layout from `binary.md`. SHA-256:
`SHA256.HashData`.

## HTTP client and test server (B32)

**Client:** `HttpClient` (same as C# overlay). Test-only cert validator.
CORS in the resolver.

**Server:** Giraffe/Suave/Kestrel or `HttpListener` on `127.0.0.1:18080`
and HTTPS `:18443`. `tests/README.md`. Unix `\n`.

Do not start the Haskell test server.

## Errors

```fsharp
type Soft = bool
type DhallError =
  | Parse of string
  | TypeCheck of string
  | Import of Soft * string
```

Library: `Result<_, DhallError>`. Timeouts on type-inference failure tests.

## Bindings (B36–B40)

```fsharp
type Decoder<'a> = { ExpectedType: Expr; Extract: Expr -> Result<'a, DecodeError> }
```

| Dhall | F# |
|---|---|
| Bool, Text, Bytes | `bool`, `string`, `byte[]` |
| Natural, Integer | `bigint` |
| Double | `float` |
| Optional a | `'a option` |
| List a | `'a list` or `'a array` (pick one) |
| records | `{ x: bigint; y: bool }` with labels = Dhall names (backticks if needed) |
| unions | discriminated union; empty alt = nullary case |
| `A → B` | `'a -> Result<'b, _>` via encode/apply/normalize/decode |

Extra/missing record fields: error. Encoding F# functions to Dhall is
optional (B40).

## CLI (B34)

`Argu` or `System.CommandLine`. stdin / `--file`. `exit 1`.

## Do not

- `Seq.iter` on `seq<char>` as Unicode scalars
- `int` as Natural
- Newtonsoft JSON as Dhall
