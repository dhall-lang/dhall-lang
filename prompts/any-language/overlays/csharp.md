# Overlay — C# / .NET 8

Read after `prompts/any-language/00-shared.md` on every C# set-B session.

## Toolchain

- **.NET 8** (or 9). SDK-style `csproj`. LangVersion `latest` (records,
  file-scoped namespaces, raw string literals).
- Tests: xUnit or NUnit. `DHALL_LANG_TESTS`.
- Always `Encoding.UTF8`. Parser iterates **`Rune`** / `StringInfo` is
  grapheme-based — **use `Rune` / `EnumerateRunes()`**, not `foreach (char
  c)` (UTF-16 code units) and not grapheme clusters.

```csharp
foreach (var rune in s.EnumerateRunes()) { ... }
```

## Forbidden implementations

No standard C# Dhall. Do not P/Invoke `libdhall`, wrap the `dhall` CLI, or
paste dhall-haskell via `DllImport`.

## Project layout

```text
src/Dhall/Syntax.cs
src/Dhall/Parser.cs
src/Dhall/Binary.cs
src/Dhall/Shift.cs Substitute.cs Alpha.cs Beta.cs
src/Dhall/TypeCheck.cs Import.cs Bind.cs
tests/Dhall.Suite/
tests/Dhall.Bind/
```

Namespaces `Dhall.Syntax`, … matching B00. Import-test env:
`HOME`/`USERPROFILE`, `XDG_CACHE_HOME`, `DHALL_TEST_VAR`.

## AST (B01)

C# has no built-in DUs until you pick a style. Prefer:

```csharp
abstract record Expr;
record Lam(string Name, Expr Type, Expr Body) : Expr;
record NaturalLit(BigInteger N) : Expr;
```

or `OneOf` / a tagged `record Expr(Kind K, ...)`. Recursion is ordinary
references.

```csharp
enum ImportMode { Code, Text, Bytes, Location, Source }
```

Do not use `JsonNode` / `dynamic` as the AST.

## Parser (B02–B08)

- Recursive descent with an integer index into the string, advancing by
  `Rune`. Backtrack by saving the index. ABNF order.
- **No** `System.Text.RegularExpressions` as the parser. **No**
  Sprache/Superpower **token** lexers that break nested comments.
- Sprache/Parlot *on runes* is acceptable if backtracking matches ABNF.

## Numbers, text, time

| Dhall | C# |
|---|---|
| Bool | `bool` |
| Natural | `BigInteger` ≥ 0 (`System.Numerics`) |
| Integer | `BigInteger` |
| Double | `double` |
| Text | `string` |
| Bytes | `byte[]` |
| Date | `DateOnly` (.NET 6+) |
| Time | `TimeOnly` + `int Precision` |
| TimeZone | `TimeSpan` offset or minutes `int` |

**Never** use `long`/`ulong` as the only integer type. B37: `BigInteger.Pow(2, 64)`.

Double equality: via CBOR, not `==`.

## CBOR (B09–B10)

`System.Formats.Cbor` (`CborWriter` / `CborReader`) is a good byte sink.
You still implement Dhall term layout from `binary.md` (bignum tags, float
widths). Do not `JsonSerializer` the AST to CBOR.

SHA-256: `SHA256.HashData`.

## HTTP client and test server (B32)

**Client:** `HttpClient`. For HTTPS tests, `HttpClientHandler` with a
custom validator that accepts the test cert **only** in tests. Implement
CORS in the resolver.

**Server:** Kestrel (`Microsoft.AspNetCore.Server.Kestrel`) or
`HttpListener` bound to `127.0.0.1:18080` and HTTPS `:18443` with the
test certificate. Implement `tests/README.md`. xUnit `IAsyncLifetime`
start/stop. Unix `\n`.

Do not start the Haskell test server.

## Errors

```csharp
class ParseException : Exception;
class TypeCheckException : Exception;
class ImportException : Exception { public bool Soft { get; } }
```

Type-inference failures: `CancellationTokenSource` timeout.

## Bindings (B36–B40)

```csharp
sealed record Decoder<T>(Expr ExpectedType, Func<Expr, T> Extract);
```

| Dhall | C# |
|---|---|
| Bool, Text, Bytes | `bool`, `string`, `byte[]` |
| Natural, Integer | `BigInteger` |
| Double | `double` |
| Optional a | `T?` for ref types; `Nullable<T>` for structs — **document** `None` |
| List a | `IReadOnlyList<T>` / `List<T>` |
| records | `record R(BigInteger X, bool Y)` with names = labels |
| unions | abstract record + one nested record per alternative; empty alt = empty record |
| `A → B` | `Func<A, T>` / `Func<A, Task<B>>` via encode/apply/normalize/decode |

System.Text.Json attributes are not Dhall. Extra/missing fields: error.
Encoding C# delegates to Dhall is optional (B40). Source generators for
records are optional.

## CLI (B34)

`System.CommandLine` optional. stdin / `--file`. `return 1`.

## Do not

- `foreach (char c in s)` as the parser
- `int` as Natural
- `JsonDocument` as Dhall
