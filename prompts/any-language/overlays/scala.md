# Overlay — Scala 3

Read after `prompts/any-language/00-shared.md` on every Scala set-B session.

## Toolchain

- **Scala 3.3+** (enums, significant indentation optional). Do not start a new implementation on Scala 2.
- Build: `sbt` or `scala-cli` / Mill. Test: MUnit or weaver; JUnit is fine.
- UTF-8 source and file IO (`scala.io.Codec.UTF8` / `java.nio.charset.StandardCharsets.UTF_8`).
- Point the suite at `DHALL_LANG_TESTS`.

## Forbidden implementations

Do not wrap `dhall-haskell`, JNI, or the `dhall` CLI as the implementation. There is no standard Scala Dhall to copy; do not paste `dhall-haskell` via Scala Native tricks.

## Project layout

```text
src/main/scala/io/dhall/{syntax,parser,binary,norm,typecheck,imports,hash,bind}/
src/test/scala/io/dhall/suite/
src/test/scala/io/dhall/bind/
```

Packages may be `dhall.*` if you prefer. Map modules to B00 names.

Import-test env: `HOME`, `XDG_CACHE_HOME`, `DHALL_TEST_VAR` as in `00-shared.md`.

## AST (B01)

`enum Expr` (Scala 3) with nested cases, or a `sealed trait` + `case class`es. Recursion is ordinary; use `List[Expr]`, `Vector[Expr]`, or `::` for non-empty lists.

`enum ImportMode { case Code, Text, Bytes, Location, Source }`

Do not use `ujson.Value` / Circe `Json` as the Dhall AST.

## Parser (B02–B08)

- Recursive descent or **cats-parse** / **fastparse** on a **code-point** view of the string. Fastparse is acceptable if alternatives are ordered like the ABNF and you can backtrack; do not use a separate lexer pass.
- **No** lexer generators (lex, scala-lexer) for Dhall.
- Name parsers after ABNF rules.

## Numbers, text, time

| Dhall | Scala |
|---|---|
| Bool | `Boolean` |
| Natural | `BigInt` with a runtime `>= 0` check, or a `opaque type Natural = BigInt` |
| Integer | `BigInt` |
| Double | `Double` |
| Text | `String` |
| Bytes | `IArray[Byte]` or `Array[Byte]` |
| Date | `java.time.LocalDate` |
| Time | `LocalTime` + `precision: Int` |
| TimeZone | `ZoneOffset` |

Do not use `Long` as Natural/Integer. `2^64` must decode (B37).

## CBOR (B09–B10)

Follow `binary.md`. Libraries (borer, jackson-cbor, scodec-bits) may write bytes; **term layout** is your code. Compare parser tests to `.dhallb` bytes. NaN-safe comparison.

SHA-256: `java.security.MessageDigest` or `com.google.crypto.tink` / `fs2.hashing` — any correct SHA-256.

## HTTP client and test server (B32)

**Client:** `java.net.http.HttpClient` or http4s Ember. Trust the test self-signed cert only in tests.

**Server:** http4s, Pekko HTTP, or JDK `HttpServer`/`HttpsServer`. Contract is `tests/README.md` (`127.0.0.1:18080` / `:18443`). Unix newlines. Start in suite fixture, stop after.

Do not shell out to the Haskell test server.

## Errors

`enum DhallError` or a `sealed trait` with parse / type / import (soft vs hard). MUnit timeout on type-inference failure tests.

## Bindings (B36–B40)

```scala
trait Decoder[A]:
  def expectedType: Expr
  def extract(e: Expr): Either[DecodeError, A]

trait Encoder[A]:
  def dhallType: Expr
  def inject(a: A): Expr
```

Typeclass instances are idiomatic. Optional generic derivation (`Magnolia`, `shapeless` is Scala 2 — avoid) must use Dhall field names.

| Dhall | Scala |
|---|---|
| Bool, Text, Bytes | `Boolean`, `String`, `Array[Byte]` |
| Natural, Integer | `BigInt` (Natural ≥ 0) |
| Double | `Double` |
| Optional a | `Option[A]` |
| List a | `List[A]` or `Vector[A]` (pick one and keep it) |
| records | `case class` with parameters named as Dhall labels |
| unions | `enum` / `sealed trait` with one case per alternative; empty alts are nullary cases |
| `A → B` | `A => B` wrapping encode/apply/normalize/decode |

Missing/extra record fields: error. Encoding Scala functions to Dhall is optional (B40).

## CLI (B34)

`main` on stdin/`--file`. decline / scopt optional. Non-zero exit on errors.

## Do not

- Scala 2-only code as the new implementation
- `Any` as the AST
- JSON libraries as Dhall
- Silent `toInt` truncation
