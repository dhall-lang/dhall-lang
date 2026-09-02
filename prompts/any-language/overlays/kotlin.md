# Overlay — Kotlin (JVM 17+)

Read after `prompts/any-language/00-shared.md` on every Kotlin set-B session.

## Toolchain

- **Kotlin 2.x**, JVM **17+** (sealed classes, records interop). Gradle
  Kotlin DSL. Tests: JUnit 5. Kotlin Native/JS are out of scope unless you
  still run the HTTP tests; default is **JVM**.
- `DHALL_LANG_TESTS`. UTF-8 (`Charsets.UTF_8`). Parser: iterate
  **code points** (`Char` in Kotlin is UTF-16; use `codePoints()` /
  `forEachCodePoint` / a `String` iterator that handles surrogates).

```kotlin
fun String.codePointSeq(): Sequence<Int> = sequence {
    var i = 0
    while (i < length) {
        val cp = codePointAt(i)
        yield(cp)
        i += Character.charCount(cp)
    }
}
```

## Forbidden implementations

Do not wrap dhall-haskell JNI, the `dhall` CLI, or a Java Dhall library
unless that library **is** this project (shared JVM module). Do not paste
`dhall-haskell`.

## Project layout

```text
src/main/kotlin/io/dhall/syntax/
src/main/kotlin/io/dhall/parser/
src/main/kotlin/io/dhall/binary/
src/main/kotlin/io/dhall/norm/
src/main/kotlin/io/dhall/typecheck/
src/main/kotlin/io/dhall/imports/
src/main/kotlin/io/dhall/bind/
src/test/kotlin/io/dhall/suite/
src/test/kotlin/io/dhall/bind/
```

`import` is a keyword: package `imports`. Import-test env: `HOME`,
`XDG_CACHE_HOME`, `DHALL_TEST_VAR`.

## AST (B01)

```kotlin
sealed interface Expr {
    data class Lam(val name: String, val type: Expr, val body: Expr) : Expr
    data class NaturalLit(val n: BigInteger) : Expr
    ...
}
enum class ImportMode { Code, Text, Bytes, Location, Source }
```

Do not use `Any` / kotlinx.serialization JSON as the AST.

## Parser (B02–B08)

- Recursive descent on a code-point cursor. Backtrack. ABNF order.
- **No** ANTLR lexer. Nested comments and interpolations.
- Arrow functions named after ABNF rules.

## Numbers, text, time

| Dhall | Kotlin |
|---|---|
| Bool | `Boolean` |
| Natural | `BigInteger` ≥ 0 |
| Integer | `BigInteger` |
| Double | `Double` |
| Text | `String` |
| Bytes | `ByteArray` |
| Date | `java.time.LocalDate` |
| Time | `LocalTime` + `Int` precision |
| TimeZone | `ZoneOffset` |

**Never** use `Long` as the only integer type. B37: `BigInteger.TWO.pow(64)`.

Double equality: via CBOR, not `==`.

## CBOR (B09–B10)

Jackson CBOR, kotlinx.serialization CBOR, or a small writer — **term
layout** from `binary.md`. SHA-256: `MessageDigest`.

## HTTP client and test server (B32)

**Client:** Ktor client or `java.net.http.HttpClient`. Test-only trust of
the self-signed cert. CORS in the resolver.

**Server:** Ktor / `com.sun.net.httpserver` on `127.0.0.1:18080` and
HTTPS `:18443`. `tests/README.md`. JUnit `@BeforeAll`/`@AfterAll`. Unix
`\n`.

Do not start the Haskell test server.

## Errors

```kotlin
sealed class DhallException : RuntimeException()
class ParseException(...) : DhallException()
class TypeCheckException(...) : DhallException()
class ImportException(val soft: Boolean, ...) : DhallException()
```

Timeouts on type-inference failure tests (`assertTimeout`).

## Bindings (B36–B40)

```kotlin
data class Decoder<out A>(val expectedType: Expr, val extract: (Expr) -> A)
```

| Dhall | Kotlin |
|---|---|
| Bool, Text, Bytes | `Boolean`, `String`, `ByteArray` |
| Natural, Integer | `BigInteger` |
| Double | `Double` |
| Optional a | `A?` (`null` = `None` when type is known) |
| List a | `List<A>` |
| records | `data class R(val x: BigInteger, val y: Boolean)` names = labels |
| unions | `sealed interface` / `sealed class` with one type per alternative |
| `A → B` | `(A) -> B` via encode/apply/normalize/decode |

Extra/missing fields: error. kotlinx.serialization must still enforce
exact keys if you use it. Encoding Kotlin functions to Dhall is optional
(B40).

## CLI (B34)

`fun main`. Clikt optional. stdin / `--file`. `exitProcess(1)`.

## Do not

- `for (c in string)` as the only parser cursor
- `Int` as Natural
- `kotlinx.serialization.json.Json` as Dhall
