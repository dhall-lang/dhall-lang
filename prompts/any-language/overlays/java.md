# Overlay — Java 17+

Read after `prompts/any-language/00-shared.md` on every Java set-B session.

## Toolchain

- Java **17 or newer** (sealed types, records). Prefer 21 LTS.
- Build: Gradle (Kotlin DSL) or Maven. One module is enough until B36; bindings may be the same artifact (`io.dhall:dhall`).
- Tests: JUnit 5. Parameterize over files under `DHALL_LANG_TESTS` (absolute path to `dhall-lang/tests`).
- Encoding: UTF-8 everywhere (`StandardCharsets.UTF_8`). Do not use the platform default charset.
- Command to run the language suite (adapt): `./gradlew test` with `DHALL_LANG_TESTS` set.

## Forbidden implementations

Do **not** implement Dhall by wrapping:

- `dhall-haskell` / JNI / `clay-dhall`
- Eta `dhall-eta`
- Shelling out to the `dhall` executable except in one-off debugging

You may read those projects for ideas. Do not paste them.

## Project layout

```text
src/main/java/io/dhall/{syntax,parser,binary,norm,typecheck,import_,hash,bind}/
src/test/java/io/dhall/suite/          # walks DHALL_LANG_TESTS
src/test/java/io/dhall/bind/           # B36–B40
```

`import` is a keyword: use package `import_` or `imports`. Module names should still map to B00 (`Syntax`, `Parser`, …).

Environment (import tests, B28+):

- `DHALL_LANG_TESTS` — root of the acceptance tree
- `HOME` → `tests/import/home`
- `XDG_CACHE_HOME` → `tests/import/cache` (copy to a temp dir if the suite might write)
- `DHALL_TEST_VAR` = `6 * 7`

## AST (B01)

Use a **sealed interface** `Expr` with `record` implementations, one per `syntax.md` constructor. Recursion via `Expr` fields (the JVM heap, no `Box`). Lists: `List<Expr>` or `record NonEmptyList(Expr head, List<Expr> tail)`.

`ImportMode` enum: `CODE`, `TEXT`, `BYTES`, `LOCATION`, `SOURCE`.

Do not use `Object` as the AST. Do not encode Dhall as Jackson `JsonNode`.

## Parser (B02–B08)

- Recursive descent (or combinators) on a **code-point** cursor into a `String` / `char[]` interpreted as UTF-16 with proper surrogate handling, or decode to `int[]` code points. `char` is not a Unicode scalar.
- **No** ANTLR/JavaCC **lexer**. Nested `{- -}` and `"${…}"` make tokenizers wrong (`dhall.abnf` comments).
- Alternatives: try in ABNF order; backtrack on failure (`int mark` / restore).
- Name methods after ABNF rules (`completeExpression`, `importHashed`).

## Numbers, text, time

| Dhall | Java |
|---|---|
| Bool | `boolean` in bindings; AST may use a `record BoolLit(boolean v)` |
| Natural | `BigInteger` ≥ 0 (reject sign in the value) |
| Integer | `BigInteger` |
| Double | `double` (IEEE-754). Compare via CBOR, not `==` |
| Text | `String` (UTF-16 Java string holding Unicode) |
| Bytes | `byte[]` |
| Date | `java.time.LocalDate` |
| Time | `LocalTime` **plus** an `int precision` (fractional-second digits as in the spec) |
| TimeZone | `ZoneOffset` (minutes) |

**Never** store Natural/Integer only in `long`. `2^64` must work (B37).

## CBOR (B09–B10)

Implement Dhall’s subset yourself or use a low-level library (e.g. Jackson `CBORFactory`, Java CBOR-canonical encoders) **only** as a byte sink. Map/int/float/bignum layout must follow `binary.md`, not Jackson defaults.

NaN: compare encoded bytes or use a NaN-aware term equality. Do not use `Double.equals` as Dhall equality.

SHA-256: `MessageDigest.getInstance("SHA-256")`.

## Equivalence and hashing

`equivalent` = CBOR bytes of `encode(alpha(beta(e)))`. Semantic hash fixtures are `sha256:` + hex, not the `1220` cache filename prefix.

## HTTP client and test server (B32)

**Client (import resolver):** `java.net.http.HttpClient` (HTTP/1.1). For HTTPS tests, trust the self-signed cert from the test server (custom `TrustManager` **only** in tests). Follow CORS using response headers; do not use a browser.

**Test server:** `com.sun.net.httpserver.HttpServer` on `127.0.0.1:18080` and `HttpsServer` on `127.0.0.1:18443`, **or** Jetty/Netty with the same contract. Implement every endpoint in `tests/README.md`. Serve `tests/import/**` with Unix newlines and `Access-Control-Allow-Origin: *`. Use the same self-signed PEM pair as dhall-test-server (copy certs into the Java test resources). JUnit `@BeforeAll` start / `@AfterAll` stop.

Do not call out to Ruby or the Haskell warp server.

## Errors

A single hierarchy is enough: `ParseException`, `TypeException`, `ImportException` (soft vs hard as in the import notes). Type-inference **failure** tests: JUnit timeout (e.g. 5–30s).

## Bindings (B36–B40)

```text
record Decoder<A>(Expr expectedType, Function<Expr, A> extract)
record Encoder<A>(Expr dhallType, Function<A, Expr> inject)
```

Or a pair of interfaces `Decoder<A>` / `Encoder<A>` with the same data.

| Dhall | Java binding |
|---|---|
| Bool | `boolean` / `Boolean` |
| Text | `String` |
| Bytes | `byte[]` |
| Natural, Integer | `BigInteger` |
| Double | `double` |
| Optional a | `Optional<A>` |
| List a | `List<A>` (`ArrayList`) |
| `{ x : T, y : U }` | `record R(T x, U y)` or a class with fields **named** `x`, `y` |
| `< L : A \| R >` | sealed interface with `record L(A value)` and `record R()` for empty alts |
| `A → B` | `Function<A, B>` that encodes, applies, normalizes, decodes (B39) |

Records: extra/missing fields are errors (B38). Do not use `Map<String,Object>` as the default record binding.

Encoding host `Function` values back to Dhall is optional (B40).

Reflection (`Record` components, annotations) is optional and must use Dhall labels, not `getX` bean renaming unless you document a mapping.

## CLI (B34)

`main` reading stdin or `--file`. Flags are free. Exit non-zero on parse/type/import errors. Picocli is optional.

## Do not

- Use `int`/`long` as the only integer representation
- Parse Dhall with regex
- Treat Dhall as JSON
- Enable parser tests as “success” without comparing `.dhallb`
