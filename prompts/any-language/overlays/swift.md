# Overlay — Swift 5.9+

Read after `prompts/any-language/00-shared.md` on every Swift set-B session.

## Toolchain

- Swift **5.9+** (or Swift 6). Package: SwiftPM (`Package.swift`).
- Tests: XCTest or **Swift Testing**. `DHALL_LANG_TESTS` for the suite root.
- macOS/Linux: Linux is supported via Swift.org toolchains; HTTPS tests need
  a TLS stack (`NIOSSL` / system SecureTransport).
- Files: UTF-8. **Parser unit is Unicode scalar (`Unicode.Scalar`), not
  `Character`.** `Character` is an extended grapheme cluster (`é` as one
  `Character` vs `e` + combining accent). ABNF counts scalars/code points.

```swift
for scalar in string.unicodeScalars { ... }
```

## Forbidden implementations

No standard Swift Dhall. Do not wrap the `dhall` CLI, WASM-haskell, or
JNI. Do not paste `dhall-haskell` via Swift–C interop as the library.

## Project layout

```text
Sources/Dhall/Syntax.swift
Sources/Dhall/Parser.swift
Sources/Dhall/Binary.swift
Sources/Dhall/Normalize.swift   // shift, subst, alpha, beta
Sources/Dhall/TypeCheck.swift
Sources/Dhall/Import.swift
Sources/Dhall/Bind.swift
Tests/DhallSuiteTests/
Tests/DhallBindTests/
```

Import-test env: `HOME`, `XDG_CACHE_HOME`, `DHALL_TEST_VAR` via
`setenv` in test `setUp` (process-wide — serialize import tests if needed).

## AST (B01)

```swift
indirect enum Expr {
  case lam(String, Expr, Expr)
  case natural(BigUInt) // see integers
  ...
}
enum ImportMode { case code, text, bytes, location, source }
```

`indirect` is required for recursion. Do not use `[String: Any]` JSON.

## Parser (B02–B08)

- Recursive descent on `String.UnicodeScalarView` + an index.
- Backtrack by saving the index. Alternatives in ABNF order.
- **No** `Scanner` / token lexer. Nested `{- -}` and interpolations.

## Numbers, text, time

Swift has **no** unbounded integer in the standard library.

| Dhall | Swift |
|---|---|
| Bool | `Bool` |
| Natural | `BigUInt` (**Attaswift BigInt** or equivalent) |
| Integer | `BigInt` |
| Double | `Double` |
| Text | `String` |
| Bytes | `Data` / `[UInt8]` |
| Date | `DateComponents` (Y-M-D) or a small struct |
| Time | clock + `Int` precision |
| TimeZone | minutes `Int` |

**Never** use `UInt64`/`Int` as the only Natural/Integer. B37 requires
`2^64`.

Double equality: via CBOR, not `==`.

Foundation `Date` is an instant, not a Dhall `Date` — do not confuse them.

## CBOR (B09–B10)

Implement Dhall terms then encode. Swift CBOR libs (`SwiftCBOR`,
`PotentCodables`) may write items; layout follows `binary.md`. Do not
`Codable` the AST and hope.

SHA-256: `CryptoKit` (`SHA256`) or `CommonCrypto`.

## HTTP client and test server (B32)

**Client:** `URLSession`. For HTTPS tests, a `URLSessionDelegate` that
accepts the test self-signed cert **only** in tests. Implement CORS in
the resolver.

**Server:** Hummingbird, Vapor, or `NIOHTTP1` + `NIOSSL` on
`127.0.0.1:18080` / `:18443`. Implement `tests/README.md`. XCTest
`setUpWithError` start / `tearDown` stop. Unix `\n` in bodies.

Do not spawn the Haskell test server.

## Errors

```swift
enum DhallError: Error {
  case parse(String)
  case typeCheck(String)
  case import(soft: Bool, String)
}
```

Type-inference failures: `XCTestExpectation` timeout or Swift Testing
time limits.

## Bindings (B36–B40)

```swift
struct Decoder<T> {
  var expectedType: Expr
  var extract: (Expr) throws -> T
}
```

| Dhall | Swift |
|---|---|
| Bool, Text, Bytes | `Bool`, `String`, `Data` |
| Natural, Integer | `BigUInt`, `BigInt` |
| Double | `Double` |
| Optional a | `T?` (`nil` = `None` when the decoder knows `T`) |
| List a | `[T]` |
| records | `struct` with properties named as labels |
| unions | `enum { case left(BigUInt); case emptyAlt }` |
| `A → B` | `(A) throws -> B` via encode/apply/normalize/decode |

`Codable` synthesis is not Dhall. Extra/missing fields: error. Encoding
Swift closures to Dhall is optional (B40).

## CLI (B34)

`swift run dhall`. `ArgumentParser` optional. stdin / `--file`. Exit 1
on errors.

## Do not

- Parse with `Character`
- `Int` as Natural
- `JSONDecoder` on `.dhall` files
