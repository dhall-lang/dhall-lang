# Overlay — Go 1.22+

Read after `prompts/any-language/00-shared.md` on every Go set-B session.

## Toolchain

- Go **1.22+**. Module path e.g. `github.com/example/dhall`.
- Tests: `go test ./...`. Use `t.Setenv` for `HOME`, `XDG_CACHE_HOME`, `DHALL_TEST_VAR`.
- `DHALL_LANG_TESTS` for the suite root. Walk with `filepath.WalkDir`.
- Source files UTF-8. Dhall Text is Unicode: use `[]rune` or `utf8` iterators in the parser, not `range` over `string` as bytes when matching ABNF characters. `for _, r := range s` iterates runes (code points) — that is the correct unit, not `s[i]` bytes.

## Forbidden implementations

Do **not** copy or wrap [`dhall-golang`](https://github.com/philandstuff/dhall-golang). You may read it. Do not paste `parser.go` / `binary.go`. Do not shell out to `dhall`.

## Project layout

```text
syntax/expr.go
parser/
binary/
norm/          # shift, subst, alpha, beta
typecheck/
imports/
hash/
bind/
suite_test.go  # or internal/suite
bind/bind_test.go
```

Package `imports` not `import`. Map to B00 module names in comments if packages differ.

## AST (B01)

```go
type Expr struct { // or an interface with typed structs
}
```

Prefer a tagged struct or an `interface { isExpr() }` with one concrete type per constructor (`type Lambda struct { ... }`). Recursion via pointers (`*Expr`) to avoid copies.

```go
type ImportMode int
const (
    ImportCode ImportMode = iota
    ImportText
    ImportBytes
    ImportLocation
    ImportSource
)
```

Do not use `map[string]any` or `json.RawMessage` as the AST.

## Parser (B02–B08)

- Recursive descent with an `int` rune offset into `[]rune` (or a `decoder` over the string).
- **No** `text/scanner` / `go/scanner` as a Dhall lexer. Nested comments and interpolations will be wrong.
- Backtracking: save offset, restore on failed alternative. Try ABNF alternatives in order.
- Name functions after ABNF rules (`CompleteExpression`, `importHashed`).

## Numbers, text, time

| Dhall | Go |
|---|---|
| Bool | `bool` |
| Natural | `*big.Int` with `Sign() >= 0` |
| Integer | `*big.Int` |
| Double | `float64` |
| Text | `string` (UTF-8) |
| Bytes | `[]byte` |
| Date | `time.Time` in UTC date-only, or a small `Date{Y,M,D}` |
| Time | clock + `Precision int` |
| TimeZone | `int` minutes |

**Never** use `uint64`/`int64` as the only integer type. `math/big` is required for B37 (`2^64`).

Equality of Doubles: via CBOR, not `==`.

## CBOR (B09–B10)

Follow `binary.md`. `fxamacker/cbor` or `github.com/fxamacker/cbor/v2` can write items if you set encodings yourself. Do not `cbor.Marshal` the AST blindly.

SHA-256: `crypto/sha256`.

## HTTP client and test server (B32)

**Client:** `net/http`. For HTTPS tests, `TLSClientConfig{InsecureSkipVerify: true}` **only** on the test/import-test client (or pin the test cert). Implement Dhall CORS in the resolver.

**Server:** `net/http` + `http.ListenAndServeTLS` on `127.0.0.1:18443` with the test PEM files; HTTP on `:18080`. `httptest.Server` cannot bind those fixed ports — use a real listener. Implement `tests/README.md` routes. `httptest` is fine for unit tests of handlers, not as a replacement for the contract ports.

Do not exec the Haskell test server.

Windows: import tests still need `HOME`-like behavior; the suite documents `%USERPROFILE%`. Prefer running import tests on Unix first.

## Errors

```go
type ParseError struct { ... }
type TypeError struct { ... }
type ImportError struct { Soft bool; ... }
```

Type-inference failure tests: `context.WithTimeout` around `Infer`.

## Bindings (B36–B40)

Go has no generics-as-typeclasses like Haskell. Use:

```go
type Decoder[T any] struct {
    Type  Expr
    Extract func(Expr) (T, error)
}
```

and concrete helpers `DecodeBool`, `DecodeNatural`, `RecordDecoder`, `UnionDecoder`. Optional `Bind` via `reflect` (struct tags `` `dhall:"x"` ``) must still reject extra/missing fields.

| Dhall | Go |
|---|---|
| Bool, Text, Bytes | `bool`, `string`, `[]byte` |
| Natural, Integer | `*big.Int` |
| Double | `float64` |
| Optional a | `*T` or a `Optional[T]` with `Ok bool` — **pick one** and use it everywhere (`None` is distinct from missing) |
| List a | `[]T` |
| records | `struct { X *big.Int \`dhall:"x"\` }` with **exported** fields; document the label mapping |
| unions | `struct { Tag string; Left *big.Int; Right *string }` or a small sum type; empty alt: tag only |
| `A → B` | `func(A) (B, error)` closing over encode/apply/normalize/decode |

`encoding/json` tags are not Dhall. Encoding Go `func` values to Dhall is optional (B40).

## CLI (B34)

`flag` or `cobra`. stdin / `-file`. `os.Exit(1)` on errors.

## Do not

- `interface{}` / `any` as Expr
- `json.Unmarshal` Dhall
- Copy `dhall-golang` files
