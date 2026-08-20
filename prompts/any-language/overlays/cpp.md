# Overlay — C++20

Read after `prompts/any-language/00-shared.md` on every C++ set-B session.

## Toolchain

- **C++20** (or C++23): `std::variant`, `concepts` optional, `std::span`, `char8_t` optional.
- CMake 3.20+ or Meson. Tests: Catch2 or GoogleTest.
- Compiler: GCC 12+ / Clang 16+ / MSVC 19.3+ with UTF-8 (`/utf-8` on MSVC).
- `DHALL_LANG_TESTS` environment variable. `std::filesystem` to walk tests.
- Source and Dhall files: UTF-8. Parser iterates **Unicode scalar values**, not `char` / not assuming UTF-32 `wchar_t`. Use a small UTF-8 decoder (or `icu` / `utf8cpp`). `operator[]` on `std::string` is a byte, not a Dhall character.

## Forbidden implementations

Do not wrap:

- [`clay-dhall`](https://github.com/as-capabl/clay-dhall) (Haskell as a `.so`)
- `dlopen` of `libdhall`
- `popen("dhall")`

You may read those projects. Do not paste them.

## Project layout

```text
include/dhall/{Syntax,Parser,Binary,...}.hpp
src/...
tests/suite.cpp
tests/bind.cpp
CMakeLists.txt
```

Namespaces: `dhall::syntax`, `dhall::parser`, … matching B00.

Import tests: set `HOME` / `XDG_CACHE_HOME` / `DHALL_TEST_VAR` in the test fixture (`setenv` / `SetEnvironmentVariable`). Copy cache if writable.

## AST (B01)

```cpp
struct Expr; // recursive
using ExprPtr = std::unique_ptr<Expr>; // or std::shared_ptr

struct Expr {
  struct Lam { std::string name; ExprPtr type; ExprPtr body; };
  struct Nat { boost::multiprecision::cpp_int n; /* ≥ 0 */ };
  std::variant<Lam, Nat, ...> v;
};
```

`std::variant` of non-recursive alternatives holding `ExprPtr` is the usual pattern. Do not use `void*` / `union` without tags.

```cpp
enum class ImportMode { Code, Text, Bytes, Location, Source };
```

Do not use `nlohmann::json` as the AST.

## Parser (B02–B08)

- Recursive descent with a `size_t` byte offset plus a `peekCodepoint()` helper, or decode the whole file to `std::u32string` once.
- Backtracking: save offset. Alternatives in ABNF order.
- **No** flex/bison **lexer**. Nested `{- -}` and interpolations.
- Boost.Spirit *character-level* Qi/X3 is allowed if alternatives are ordered and backtracking works; it is easy to accidentally tokenize — prefer hand-written descent if unsure.

## Numbers, text, time

| Dhall | C++ |
|---|---|
| Bool | `bool` |
| Natural | `boost::multiprecision::cpp_int` ≥ 0, or GMP `mpz` |
| Integer | `cpp_int` / `mpz` |
| Double | `double` |
| Text | `std::string` (UTF-8) |
| Bytes | `std::vector<std::uint8_t>` |
| Date | `std::chrono::year_month_day` (C++20) |
| Time | `hh_mm_ss` + `int precision` |
| TimeZone | `std::chrono::minutes` |

**Never** use `std::uint64_t` / `long long` as the only Natural/Integer. B37 requires `2^64`.

`double` equality: via CBOR, not `==`.

## CBOR (B09–B10)

Implement terms then serialize. `libcbor`, TinyCBOR, or QCBOR may write items; **structure** follows `binary.md`. Do not serialize `Expr` with cereal/protobuf.

SHA-256: OpenSSL `SHA256`, `cpp-sha256`, or `std::` from a crypto lib. Windows: BCrypt or the same OpenSSL.

## HTTP client and test server (B32)

**Client:** `libcurl`, Boost.Beast, or cpp-httplib. For HTTPS tests, disable peer verify **only** in the test/import-test client, or trust the test PEM. Implement CORS in the resolver, not in curl defaults.

**Server:** cpp-httplib, Beast, or `httplib.h` single header. HTTP `127.0.0.1:18080`, HTTPS `:18443` with test cert/key. Implement `tests/README.md`. Catch2 `TEST_CASE` listener or a RAII `ServerGuard`. Unix `\n` in bodies.

Link OpenSSL/LibreSSL for TLS. Do not exec the Haskell test server.

## Errors

```cpp
class parse_error : public std::runtime_error { ... };
class type_error  : public std::runtime_error { ... };
class import_error : public std::runtime_error {
  bool soft;
};
```

Prefer `std::expected` (C++23) or `tl::expected` in the library API instead of throwing on every recursive call if you want; throwing is acceptable if tests still catch failures.

Type-inference failures: run infer in a thread + `join_for` timeout, or Catch2 `BENCHMARK` is the wrong tool — use an explicit timeout.

## Bindings (B36–B40)

C++ has no built-in typeclasses. Provide:

```cpp
template<class T>
struct decoder {
  Expr expected_type;
  std::function<T(Expr const&)> extract;
};
```

and overloads `decode_bool`, `decode_natural`, plus macros/templates for aggregates if you want (not required).

| Dhall | C++ |
|---|---|
| Bool, Text, Bytes | `bool`, `std::string`, `std::vector<uint8_t>` |
| Natural, Integer | `cpp_int` |
| Double | `double` |
| Optional a | `std::optional<A>` |
| List a | `std::vector<A>` |
| records | `struct { cpp_int x; bool y; }` with a hand-written decoder listing field names as Dhall labels |
| unions | `std::variant<Left, Right>` or a tagged struct; empty alt = `std::monostate` alternative |
| `A → B` | `std::function<B(A)>` via encode/apply/normalize/decode |

Do not use `std::map<std::string, Expr>` as the default *host* record type (that is still Dhall, not a binding). Extra/missing fields: error.

Encoding C++ function objects to Dhall is optional (B40).

## CLI (B34)

`int main`. `CLI11` or `cxxopts` optional. stdin / `--file`. Return `1` on errors. Set UTF-8 console on Windows if you print Text.

## Do not

- `char` as Unicode
- nlohmann JSON as Dhall
- `atoi` / `stoll` as Natural
- Preprocessor as a parser
