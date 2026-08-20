# Overlay — Haskell / GHC 9.4+

Read after `prompts/any-language/00-shared.md` on every Haskell set-B
session.

This overlay is for a **separate** Cabal/Stack project, not the literate
package in `dhall-lang/standard/` (that is set A).

## Toolchain

- GHC 9.4–9.10, Cabal 3.8+ or Stack. Package name e.g. `dhall-reference`
  (avoid `dhall` on Hackage / colliding with dhall-haskell).
- Tests: tasty + tasty-hunit (or hedgehog only for extras). Env:
  `DHALL_LANG_TESTS`.
- UTF-8: `Text` (`Data.Text`). Parser: `text` + megaparsec/attoparsec on
  `Text`, or a character stream of `Char` (Unicode scalar). Do not parse
  `ByteString` as Latin-1.

## Forbidden implementations

Do **not** copy or depend on:

- Hackage **`dhall`** (dhall-haskell) as the language implementation
- `standard` from this repo as a library (that is set A)
- `dhall-json` / `dhall-yaml` as a substitute for a parser

You may read dhall-haskell and `standard/` for ideas. Do not paste
`Dhall.Parser`, `Dhall.TypeCheck`, `Dhall.Import`, `Dhall.Binary`,
`Dhall.Normalize`, `Dhall.Eval`.

**Allowed copy (test infra only):** `dhall-test-server` (same exception as
set A slice A20), with fixture paths pointed at `dhall-lang/tests/import`.

## Project layout

```text
src/Dhall/Syntax.hs
src/Dhall/Parser.hs
src/Dhall/Binary.hs
src/Dhall/Shift.hs Substitute.hs Alpha.hs Beta.hs
src/Dhall/Equiv.hs FunctionCheck.hs TypeCheck.hs
src/Dhall/Import.hs Hash.hs
src/Dhall/Bind.hs          -- B36–B40; do not import Dhall.Marshal from Hackage
test/Suite.hs
test/Bind.hs
```

Use different module names if you want to avoid clashing with Hackage
`Dhall.*` when both are in the same package db (`Ref.Syntax`, etc.).

Import-test env: `HOME`, `XDG_CACHE_HOME`, `DHALL_TEST_VAR` as in
`00-shared.md`.

## AST (B01)

```haskell
data Expr
  = Lam Text Expr Expr
  | NaturalLit Natural
  | ...
data ImportMode = Code | RawText | RawBytes | Location | Source
```

`Natural` / `Integer` from `base` (`Numeric.Natural`, `Integer`) are
unbounded — use them. Do not use `Int`/`Word64` as the only integer type.

Do not reuse Hackage `Dhall.Syntax.Expr`.

## Parser (B02–B08)

- Megaparsec / attoparsec / recursive descent on `Text`. Backtracking
  (`try`); alternative order = ABNF order.
- **No** Alex/Happy **lexer** for Dhall. Nested comments and interpolations.
- Function names may follow ABNF rules.

## Numbers, text, time

| Dhall | Haskell |
|---|---|
| Bool | `Bool` |
| Natural | `Natural` |
| Integer | `Integer` |
| Double | `Double` |
| Text | `Text` |
| Bytes | `ByteString` |
| Date | `Day` (`time`) |
| Time | `TimeOfDay` + `Int` precision |
| TimeZone | `TimeZone` or minutes `Int` |

Double equality: via CBOR (`binary.md`), not `(==)` (NaN).

## CBOR (B09–B10)

`cborg` `Codec.CBOR.Term` is appropriate (the spec package uses it). You
must still implement **Dhall** `encode`/`decode` judgments; do not
`serialise` a `Generic` AST. SHA-256: `cryptohash-sha256` or `cryptonite`
/`crypton`.

## HTTP client and test server (B32)

**Client:** `http-client` + `http-client-tls`. In tests, accept the
self-signed cert. Implement CORS in the resolver.

**Server:** vendor `dhall-test-server` or reimplement with `warp` /
`warp-tls` on `127.0.0.1:18080` / `:18443`. Contract: `tests/README.md`.

## Errors

`ExceptT` / `Either` with parse, type, import (`soft :: Bool`). Tasty
timeouts on `type-inference/failure`.

## Bindings (B36–B40)

You **may** follow the *shape* of `FromDhall`/`ToDhall` but must write
your own classes in this package:

```haskell
data Decoder a = Decoder { expectedType :: Expr, extract :: Expr -> Either DecodeError a }
class FromDhall a where decoder :: Decoder a
class ToDhall a where encoder :: Encoder a
```

Do not `build-depends: dhall` for `Dhall.Marshal`.

| Dhall | Haskell |
|---|---|
| Bool, Text, Bytes | `Bool`, `Text`, `ByteString` |
| Natural, Integer | `Natural`, `Integer` |
| Double | `Double` |
| Optional a | `Maybe a` |
| List a | `[a]` or `Seq a` (pick one) |
| records | record datatypes with field names = labels |
| unions | sum types; empty alt = nullary constructor |
| `A → B` | `a -> IO b` or `a -> Either e b` via encode/apply/normalize/decode |

`GHC.Generics` derivation is optional (B40). Encoding Haskell functions
to Dhall is optional.

## CLI (B34)

`optparse-applicative` optional. stdin / `--file`. `ExitFailure 1`.

## Do not

- `cabal install dhall` and re-export it
- Mix this package with set A `standard` in one codebase
- Use `Float` as Dhall Double
