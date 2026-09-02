# Overlay — Python 3.11+

Read after `prompts/any-language/00-shared.md` on every Python set-B session.

## Toolchain

- Python **3.11+** (`match`, `Self`, `tomllib` unused). Prefer 3.12.
- Package: `dhall` on src layout (`src/dhall/`). Tests: **pytest**.
- `DHALL_LANG_TESTS` points at `dhall-lang/tests`. Discover files with `pathlib`.
- Always open text as UTF-8 (`encoding="utf-8"`). Parser iterates **Unicode code points** (`for ch in s` is fine: Python `str` is a sequence of code points).

Type-check the implementation with pyright or mypy in strict-ish mode if you can; not required for slices to pass.

## Forbidden implementations

Do **not** wrap:

- PyPI `dhall` / `dhall-python` (typically Rust FFI)
- `subprocess` to the `dhall` CLI as the library
- `dhall-haskell` via Haskell FFI

You may read those projects. Do not paste them.

## Project layout

```text
src/dhall/syntax.py
src/dhall/parser.py          # or parser/
src/dhall/binary.py
src/dhall/shift.py substitute.py alpha.py beta.py
src/dhall/equiv.py function_check.py typecheck.py
src/dhall/imports.py hash.py
src/dhall/bind.py
tests/test_suite.py
tests/test_bind.py
```

Import-test env: `monkeypatch.setenv` for `HOME`, `XDG_CACHE_HOME`, `DHALL_TEST_VAR`. Copy the cache dir if tests might write.

## AST (B01)

`dataclasses` + a tagged union:

```python
@dataclass(frozen=True)
class Lambda:
    name: str
    input_type: "Expr"
    body: "Expr"
Expr = Union[Lambda, NaturalLit, ...]
```

Or a single `Expr` with an `enum Kind` and payloads. Prefer many small `@dataclass` types and `match`.

`class ImportMode(enum.Enum): CODE = ...; SOURCE = ...`

Do not use `dict` / JSON as the AST. Do not use `ast` (Python AST).

## Parser (B02–B08)

- Recursive descent with an index into `str`. Backtrack by saving `i`. Try alternatives in ABNF order.
- **No** `tokenize`, **no** Lark/ANTLR lexer mode for Dhall. Lark *Earley on characters* is possible but easy to get wrong; descent is safer.
- Nested `{- -}`: increment depth; interpolations parse `complete-expression`.
- Name functions after ABNF rules.

## Numbers, text, time

| Dhall | Python |
|---|---|
| Bool | `bool` (careful: `bool` is a `int` subclass — do not treat `True` as Natural) |
| Natural | `int` with `>= 0` |
| Integer | `int` |
| Double | `float` (IEEE binary64 on CPython) |
| Text | `str` |
| Bytes | `bytes` |
| Date | `datetime.date` |
| Time | `datetime.time` + `precision: int` |
| TimeZone | `datetime.timezone` or minutes `int` |

Python `int` is unbounded — good. Still reject negative Natural.

`float` equality: via CBOR, not `==` (NaN). Do not use `decimal.Decimal` as Double.

## CBOR (B09–B10)

`cbor2` may encode primitive items. You must still build the **Dhall CBOR term structure** from `binary.md` (list tags, bignums, float widths). Do not `cbor2.dumps(ast)` on a dataclass.

SHA-256: `hashlib.sha256`.

## HTTP client and test server (B32)

**Client:** `urllib.request` or `httpx`. For HTTPS tests, `ssl` context that accepts the self-signed cert **only** in tests. Implement CORS in the resolver.

**Server:** `http.server.HTTPServer` on `127.0.0.1:18080` and an `SSLContext` wrap for `:18443` (`ssl.PROTOCOL_TLS_SERVER` + load test cert/key). Thread in `pytest` fixture (`yield`, then shutdown). Implement `tests/README.md`. Write bodies with `\n`.

Do not use Flask as a reason to skip endpoints. Do not start the Haskell server.

## Errors

```python
class DhallError(Exception): ...
class ParseError(DhallError): ...
class TypeError_(DhallError): ...  # TypeError shadows builtin — name TypeCheckError
class ImportError(DhallError):
    soft: bool
```

Type-inference failures: `pytest.mark.timeout` (`pytest-timeout`) or `signal`/`alarm` where available.

## Bindings (B36–B40)

```python
@dataclass
class Decoder(Generic[T]):
    expected_type: Expr
    extract: Callable[[Expr], T]
```

Or `Protocol` with `from_expr` / `to_expr`.

| Dhall | Python |
|---|---|
| Bool | `bool` |
| Text, Bytes | `str`, `bytes` |
| Natural, Integer | `int` |
| Double | `float` |
| Optional a | `T \| None` — document that Python `None` is `None T` **only** when the decoder knows `T`; bare `None` without type fails |
| List a | `list[T]` |
| records | `dataclasses.dataclass` / `NamedTuple` / `pydantic` model with **field names = labels** |
| unions | `Enum` + payload, or a `@dataclass` with a `tag` field; empty alts have no value |
| `A → B` | `Callable[[A], B]` via encode/apply/normalize/decode |

Pydantic/attrs extras (`extra = forbid`) are good for B38. Encoding Python callables to Dhall is optional (B40).

Do not decode records as plain `dict` unless you still enforce exact keys.

## CLI (B34)

`python -m dhall` / `typer` / `argparse`. stdin / `--file`. `sys.exit(1)`.

## Do not

- `eval` Dhall
- `json.loads` Dhall
- `from dhall import *` wrapping the PyPI binary wheel as “the implementation”
