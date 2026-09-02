# Language overlays for set B

Set B (`B00`–`B40`) is language-neutral. An **overlay** freezes toolchain,
types, libraries, and bindings for one host language so an agent does not
reinvent those choices (or contradict the spec).

Do **not** copy `B00`–`B40` into a per-language tree. Attach the overlay
on every session together with `00-shared.md` and one slice.

## Files

| Overlay | Language |
|---|---|
| [`java.md`](./java.md) | Java 17+ |
| [`scala.md`](./scala.md) | Scala 3 |
| [`rust.md`](./rust.md) | Rust 2021 |
| [`golang.md`](./golang.md) | Go 1.22+ |
| [`python.md`](./python.md) | Python 3.11+ |
| [`typescript.md`](./typescript.md) | TypeScript 5 + Node 20+ |
| [`lean4.md`](./lean4.md) | Lean 4 |
| [`cpp.md`](./cpp.md) | C++20 |
| [`haskell.md`](./haskell.md) | Haskell / GHC 9.4+ |
| [`swift.md`](./swift.md) | Swift 5.9+ |
| [`csharp.md`](./csharp.md) | C# / .NET 8 |
| [`fsharp.md`](./fsharp.md) | F# / .NET 8 |
| [`erlang.md`](./erlang.md) | Erlang / OTP 26+ |
| [`kotlin.md`](./kotlin.md) | Kotlin (JVM 17+) |
| [`ocaml.md`](./ocaml.md) | OCaml 4.14 / 5.x |

## Session wrapper

```text
Follow prompts/any-language/00-shared.md
and prompts/any-language/overlays/<lang>.md.
Then do only prompts/any-language/<slice>.md.

The implementation lives in <YOUR-PROJECT>.
Point tests at <PATH-TO-dhall-lang>/tests as in dhall-lang/tests/README.md.

Do not start the next slice. Do not translate dhall-haskell or the
production library named in the overlay’s “Forbidden implementations”
section. Begin.
```

If the overlay and a B slice disagree on **spec** (ABNF, judgments, test
globs), the B slice and `standard/` win. If they disagree on **host types
or crates**, the overlay wins.

## What belongs in an overlay vs a B slice

| Overlay | B slice |
|---|---|
| `enum` vs class, `BigInteger`, crate names | `Natural` is unbounded |
| JUnit vs `cargo test` | `tests/parser/success/**` must pass |
| How a union looks in Java | Extra Dhall fields are an error |
| HTTP framework | Ports `18080` / `18443` and CORS table |

## When to fork a B file

Only if a slice is unusable without types (usually B00, B32, B36–B38).
Prefer adding a “Language” subsection in the overlay rather than a
`B00-rust.md` duplicate of test globs.
