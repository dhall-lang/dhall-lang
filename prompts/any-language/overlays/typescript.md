# Overlay — TypeScript 5 (Node 20+)

Read after `prompts/any-language/00-shared.md` on every TypeScript set-B session.

## Toolchain

- **TypeScript 5.x**, **Node.js 20+** (built-in test runner or Vitest). `package.json` `"type": "module"`.
- Compile with `strict: true`. Target `ES2022`.
- `DHALL_LANG_TESTS` = absolute path to `dhall-lang/tests`. Walk with `node:fs` / `glob`.
- Source UTF-8. Parser: iterate Unicode **code points** (`for (const ch of str)` or `codePointAt`). Do not treat UTF-16 `string.length` units as ABNF characters when they are surrogates.

Deno is allowed if you still hit Node-compatible tests and TLS; default to Node so `node:https` and env vars match `tests/README.md`.

## Forbidden implementations

Do not wrap:

- `dhall` CLI / `dhall-to-json` as the library
- incomplete JS Dhall ports as a dependency
- WASM build of dhall-haskell as the implementation (a from-scratch TS AST is the point)

You may read javascript/dhall experiments. Do not paste them.

## Project layout

```text
src/syntax.ts
src/parser.ts
src/binary.ts
src/shift.ts substitute.ts alpha.ts beta.ts
src/equiv.ts functionCheck.ts typecheck.ts
src/imports.ts hash.ts
src/bind.ts
test/suite.test.ts
test/bind.test.ts
```

Import tests: `process.env.HOME`, `XDG_CACHE_HOME`, `DHALL_TEST_VAR`. On Windows, also `USERPROFILE` if you run those tests there.

## AST (B01)

Discriminated unions:

```ts
type Expr =
  | { tag: "lambda"; name: string; inputType: Expr; body: Expr }
  | { tag: "natural"; value: bigint }
  | ...
```

`type ImportMode = "code" | "text" | "bytes" | "location" | "source"`

Do not use `unknown` JSON trees as Expr. Do not use `as any` to smuggle JSON.

## Parser (B02–B08)

- Recursive descent with an index into the string, advancing by code points.
- Backtrack: save index. Alternatives in ABNF order.
- **No** token lexer (`moo`, Chevrotain lexer). Nested comments + interpolations.
- **No** PEG generator unless you prove character-level ABNF fidelity (usually not worth it).

## Numbers, text, time

| Dhall | TypeScript |
|---|---|
| Bool | `boolean` |
| Natural | `bigint` with `>= 0n` |
| Integer | `bigint` |
| Double | `number` (IEEE binary64) |
| Text | `string` |
| Bytes | `Uint8Array` |
| Date | `{ y, m, d }` or `Temporal.PlainDate` if you enable Temporal |
| Time | clock + `precision: number` |
| TimeZone | minutes `number` |

**Never** use `number` for Natural/Integer. `2^64` is not safe in `number` (B37).

`number` equality: via CBOR, not `===` (NaN).

## CBOR (B09–B10)

Libraries: `cbor-x`, `cborg`, `@stablelib/cbor` — use them as item encoders if you control tags and float widths. Do not `encode(ast)` with default object maps unless they match `binary.md` (including key order).

SHA-256: `node:crypto` `createHash("sha256")`.

## HTTP client and test server (B32)

**Client:** `fetch` (Node 20) or `node:http`/`https`. For HTTPS tests, `NODE_TLS_REJECT_UNAUTHORIZED=0` is too blunt for the whole process; prefer an `https.Agent({ rejectUnauthorized: false })` **only** on the test import client. Implement CORS in the resolver.

**Server:** `node:http` on `127.0.0.1:18080` and `node:https` on `:18443` with the test cert/key PEMs. Implement `tests/README.md`. Vitest/Jest `beforeAll` / `afterAll`. Bodies: Unix `\n`.

Do not spawn the Haskell test server.

## Errors

```ts
class ParseError extends Error {}
class TypeCheckError extends Error {}
class ImportError extends Error { readonly soft: boolean }
```

Type-inference failures: `AbortSignal.timeout(...)` around infer.

## Bindings (B36–B40)

```ts
type Decoder<A> = {
  expectedType: Expr;
  extract: (e: Expr) => A;
};
```

| Dhall | TypeScript |
|---|---|
| Bool, Text, Bytes | `boolean`, `string`, `Uint8Array` |
| Natural, Integer | `bigint` |
| Double | `number` |
| Optional a | `A \| null` — **freeze `null`**, not `undefined`, for `None` |
| List a | `A[]` |
| records | object types `{ readonly x: bigint; readonly y: boolean }` with **exact** keys (`exactOptionalPropertyTypes` helps) |
| unions | `{ tag: "Left"; value: bigint } \| { tag: "Right" }` (empty alt: no `value`) |
| `A → B` | `(a: A) => B` via encode/apply/normalize/decode |

Reject extra keys (`Object.keys`). Do not use `Record<string, unknown>` as the default record binding.

Zod/io-ts extras must still match B38. Encoding TS functions to Dhall is optional (B40).

## CLI (B34)

`src/cli.ts` + `bin` in `package.json`. `node:util` parseArgs or commander. stdin / `--file`. `process.exitCode = 1`.

## Do not

- `JSON.parse` Dhall
- `number` for hashes or Naturals
- `eval`
