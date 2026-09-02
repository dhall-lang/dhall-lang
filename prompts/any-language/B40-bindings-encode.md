# B40 — Bindings: encode host → Dhall

Depends on: B39  
Read first: `prompts/any-language/00-shared.md`

## Goal

Every `Decoder` from B37–B39 has a matching `Encoder`. Round-trip:

```text
host → encode → (already a normal-ish expr) → decode → host
```

and, when the Dhall type is unique:

```text
expr (typed, normal) → decode → encode → equivalent expr
```

Use `equivalent` (αβ+CBOR) for the second.

Optional: generic/reflection mapping if the host has struct metadata
(Haskell `Generic`, Go tags, Rust serde). If you add it, it must obey
the same field-name and union-alternative rules. It is not required.

## Tests (local)

- Round-trip a nested record `{ name : Text, ok : Bool, n : Natural }`.
- Round-trip `Optional (List Integer)` including `None` and `Some [+1, -2]`.
- Encode a host function `bool -> bool` only if you support it; otherwise
  document that **decoding** Dhall functions works (B39) but **encoding**
  host functions is unsupported (common and acceptable).

## Done when

Encoders exist for all types you decode except possibly host functions.
Local bindings tests from B36–B40 all pass. Language `tests/` suite from
B35 still passes.
