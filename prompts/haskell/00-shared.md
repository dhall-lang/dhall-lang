# Shared instructions — Haskell reference (set A)

Read this file at the start of every Haskell slice. It applies until the
whole `A*` sequence is finished.

## Repository roles

- `standard/*.md` is the spec **and**, where it contains ` ```haskell ` blocks, the reference code.
- `standard/Parser.hs` and `standard/Interpret.hs` are ordinary Haskell (not literate).
- `tests/` is the language acceptance suite. A standard-compliant reference must pass it. How to interpret each suite is in `tests/README.md`.
- Sibling repo `dhall-haskell` is **inspiration only**, except the HTTP test server (see A20). Do not paste `Dhall.TypeCheck`, `Dhall.Import`, `Dhall.Parser`, `Dhall.Binary`, or `Dhall.Normalize`.

## Literate Haskell

- Put executable code in fenced ` ```haskell ` blocks inside the kebab-case `.md` file for that judgment (`alpha-normalization.md`, `binary.md`, …).
- Match the existing verbose style: one judgment ⇒ one equation, `where` clauses with subscripted names (`t₀`, `t₁`).
- After A00, **do not commit** `*.lhs` duplicates. GHC still needs `Module.lhs` at compile time; A00 creates those as untracked symlinks.
- Keep `Equivalence.lhs-boot` (or `.hs-boot`): it is a GHC cycle breaker, not a spec copy.
- `Parser.hs` stays non-literate: it follows `dhall.abnf`, not a judgment document.

## Building and testing

From `standard/`:

```bash
nix-shell   # optional; same env as CI
cabal build
cabal test  --test-show-details=direct
```

After A00 the test driver may support filtering; if so, run only the globs named in the current slice plus all previously passing suites.

Expression equality in tests: compare `Binary.encode` of both sides, using the existing NaN-aware CBOR `Term` comparison in `standard/tasty/Main.hs`. Do not use `==` on `Double`.

Type-inference **failure** tests must have a timeout: ill-typed terms are not guaranteed to terminate.

## Imports and `as Source`

- Algorithm: `standard/imports-implementation-notes.md` if present, otherwise the same document on branch `feature/winitzki/lang-1185-import-as-source`.
- Formal companion: `standard/imports.md` (still evolving). Prefer the notes when they are clearer.
- `as Source` is **not yet** on `master` but **is in scope** for this reference. Implement it when you reach A23. Until `binary.md` on that PR assigns a CBOR import-mode integer, use `4` for `as Source` (0=Code, 1=Text, 2=Location, 3=Bytes) and record that choice in a comment next to `encode`.
- Do **not** implement dhall-haskell’s extra “semi-semantic” cache under `.cache/dhall-haskell/`. Only the standard semantic cache (`…/dhall/1220…`) is required.

## Fixtures

- Parser `*B.dhallb` / `*B.diag` must eventually be produced by this package’s `encode` + diagnostic printer (A28). Until A28, do not regenerate fixtures with Ruby `cbor2diag.rb`.
- Binary-decode `*A.dhallb` that are not parse round-trips stay hand-written.

## Allowed vs forbidden edits

Allowed unless a slice says otherwise: `standard/**`, `tests/**` (only if you must add `as Source` fixtures that the standard PR has not landed), `scripts/generate-test-files.sh`, `nixops/overlay.nix`, `.gitignore`, `standard/standard.cabal`.

Forbidden: copying production dhall-haskell language modules; performance rewrites; changing judgment **meaning** to make a test pass (if a test and the spec disagree, stop and report it).

## Definition of done for a slice

1. `cabal build` succeeds (`-Wall -Werror`).
2. Every test glob listed in the slice passes.
3. Every previously completed slice still passes.
4. No unrelated files changed.
