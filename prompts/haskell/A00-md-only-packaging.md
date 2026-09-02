# A00 — `.md` is the only committed literate source

Depends on: nothing  
Read first: `prompts/haskell/00-shared.md`

## Goal

Stop duplicating spec documents as `Foo.lhs` + `foo.md`. Keep kebab-case
`.md` files as the source of truth. GHC/markdown-unlit still need a `.lhs`
path at compile time — generate those as **untracked symlinks**.

## Why

Today these pairs are byte-identical (example: `AlphaNormalization.lhs` ≡
`alpha-normalization.md`). That duplication will rot. Cabal already uses
`-pgmL markdown-unlit`.

GHC will not compile `alpha-normalization.md` as a module named
`AlphaNormalization`. The documented markdown-unlit setup is:

```text
AlphaNormalization.lhs  →  symlink to  alpha-normalization.md
```

## Do

1. Add a script `standard/link-literate.sh` that, from `standard/`, creates
   (or refreshes) these symlinks:

   | Target (symlink) | Source |
   |---|---|
   | `Syntax.lhs` | `syntax.md` |
   | `AlphaNormalization.lhs` | `alpha-normalization.md` |
   | `BetaNormalization.lhs` | `beta-normalization.md` |
   | `Binary.lhs` | `binary.md` |
   | `Equivalence.lhs` | `equivalence.md` |
   | `FunctionCheck.lhs` | `function-check.md` |
   | `Multiline.lhs` | `multiline.md` |
   | `Shift.lhs` | `shift.md` |
   | `Substitution.lhs` | `substitution.md` |

2. Delete the committed `.lhs` copies listed above from git (not
   `Equivalence.lhs-boot`).
3. Gitignore `standard/*.lhs` but **do not** ignore `standard/Equivalence.lhs-boot`.
4. Run the script from Nix (`nixops/overlay.nix` `postPatch` or
   `preConfigure` of the `standard` package) and document it in
   `standard/README.md` and `standard/shell.nix` so `cabal build` after
   `nix-shell` still works. A `cabal` `preBuild` hook is acceptable if Nix
   is updated to match.
5. Confirm `standard.cabal` still lists the same `exposed-modules` and
   `build-tool-depends: markdown-unlit`.

## Do not

- Rename the public `.md` files (links in `standard/README.md` use kebab-case).
- Put Haskell in `type-inference.md` or `imports.md` yet (later slices).
- Change parser/encode behavior.

## Tests

```text
cabal build
cabal test   # still only parser/success today; must keep passing (~300 cases)
```

## Done when

- `git ls-files 'standard/*.lhs'` is empty except `Equivalence.lhs-boot` if
  that remains tracked.
- `cabal build` and existing tasty parser-success tests pass after a clean
  checkout **plus** running `link-literate.sh` (or equivalent Nix hook).
- `standard/README.md` explains the symlink step.
