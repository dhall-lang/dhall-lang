# Using the Dhall implementation prompts

This directory is a **script for an AI coding agent** (or a careful human) to
implement Dhall until it matches the language standard. The files here are
instructions. They are not the implementation.

There are two independent scripts:

| Set | Directory | What it produces |
|---|---|---|
| **A — Haskell reference** | [`haskell/`](./haskell/) | Completes the literate Haskell package in `standard/` of **this** repository |
| **B — Any language** | [`any-language/`](./any-language/) | A new Dhall implementation in a language you choose, plus host-language bindings |

Set A edits `dhall-lang`. Set B is meant to be run against a **new project**
that uses this repository only as the spec and as `tests/`. Do not mix slices
from A and B in one session.

Indexes with every slice in order:

- [`haskell/README.md`](./haskell/README.md) — `A00` … `A28`
- [`any-language/README.md`](./any-language/README.md) — `B00` … `B40`

---

## What “done” means

**Set A** is done when, in this repo:

1. `cabal test` in `standard/` runs the suites described in [`tests/README.md`](../tests/README.md) and they pass.
2. Parser `.dhallb` / `.diag` fixtures are generated from the reference encoder (slice A28), not from Ruby.
3. Literate code lives in kebab-case `standard/*.md` files (slice A00).

**Set B** is done when your implementation:

1. Passes the same `tests/` tree, pointed at this checkout.
2. Has the small bindings tests required by `B36`–`B40` (those tests live in *your* project, not here).

Performance is out of scope for both sets.

---

## Before you start

1. Work on a branch. Commit after each successful slice so you can revert a bad one.
2. Read [`tests/README.md`](../tests/README.md) once. Every slice’s test globs assume that protocol (what A and B files mean, how import tests set `HOME` / cache / the local HTTP server).
3. For set A, you need a Haskell toolchain (`nix-shell` from `standard/shell.nix` is enough). For set B, you need whatever toolchain the target language uses, plus a clone of **this** `dhall-lang` repo for `standard/` and `tests/`.
4. Keep a sibling clone of [`dhall-haskell`](https://github.com/dhall-lang/dhall-haskell) only as **inspiration**. Set A may *copy* `dhall-test-server` (A20) and nothing else from that repo. Set B must not paste dhall-haskell sources.

---

## How a slice file is structured

Each `Axx-….md` / `Bxx-….md` file has the same shape:

| Section | Meaning |
|---|---|
| **Depends on** | Slices that must already satisfy their “Done when” |
| **Read first** | Always the set’s `00-shared.md` |
| **Goal** | What this slice is for |
| **Spec** | Which standard documents to follow |
| **Do / Do not** | Allowed edits and forbidden shortcuts |
| **Tests that must pass** | Exact globs under `tests/` (or local bindings tests) |
| **Done when** | The gate before you open the next slice |

If a glob in a slice disagrees with files on disk (renamed tests, new
fixtures), trust **the tree** and `tests/README.md`, and note the mismatch
in the commit message. Do not invent tests to make a slice look finished.

---

## Running one session (the only supported workflow)

Work **one slice per session**. Context windows fill up; a model that
“continues” into the next slice will skip tests and mix concerns.

### 1. Pick the next slice

Use the index. The next slice is the first one whose “Done when” is not
yet true. Do not skip dependencies listed at the top of the file.

Set A and set B are already ordered so that walking `A00`, `A01`, … or
`B00`, `B01`, … is correct. Occasional later slices can start once an
earlier *dependency* is done (for example A05 does not need A04); still
prefer linear order unless you know the dependency graph.

### 2. Open a new chat / agent turn

Attach or `@`-mention **exactly**:

1. `prompts/<set>/00-shared.md`
2. The single slice file, e.g. `prompts/haskell/A13-typecheck-list-optional.md`
3. Any spec files the slice names (`standard/type-inference.md`, …) if your
   tool does not already have the repo in context

Do **not** paste the entire `prompts/` tree. Do **not** attach A14 while
asking for A13.

### 3. Use a wrapper prompt like this

**Set A example**

```text
You are completing the Dhall literate Haskell reference in this dhall-lang
repository.

Follow prompts/haskell/00-shared.md strictly.
Then do only the work in prompts/haskell/A13-typecheck-list-optional.md.

Rules:
- Do not start the next slice.
- Do not copy dhall-haskell language sources.
- Stop if a test and the spec disagree; report that instead of changing
  the test or the judgment meaning.
- When finished, list files you changed and the test command you ran,
  and quote the slice “Done when” checklist.

Begin.
```

**Set B example**

```text
You are implementing Dhall in <LANGUAGE> in <YOUR-PROJECT-PATH>.
The language standard and acceptance tests live in <PATH-TO-dhall-lang>.

Follow prompts/any-language/00-shared.md strictly
and prompts/any-language/overlays/<lang>.md
(java | scala | rust | golang | python | typescript | lean4 | cpp |
haskell | swift | csharp | fsharp | erlang | kotlin | ocaml).
Then do only the work in prompts/any-language/B07-parser-expression.md.

Point the test harness at <PATH-TO-dhall-lang>/tests as described in
dhall-lang/tests/README.md.

Do not start the next slice. Do not translate dhall-haskell or the
production library named in the overlay’s “Forbidden implementations”
section. When finished, list files changed, tests run, and the
“Done when” checklist.

Begin.
```

Replace the slice path each time. Keep the “do not start the next slice”
line; models otherwise chain A13→A14 in one go and leave A13 half-tested.

### 4. Let the agent work, then you verify

The agent should run the tests named in the slice. You still:

1. Read the diff. Check it did not “fix” a test file to match a wrong
   implementation, and did not weaken a judgment.
2. Re-run the slice’s tests yourself (`cabal test` for set A, your harness
   for set B).
3. Re-run **previously passing** suites (regressions). For set A, once the
   tasty driver covers multiple groups, `cabal test` is enough if it runs
   all enabled groups.
4. If the slice says `cabal build` must succeed with `-Wall -Werror`,
   confirm that.

Only then mark the slice done (see [Tracking progress](#tracking-progress)).

### 5. Commit, then start a fresh session for the next slice

Suggested commit message style:

```text
standard: type-check List and Optional (prompt A13)

Pass tests/type-inference/success/unit/List* and Optional*/Some*/None*.
```

A new chat for A14 should again attach `00-shared.md` + `A14-….md`, not
the A13 transcript. If the model needs recent code, it can read the repo;
it does not need the previous prompt conversation.

---

## What to do when a slice fails

| Symptom | What you should do |
|---|---|
| Tests named in the slice fail | Stay on this slice. Point the next session at the **same** slice file plus the failure output. |
| Tests from an *earlier* slice now fail | Treat as a regression. Fix on this slice or revert; do not move forward. |
| A glob matches no files | Check whether names changed. Update the slice file if needed; do not skip silently. |
| Implementation loops on `type-inference/failure` | Add/keep a timeout. Ill-typed terms are not guaranteed to terminate (`tests/README.md`). |
| Agent wants to copy `Dhall.TypeCheck` / `Dhall.Import` | Refuse. Point it at `standard/*.md` (set A) or the spec (set B). Exception: set A slice A20 *must* copy `dhall-test-server`. |
| Agent wants to change a judgment so a test passes | Stop. Open a dhall-lang issue if the spec and the test disagree. |
| Agent implements the next slice “while it is here” | Revert those extra files. The next session will do that work with the right tests. |
| `as Source` tests are missing on `master` | Expected until the standard PR lands. Follow the slice: add a minimal fixture or use the PR branch. Do not skip the *feature*. |

If a session thrashes (repeated wrong fixes), start a **new** chat with:

- `00-shared.md`
- the same slice file
- the current failing test names and a short excerpt of the spec section

Do not paste a long failed transcript unless the tool can use it as
readonly context.

---

## Tracking progress

Keep a simple checklist in the branch (an issue, a note, or a local file
you do not have to commit). Example:

```text
A00 packaging          done  <commit>
A01 parser failure     done  <commit>
A02 alpha              done  <commit>
A03 beta unit          in progress
```

Optional: at the top of each slice, some teams add HTML comments
`<!-- status: done -->` after the slice lands. Not required.

For set B, also record the path to your implementation repo and the path
to the `dhall-lang` checkout the harness uses, so every session can
repeat them in the wrapper prompt.

---

## Set A specifics (this repository)

Shared rules: [`haskell/00-shared.md`](./haskell/00-shared.md).

- Implement in `standard/`. Literate Haskell goes in ` ```haskell ` blocks
  inside the kebab-case `.md` spec. `Parser.hs` / `Interpret.hs` stay
  ordinary Haskell.
- After A00, do not commit `*.lhs` duplicates. Build-time symlinks satisfy GHC.
- Compare expressions via `Binary.encode` (NaN-aware), never `Double` equality.
- Import algorithm: `imports-implementation-notes.md` (or the
  `feature/winitzki/lang-1185-import-as-source` branch), not a paste of
  `Dhall.Import`.
- `as Source` is in scope at A23 even if it is not on `master` yet.

Natural checkpoints (good places to pause a week):

- **A04** — α/β wired; import-free normalization passes
- **A09** — binary decode complete
- **A19** — type inference without imports
- **A26** — full `tests/` 
- **A28** — fixtures generated from this package

A00 (packaging) should be first so later literate edits are not duplicated
into `.lhs` files.

---

## Set B specifics (new implementation)

Shared rules: [`any-language/00-shared.md`](./any-language/00-shared.md).

- Your code lives in another repo or directory. This `dhall-lang` tree is
  the spec + golden tests. Do not rewrite `standard/` as part of set B
  unless you are also contributing a spec fix upstream.
- Parser: no lexer; ABNF alternatives are left-to-right; backtracking is
  required. See the comments at the top of `standard/dhall.abnf`.
- HTTP import tests: reimplement the **contract** in `tests/README.md`.
  Do not vendor the Haskell warp server unless the overlay allows it
  (set A, or set B [`haskell.md`](./any-language/overlays/haskell.md)
  — `dhall-test-server` only, not `Dhall.Import`).
- Bindings (`B36`–`B40`) start only after the language suite is in good
  shape (ideally after B35). They do not replace `tests/`.
- Compare CBOR / encoded normal forms, not pretty-printed Dhall.
- Attach a **language overlay** from
  [`any-language/overlays/`](./any-language/overlays/) on every session
  (toolchain, AST, parser, CBOR, HTTP, bindings). Do not fork `B00`–`B40`
  per language. See [`overlays/README.md`](./any-language/overlays/README.md).

Natural checkpoints:

- **B09** — parser success (encode matches `.dhallb`)
- **B17** — import-free normalization
- **B26** — import-free type inference
- **B32 / B35** — full acceptance suite
- **B40** — bindings round-trip

---

## How much context to give the model

Minimum that works:

- The wrapper prompt (above)
- `00-shared.md`
- One slice file
- Repo access so the agent can open `standard/` and `tests/` itself

Helpful extras when the slice names them:

- The spec Markdown file for that judgment
- `tests/README.md` for suite rules
- A few failing test files, not the entire `tests/` tree

Usually **unhelpful**:

- All remaining slice files
- The whole of `dhall-haskell`
- Previous slice diffs, unless the new slice depends on an API you just
  added and the model cannot find it

If the tool has a small context window, prefer a model that can search
the repo over pasting large specs into the prompt.

---

## Human-only responsibilities

The agent implements; you still own:

- Merge/spec decisions if a test and `standard/` conflict
- Whether `as Source` CBOR mode `4` is still correct once the standard PR
  assigns a number
- Not committing secrets, generated `result/` Nix links, or a dirty
  `tests/import/cache`
- Reviewing literate Haskell for judgment fidelity (set A), not just
  “tests went green”

---

## Out of scope (every slice, both sets)

- Performance work, interpreters-as-VMs, incremental compilation
- Pretty-printing / `dhall format` (tests compare expressions, not text)
- Copying production dhall-haskell language modules
- dhall-haskell’s extra semi-semantic cache (`.cache/dhall-haskell/`)
- Changing the standard’s *meaning* to match a convenient implementation

---

## Quick reference

| I want to… | Do this |
|---|---|
| Finish the reference in this repo | Set A, start at [`haskell/A00-md-only-packaging.md`](./haskell/A00-md-only-packaging.md) |
| Write Dhall in Rust/Go/Python/… | Set B + [`overlays/`](./any-language/overlays/), start at [`B00`](./any-language/B00-architecture.md) |
| Know which tests a slice unlocks | Open that slice; also the tables in `haskell/README.md` / `any-language/README.md` |
| Know how to run a suite | [`tests/README.md`](../tests/README.md) |
| Know standing rules for the set | `haskell/00-shared.md` or `any-language/00-shared.md` |
| Continue after a failure | Same slice + failure log, new session |
| Continue after success | Commit; new session; next slice only |
