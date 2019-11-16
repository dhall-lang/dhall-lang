# Deprecation of old Optional literal syntax

On August 31, 2018 the [language standard changed](https://github.com/dhall-lang/dhall-lang/pull/227) to add new `Some`/`None` `Optional` literals to eventually replace the old `List`-like `Optional` literal syntax.  This section describes what changed and how to migrate your code.

## Changes

Before this change, the normal form of an `Optional` literal resembled a list with at most 1 element:

```haskell
[ 1 ] : Optional Natural  -- An example present `Optional` literal

[] : Optional Natural     -- An example absent `Optional` literal
```

After this change, the normal form of an `Optional` literal is now either a `Some` or a `None` constructor:

```haskell
Some 1        -- An example present `Optional` literal

None Natural  -- An example absent `Optional` literal
```

Additionally, the old `List`-like `Optional` literals now normalize to their equivalent `Some`/`None` constructor.  So, for example:

* `[ 1 ] : Optional Natural` will normalize to `Some 1`
* `[] : Optional Literal` will normalize to `None Natural`.

## Phases

The old `Optional` literal syntax will be phased out in two steps:

* Phase 1 - Add new `Some`/`None` constructors

  * Standard version: [3.0.0](https://github.com/dhall-lang/dhall-lang/releases/tag/v3.0.0)
  * `dhall` version: [1.18.0](https://github.com/dhall-lang/dhall-haskell/releases/tag/1.18.0)
  * `dhall-to-json`/`dhall-to-yaml` version: [1.2.4](https://hackage.haskell.org/package/dhall-json-1.2.4)

  The first phase is backwards compatible.  The only difference is that the normal form of expressions with `Optional` literals changes to use the more compact `Some`/`None` constructors.

* Phase 2 - Remove the old `List`-like `Optional` literals

  * Standard version: [9.0.0](https://github.com/dhall-lang/dhall-lang/releases/tag/v9.0.0)
  * `dhall` version: [1.25](https://github.com/dhall-lang/dhall-haskell/releases/tag/1.25.0)
  * `dhall-to-json` version: [1.4.0](https://hackage.haskell.org/package/dhall-json-1.4.0)

  This change is strongly backwards incompatible by removing support for the old `List`-like syntax for `Optional` literals, breaking all code that still uses the old syntax.

## Migration

* Phase 1 - Automatically migrate your code

   During this phase `dhall lint` will automatically migrate the old `List`-like syntax to use `Some`/`None` instead to ease the migration process.  This is a safe and behavior-preserving transformation.

* Phase 2 - Your code breaks if you haven't migrated

  During Phase 2 the deprecation cycle is complete and if you haven't migrated then your code will fail to type-check since all list-like literals will be treated as `List`s and not `Optional`s.
