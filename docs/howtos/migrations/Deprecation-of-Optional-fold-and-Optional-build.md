# Deprecation of `Optional/fold` and `Optional/build`

> Migrate your code to remove the `Optional/fold` and `Optional/build` builtins

On December 22, 2019 the [language standard changed](https://github.com/dhall-lang/dhall-lang/pull/860)
to support a new way of destructuring `Optional` values using the existing `merge` keyword, and we proposed to
[remove the now-redundant `Optional/fold` builtin](https://github.com/dhall-lang/dhall-lang/issues/869)
. `Optional/build` is removed in sync with `Optional/fold`, since it was defined only for consistency with
other builtin folds.

## Changes

### `Optional/fold`

Instead of destructuring an `Optional` like this:

```dhall
Optional/fold Bool (Some True) Bool (λ(b : Bool) → b) False
```

... users should use `merge` like this:

```dhall
merge { None = False, Some = λ(b : Bool) → b } (Some True)
```

### `Optional/build`

Users of `Optional/build` should simply use `Some` and `None` directly.

## Phases

`Optional/build` and `Optional/fold` will be phased out in three steps:

* Phase 1 - Extend `merge` to work on `Optional`s

  * Standard version: [13.0.0](https://github.com/dhall-lang/dhall-lang/releases/tag/v13.0.0)
  * Haskell implementation version: 1.29.0
  * Ruby implementation version: To be determined
  * Rust implementation version: 0.3.0

  The first phase is backwards compatible, meaning that both `Optional/fold` and `merge`
  are supported.

* Phase 2 - Remove use of the `Optional/fold` and `Optional/build` builtins in the Prelude

  `Prelude.Optional.fold`, `Prelude.Optional.build` and other `Prelude` functions will be
  changed to use `merge` instead of the deprecated builtins.

  * Standard version: 15.0.0

* Phase 3 - Remove the `Optional/fold` and `Optional/fold` builtins

  * Standard version: 17.0.0
  * Haskell implementation version: 1.33.0
  * Ruby implementation version: To be determined
  * Rust implementation version: 0.6.0

  This change is strongly backwards incompatible by removing support for the old `List`-like syntax for `Optional` literals, breaking all code that still uses the old syntax.

## Migration

* Phase 1 + 2 - Automatically migrate your code

  Starting in `dhall-1.30.0`, `dhall lint` will automatically migrate code using the deprecated builtins
  to ease the migration process. This is a safe and behavior-preserving transformation.

* Phase 3 - Your code breaks if you haven't migrated

  During Phase 3 the deprecation cycle is complete and if you haven't migrated then your
  code will fail to type-check since the `Optional/fold` and `Optional/build` builtins will be
  out of scope.
