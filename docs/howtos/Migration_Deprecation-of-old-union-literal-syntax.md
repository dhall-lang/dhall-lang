# Deprecation of old union literal syntax

On October 25, 2018 the [language standard changed](https://github.com/dhall-lang/dhall-lang/pull/249) to
support a new way to represent union literals and on May 11, 2019 we proposed to
[remove support for the old union literal syntax](https://github.com/dhall-lang/dhall-lang/issues/542)
in favor of the new syntax.

## Changes

Instead of a union literal being represented like this:

```haskell
< Foo = 0 | Bar : Bool >
```

... the canonical representation for a union literal will become:

```haskell
< Foo : Natural | Bar : Bool >.Foo 0
```

... although this would more commonly be written as:

```haskell
let Example = < Foo : Natural | Bar : Natural >

in  Example.Foo 0
```

## Motivation

The new union literal syntax is preferred for the following reasons:

* Ergonomics: The new union literal syntax is easier to use

* Uniformity: the new syntax is the only way to represent union literals for empty alternatives

  For example, there is no way to represent `< X | Y >.X` using the old union literal syntax.  If
  empty alternatives have to be represented using the new syntax then non-empty alternatives
  should also prefer the new syntax for consistency.

* Language evolution: This paves the way for unions to store anonymous products

  If a union alternative can store less than 1 (i.e. 0) values, then at some point in the future a
  union alternative may be able to store more than 1 value, analogous to a Haskell datatype
  definition:

  ```haskell
  -- data Example = Point2D Double Double | Point3D Double Double Double

  let Example = < Point2D Double Double | Point3D Double Double Double >

  in  ...
  ```

  The new union literal syntax generalizes to anonymous products, should we choose to standardize them,
  whereas the old union literal syntax does not.

  Note that there is not yet a formal proposal to add anonymous products, nor have they been accepted for
  the standardization track, but the new syntax ensures that we leave that option open.

  For more details, see the discussion here: https://github.com/dhall-lang/dhall-lang/issues/224

## Phases

The old union literal syntax will be phased out in two steps:

* Phase 1 - Add new union literal syntax

  * Standard version: 4.0.0
  * Clojure implementation version: 0.2.0
  * Haskell implementation version: 1.19.0
  * Ruby implementation version: 0.1.0

  The first phase is backwards compatible, meaning that both the old and new union literal syntax
  are supported.

* Phase 2 - Remove support for old-style union literals

  * Standard version: 10.0.0
  * Clojure implementation version: To be determined - no earlier than 0.3.0
  * Haskell implementation version: 1.26.0
  * Ruby implementation version: To be determined - no earlier than 0.3.0

  This change is strongly backwards incompatible by removing support for the old-style union literals.

## Migration

* Phase 1 - Manually migrate your code

  There are no automated migration utilities during this phase.  You will need to manually
  updated code to use the new union literal syntax.  However, we expect that most people are using
  the new union literal syntax anyway since it's much easier to use.

* Phase 2 - Your code breaks if you haven't migrated

  During Phase 2 the deprecation cycle is complete and if you haven't migrated then your
  code will fail to parse since the old-style union literal will no longer be grammatically valid.
