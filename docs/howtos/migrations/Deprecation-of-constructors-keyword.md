# Deprecation of constructors keyword

> Migrate your code to remove the obsolete `constructors` keyword

On November 20, 2018 the [language standard changed](https://github.com/dhall-lang/dhall-lang/pull/249)
to remove the need for the `constructors` keyword and beginning the deprecation cycle for the
`constructors` keyword.  This section describes what changed and how to migrate your code.

## Changes

Before this change users would generate constructors for a union type using the `constructors`
keyword, which converted a union type into a record of constructors:

```dhall
let Example = < Left : Natural | Right : Bool >

let example = constructors Example

in  [ example.Left 1, example.Right True ]
```

After this change, you can now access union constructors as if they were fields of the
original union type instead of creating an intermediate record of `constructors`.  For
example, this is now valid:

```dhall
let Example = < Left : Natural | Right : Bool >

in  [ Example.Left 1, Example.Right True ]
```

## Phases

The `constructors` keyword is being phased out in three steps:

*   Phase 1 - Allow accessing constructors as fields of the union types

    * Standard version: [`4.0.0`](https://github.com/dhall-lang/dhall-lang/releases/tag/v4.0.0)
    * `dhall` version: [`1.19.0`](https://github.com/dhall-lang/dhall-haskell/releases/tag/1.19.0)
    * `dhall-to-json`/`dhall-to-yaml` version: 1.2.5

    The first phase is backwards compatible, adding support for accessing constructors directly from a
    union type without changing the behavior of the `constructors` key word.

*   Phase 2 - `constructors x = x`

    * Standard version: [5.0.0](https://github.com/dhall-lang/dhall-lang/releases/tag/v5.0.0)
    * `dhall` version: [1.20.0](https://github.com/dhall-lang/dhall-haskell/releases/tag/1.20.0)
    * `dhall-to-json`/`dhall-to-yaml` version: 1.2.6

    The second phase slightly breaks backwards compatibility by changing the constructors keyword
    to behaves as if it were the identity function for both type-checking and normalization
    purposes.  In other words, any Dhall expression of the form:

    ```dhall
    constructors x
    ```

    ... behaves exactly as if it were just:

    ```dhall
    x
    ```

    For the common idiom of:

    ```dhall
    let SomeUnionType = …
    let someUnionType = constructors SomeUnionType
    in  … someUnionType.SomeConstructor …
    ```

    ... this is not a breaking change because `someUnionType` becomes a synonym for `SomeUnionType` and you
    can access constructors directly off the original union type just as you did for the old `constructors`
    record.

    This change also improves type-checking and normalization performance because the interpreter
    no longer needs to materialize the intermediate record of constructors.  Now the interpreter
    only needs to type-check and normalize the constructors that you actually use.

    However, this is still a breaking change because `constructors x` is now a type instead of a term.  For
    example, this means that you can no longer store the expression `constructors x` inside of a record
    containing other terms:

    ```dhall
    -- This expression used to type-check before this change and
    -- no longer type-checks afterwards
    { foo = constructors MyUnionType
    , bar = 1
    }
    ```

    This change would also break any code that gave an explicit type annotation to a `constructors` expression
    (unlikely, but possible).

*   Phase 3 - Remove the `constructors` keyword

    * Standard version: [6.0.0](https://github.com/dhall-lang/dhall-lang/releases/tag/v6.0.0)
    * `dhall` version: [1.21.0](https://github.com/dhall-lang/dhall-haskell/releases/tag/1.21.0)
    * `dhall-to-json`/`dhall-to-yaml` version: 1.2.7

    This change is strongly backwards-incompatible by removing support for the `constructors` keyword, breaking
    all code that still uses the keyword.

## Migration

*   Phase 1 - Manually migrate your code

    During Phase 1 the `dhall` interpreter does not provide support for code migration since
    replacing `constructors x` with `x` is not a safe transformation until Phase 2.  If you
    wish to migrate your code you will need to do so manually.

    The main benefit of migrating your code manually (before Phase 2) is to get early access to
    the performance improvement from eliminating the intermediate `constructors` records.

*   Phase 2 - Automatically migrate your code

    During Phase 2 you can automatically migrate your code using `dhall lint`, which will
    automatically replace all occurrences of `constructors x` with `x` now that this is a
    behavior-preserving transformation.

*   Phase 3 - Your code breaks if you haven't migrated

    During Phase 3 the deprecation cycle is complete and if you haven't migrated then your
    code will fail to type-check with an "Unbound variable: constructors" error message.
