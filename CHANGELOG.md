# Changelog

All notable changes to the Dhall language standard will be documented in this
file.

For more info about our versioning policy, see [VERSIONING.md](VERSIONING.md).

## `v2.0.0`

Breaking changes:

*   [Fix α-normalization semantics](https://github.com/dhall-lang/dhall-lang/pull/203)

    Previously α-normalization would incorrectly normalize expressions with
    bound variables named `_`, such as this one:

    ```haskell
    λ(x: Type) → _
    ```

    ... which would incorrectly α-normalize to:

    ```haskell
    λ(_ : Type) → _
    ```

    ... but now correctly α-normalizes to:

    ```haskell
    λ(_ : Type) → _@1
    ```

*   [Disallow merging records of types and records of terms](https://github.com/dhall-lang/dhall-lang/pull/209)

    Previously the type system permitted merging records of types with records
    of terms, like this:

    ```haskell
    { x = Text } ∧ { y = 1 }
    ```

    Now the type system forbids such an expression


*   [Require whitespace when parsing the + operator](https://github.com/dhall-lang/dhall-lang/pull/202)

    Previously the parser would accept an expression without whitespace after
    the `+` operator, like this:

    ```haskell
    λ(x : Natural) → 1 +x
    ```

    Now the parser requires whitespace after the `+`:

    ```haskell
    λ(x : Natural) → 1 + x
    ```

*   [Require non-empty whitespace after keywords](https://github.com/dhall-lang/dhall-lang/pull/222)

    Previously the parser would accept keywords immediately followed by
    punctuation, such as:

    ```haskell
    if(True) then 1 else 2
    ```

    Now the parser requires whitespace after keywords:

    ```haskell
    if (True) then 1 else 2
    ```

*   [Sort fields/alternatives when β-normalizing records/unions](https://github.com/dhall-lang/dhall-lang/pull/223)

    Previously β-normalization would preserve the original order of fields.

    For example, a record like this used to be unaffected by β-normalization:

    ```haskell
    { foo = 1, bar = 2 }
    ```

    ... but now β-normalization will sort the record fields, like this:

    ```haskell
    { bar = 1, foo = 1 }
    ```

New features:

* [Standardize semantics for serializing Dhall expressions](https://github.com/dhall-lang/dhall-lang/pull/194)
* [Standardize semantics for hashing and caching](https://github.com/dhall-lang/dhall-lang/pull/208)
* [Fix grammar for `missing`](https://github.com/dhall-lang/dhall-lang/pull/213)

Other changes:

* [Fix Integer/Natural mismatch in β-normalization section](https://github.com/dhall-lang/dhall-lang/pull/204)
* [Fix typos and formatting in semantics document](https://github.com/dhall-lang/dhall-lang/pull/212)

## `v1.0.0`

Here we start versioning the language standard on its own.

Previously it was versioned together with the [reference implementation][dhall-haskell],
so see [here][dhall-haskell-changelog] for information on previous breaking changes
to the language.

[dhall-haskell]: https://github.com/dhall-lang/dhall-haskell/
[dhall-haskell-changelog]: https://github.com/dhall-lang/dhall-haskell/blob/master/CHANGELOG.md
