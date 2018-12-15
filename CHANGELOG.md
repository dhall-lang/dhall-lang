# Changelog

All notable changes to the Dhall language standard will be documented in this
file.

For more info about our versioning policy, see [versioning.md](standard/versioning.md).

## `v4.0.0`

Breaking changes:

*   [Specify CBOR encoding of Double and grammar rules for NaN and Infinity values](https://github.com/dhall-lang/dhall-lang/pull/263)

    This changes Dhall to use double-precision floating point values for
    the `Double` type.

    The primary motivation for this is so that the name `Double` is an accurate
    representation of the type's precision.

    This is a breaking change because `Double` literals outside the permitted
    range of IEEE-754 floating point values are no longer permitted.  For
    example, the following expression used to be valid before the change but is
    no longer valid afterwards:

    ```haskell
    1e1000
    ```

*   [Prevent Hurkens' paradox](https://github.com/dhall-lang/dhall-lang/pull/272)

    This fixes a type-checking soundness bug that allowed non-terminating
    expressions.

    This is a breaking change for two reasons:

    *   Some non-terminating expressions used to type check and now they don't

        See the Dhall expression for Hurkens' paradox which used to type-check
        and then fail to ever normalize:

        * https://github.com/dhall-lang/dhall-lang/blob/993d2f43d4988009f2b6bbf546211686658c0ecb/tests/typecheck/failure/hurkensParadox.dhall

        Now the expression fails to type check

    *   This changes a record of types to be a kind instead of a type

        In other words, before this change the following Dhall expression
        would have this hierarchy of types:

        ```haskell
        { x = Bool } : { x : Type } : Kind
        ```

        ... whereas after this change it now has this hierarchy of types:

        ```haskell
        { x = Bool } : { x : Type } : Sort
        ```

*   The `#`/`?`/`:` operators now require non-empty trailing whitespace

    This is a breaking change because expressions such as the following are no
    longer valid:

    ```haskell
    [ 1 ]#[ 2 ]
    ```

    ```haskell
    let a:Natural = 1 in a
    ```

    See:

    * [Disambiguate `#`/`?` operators from HTTP fragments/queries](https://github.com/dhall-lang/dhall-lang/pull/288)
    * [Require whitespace around the colon in type annotations ](https://github.com/dhall-lang/dhall-lang/pull/290)

New features:

*   [Add union constructor selection](https://github.com/dhall-lang/dhall-lang/pull/249)

    This feature improves the ergonomics of using union types so that you no
    longer need to use the `constructors` keyword to generate a record of
    constructors.  Instead, you can use the `.` operator to access constructors
    directly from the original union type.

    In other words, instead of writing this:

    ```haskell
        let Example = < Left : Natural | Right : Bool >

    in  let example = constructors Example

    in  [ example.Left 1, example.Right True ]
    ```

    ... you can now write this:

    ```haskell
        let Example = < Left : Natural | Right : Bool >

    in  [ Example.Left 1, Example.Right True ]
    ```

    This is phase 1 of the plan to deprecate the `constructors` keyword, which
    you can find here:

    * [Deprecate `constructors`](https://github.com/dhall-lang/dhall-lang/issues/244)

*   [Add support for `let` expressions with multiple `let` bindings](https://github.com/dhall-lang/dhall-lang/pull/266)

    You no longer need to nest `let` expressions in order to define multiple
    values.  Instead, you can define multiple `let` bindings within a single
    `let` expression.

    In other words, instead of writing this:

    ```haskell
        let x = 1

    in  let y = 2

    in  x + y
    ```

    ... you can now write this:


    ```haskell
    let x = 1

    let y = 2

    in  x + y
    ```

    See also:

    * [Standardize how to encode/decode multi-`let` expressions ](https://github.com/dhall-lang/dhall-lang/pull/271)

*   [Add support for quoted path components](https://github.com/dhall-lang/dhall-lang/pull/293)

    You can now quote path components for both file paths and URLs.

    For example:

    ```haskell
    /"foo"/bar/"baz qux"
    ```

    ```haskell
    https://example.com/foo/"bar?baz"?qux
    ```

    Quoted URL path components are automatically percent-encoded when URLs are
    resolved.  For example, the above URL is translated to:

    ```
    https://example.com/foo/bar%3Fbaz?qux
    ```

Other changes:

*   [Migrate Prelude into `dhall-lang` repository](https://github.com/dhall-lang/dhall-lang/pull/247)

    The Prelude is now part of the standard and shares the same version as the
    standard version

*   [Add acceptance tests](https://github.com/dhall-lang/dhall-lang/pull/265)

    The standard now includes acceptance tests that implementations can use to
    check whether they conform to the standard.

    See also:

    * [Add normalization test for multiple let bindings](https://github.com/dhall-lang/dhall-lang/pull/270)
    * [Add parser tests](https://github.com/dhall-lang/dhall-lang/pull/276)
    * [Add failure test for duplicated record fields to typecheck suite](https://github.com/dhall-lang/dhall-lang/pull/278)
    * [Fix `remoteSystems` normalization test](https://github.com/dhall-lang/dhall-lang/pull/284)
    * [Fix specification for running import tests](https://github.com/dhall-lang/dhall-lang/pull/286)
    * [Fix path termination parser tests](https://github.com/dhall-lang/dhall-lang/pull/294)

*   [Remove grammar whitespace ambiguity](https://github.com/dhall-lang/dhall-lang/pull/251)

    This clarifies the grammar to remove ambiguity in how to parse whitespace.

*   [Allow for spaces before expression in interpolated string](https://github.com/dhall-lang/dhall-lang/pull/279)

    This fixes a bug in the grammar that disallowed leading space in an
    interpolated expression

*   Small fixes to the prose:

    * [Fix Sort / Kind mistake](https://github.com/dhall-lang/dhall-lang/pull/277)
    * [Typo in binary.md](https://github.com/dhall-lang/dhall-lang/pull/283)
    * [Small fixes to import semantics section](https://github.com/dhall-lang/dhall-lang/pull/289/files)

## `v3.0.0`

Breaking changes:

*   [New `Some`/`None` constructors for `Optional` values](https://github.com/dhall-lang/dhall-lang/pull/227)

    Including: [Prelude: Use new `Some` and `None` constructors](https://github.com/dhall-lang/Prelude/pull/9)

    You can now use `Some` and `None` to build `Optional` values, and `Some`
    does not require providing the type:

    ```haskell
    -- The type annotations are optional, but provided for clarity

    Some 1 : Optional Natural

    None Natural : Optional Natural
    ```

    This is a breaking change because `Some` and `None` are now reserved
    keywords.  For example, the following code breaks as a result of this
    change:

    ```haskell
    λ(Some : Type) → Some
    ```

    This is also a breaking change because it removes `Optional/Some` and
    `Optional/None` from the Prelude

*   [Add kind-polymorphism](https://github.com/dhall-lang/dhall-lang/pull/238)

    Including: [Fix to allow type-level functions as record fields](https://github.com/dhall-lang/dhall-lang/pull/241)

    This adds support for kind-level programming by adding a new type-checking
    constant named `Sort` above `Kind` in the hierarchy of types:

    ```haskell
    Type : Kind : Sort
    ```

    This is a breaking change because `Sort` is now a reserved keyword.  For
    example, the following code breaks as a result of this change:

    ```haskell
    λ(Sort : Type) → Sort
    ```

*   [Update versioning policy for the standard and binary protocol](https://github.com/dhall-lang/dhall-lang/pull/243)

    This changes how the standard versions the binary protocol.  The protocol
    now shares the same version number as the rest of the standard.

    That is not the breaking change, though, since it does not forbid older
    versions of the standard using the older protocol version string.

    The actual breaking change is that compliant interpreters can no longer
    mix language features from different versions of the standard within a
    single run of the interpreter.  For example, you would not be able to an
    interpret an expression containing a new language feature alongside an
    import protected by a semantic integrity check preceding that language
    feature.  Either the new language feature of the semantic integrity check
    would fail depending on which standard version the interpreter was
    implementing.  Previously newer language features were compatible with
    older semantic integrity checks.

*   [Normalize record types and literals generated by operators](https://github.com/dhall-lang/dhall-lang/pull/228)

    This ensures that records generated by operators have their fields sorted.

    For example, before this change, the following expression:

    ```haskell
    { foo = 1 } ∧ { bar = True }
    ```

    ... would β-normalize to:

    ```haskell
    { foo = 1, bar = True }
    ```

    ... and now β-normalizes to:

    ```haskell
    { bar = True, foo = 1 }
    ```

    This is technically a breaking change in the sense that the standard no
    longer guarantees that record equality is order insensitive, although in
    practice records are usually only compared after they have been
    β-normalized (and therefore had their fields sorted).

New features:

*   [Prelude: Add `{Integer,Natural}/toDouble`](https://github.com/dhall-lang/Prelude/pull/10)

    This ensures consistency with the rest of the Prelude by re-exporting two
    built-ins that were missing

*   [Specify associativity for repeated elements](https://github.com/dhall-lang/dhall-lang/pull/233)

    Including: [Prelude: Parenthesize right-associative output](https://github.com/dhall-lang/Prelude/pull/11)

    The grammar now specifies the associativity of repeated elements (such as
    operators).

    This is not a breaking change because the behavior was not previously
    standardized.  Also, all operators are associative, so the associativity
    does not affect their behavior.

Other changes:

*   [Clarify the binary encoding of text literals](https://github.com/dhall-lang/dhall-lang/pull/235)

*   [Fix typo](https://github.com/dhall-lang/dhall-lang/pull/239)

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
