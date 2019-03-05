# Changelog

All notable changes to the Dhall language standard will be documented in this
file.

For more info about our versioning policy, see [versioning.md](standard/versioning.md).

## `v6.0.0`

Breaking changes:

*   [Don't tag encoded expressions with their hash](https://github.com/dhall-lang/dhall-lang/pull/362)

    Up until now, every new release of the standard required upgrading semantic
    integrity checks since the standard version is included in the input to the
    hash.  The original intent was to fail fast so that users wouldn't attempt
    to decode a malformed expression if the binary format changed.

    Now the standard is stable enough that the hash is quickly becoming the only
    thing that changes for encoded expressions, so this change removes the
    version from the input to semantic integrity check.  This implies that
    semantic integrity checks should now be stable across future standard
    versions (modulo backwards-incompatible changes to the binary format, which
    may still happen, but much less often).

    This should ease one of the biggest pain points when upgrading interpreters
    to support newer releases of the standard.

*   [Remove `constructors` keyword](https://github.com/dhall-lang/dhall-lang/pull/385)

    This is phase 3 of the plan to deprecate the `constructors` keyword, which
    you can find here:

    * [Migration: Deprecation of constructors keyword](https://github.com/dhall-lang/dhall-lang/wiki/Migration%3A-Deprecation-of-constructors-keyword)

    This phase removes the `constructors` keyword for the language so that new
    implementations of the Dhall configuration language have one less thing they
    need to implement.

    If you still haven't migrated yet, the migration is simple: in most cases
    you can delete the `constructors` keyword and your code will still work.
    The above link explains how to handle the few cases that might still break
    as a result of this change.

*   [Add referential sanity check](https://github.com/dhall-lang/dhall-lang/pull/334)

    The referential sanity check is a long-standing feature of the Haskell
    implementation that is now upstreamed into the standard.  This check is both
    a security feature and also a "sanity" feature.
    
    This check prevents a remote import from importing a local import (i.e. a
    local file or environment variable).  The exception is that a remote import
    can still contain a relative import (which still resolves to a remote
    import when canonicalized).

    Without this check a malicious remote import could exfiltrate the contents
    of sensitive local files or environment variables using the language's
    support for custom HTTP headers.

    This check is also "sane" in the sense that remote imports are globally
    addressable, whereas local imports are not, and it doesn't make sense for
    something advertised as globally addressable to depend on imports that are
    not globally addressable.

*   [CBOR-encode only some special values as half-floats](https://github.com/dhall-lang/dhall-lang/pull/376)

    This is a breaking change to the binary representation of `Double` literals
    in order to support porting Dhall to a wider range of languages, many of
    which might not support half-width `Double` representations.

    This change only now encodes all `Double` literals using at least 32 bits,
    with the exception of special values like `NaN` or `Infinity`.

*   [Sort record and union fields before CBOR encoding them](https://github.com/dhall-lang/dhall-lang/pull/392)

    Implementations must now sort record fields and union alternatives when
    serializing Dhall expressions.  The motivation for this change is to
    simplify implementation of the language for interpreters so that they don't
    need to use order-preserving maps for recording the original source order of
    fields/alternatives.

    Implementations can still internally use order-preserving maps if they want
    to support non-standard features (like code formatting or better error
    messages), but restricting the standard serialization format to sorted
    fields/alternatives ensure binary interoperability with other
    implementations.

    Note that this is not a breaking change for semantic integrity checks.
    Fields/alternatives were already sorted for semantic integrity checks since
    the expression is β-normalized before being hashed (and β-normalization
    already sorts fields).

    However, this is a potentially breaking change when serializing Dhall
    expressions in other contexts when the expressions have not yet been
    β-normalized (i.e. serializing and transmitting uninterpreted Dhall code
    over the wire).

New features:

*   [Add Unicode support for quoted path characters](https://github.com/dhall-lang/dhall-lang/pull/353)

    You can now use arbitrary Unicode characters in quoted path components, like
    this:

    ```haskell
    ./families/"禺.dhall"
    ```

    This reflects the fact that users might not have control over the names of
    files that they wish to import.

*   [Add `Text/show` built-in](https://github.com/dhall-lang/dhall-lang/pull/365)

    This adds a new `Text/show` built-in that converts a `Text` literal into
    equivalent Dhall source code:

    ```
    Text/show "ABC\ndef" = "\"ABC\\ndef\""
    ```

    The motivation for this is to enable using Dhall to generate Dhall code and
    also to use Dhall to generate JSON (since the output of `Text/show` is also
    JSON-compatible).

Other changes:

*   Fixes and improvements to the grammar

    * [Allow whitespace after -Infinity](https://github.com/dhall-lang/dhall-lang/pull/377)
    * [Add white space to empty lines inside rules](https://github.com/dhall-lang/dhall-lang/pull/379)
    * [Rename natural-raw to natural-literal-raw](https://github.com/dhall-lang/dhall-lang/pull/368)

*   Fixes and improvements to the semantics

    * [Fix judgment for shifting contexts](https://github.com/dhall-lang/dhall-lang/pull/369)
    * [Simplify import resolution judgment](https://github.com/dhall-lang/dhall-lang/pull/391)

*   Fixes and improvements to tests:

    * [Fix parsing test example](https://github.com/dhall-lang/dhall-lang/pull/344)
    * [Fix test file suffixes](https://github.com/dhall-lang/dhall-lang/pull/347)
    * [Fix types in parser tests](https://github.com/dhall-lang/dhall-lang/pull/366)
    * [Restore doubleB.json](https://github.com/dhall-lang/dhall-lang/pull/372)

    * [Add tests to exercise parser tokens for special Double values](https://github.com/dhall-lang/dhall-lang/pull/381)
    * [Fix 'correct' JSON encoding for doubles in parser test](https://github.com/dhall-lang/dhall-lang/pull/380)
    * [Use binary encoding in parser tests](https://github.com/dhall-lang/dhall-lang/pull/393)

## `v5.0.0`

Breaking changes:

*   [`constructors x = x`](https://github.com/dhall-lang/dhall-lang/pull/256)

    This change the `constructors` keyword to behave like the identity function.
    In other words, the `constructors` keyword returns the union type provided
    as its argument.

    The intermediate record of constructors is no longer necessary now that you
    can access constructors directly from the original union type.  For example,
    this code is unaffected by this change:

    ```haskell
    let Either = < Left : Natural | Right : Bool >

    let either = constructors Either

    in  [ either.Left 1, either.Right True ]
    ```

    ... because before this change the intermediate `either` value would be
    a record with two fields named `Left` and `Right` and after this change
    the intermediate `either` value would be the original `Either` type which
    you can access the `Left` and `Right` constructors from directly.  This
    code is now exactly equivalent to:

    ```haskell
    let Either = < Left : Natural | Right : Bool >

    in  [ Either.Left 1, Either.Right True ]
    ```

    The rationale for this change is to improve performance for users who
    haven't yet removed all occurrences of the `constructors` keyword from
    their code.  Removing the intermediate record of constructors improves
    type-checking and normalization speed while minimizing disruption.

    This is still a breaking change for two reasons:

    *   The most likely way this will break your code is you use record of
        terms that contains a sub-record built by the `constructors` keyword,
        like this:

        ```haskell
        { foo = 1, bar = constructors < Left : Natural | Right : Bool > }
        ```

        The above example was permitted before this change and is not permitted
        after this change, since the `bar` field transforms from a term into a
        type and records can't mix terms (like `foo`) and types (like `bar`)

    *   A less likely way this will break your code is that you gave a type
        annotation to the output of the `constructors` keyword, like this:

        ```haskell
        let Either = < Left : Natural | Right : Bool >

        let either : { Left : Natural → Either, Right : Bool → Either }
              = constructors Either

        in  [ either.Left 1, either.Right True ]
        ```

        The above example would succeed before this change, but fail after this
        change due to the type of `either` changing to `Type`.

    This is phase 2 of the plan to deprecate the `constructors` keyword, which
    you can find here:

    * [Deprecate `constructors`](https://github.com/dhall-lang/dhall-lang/issues/244)

*   [Disallow labels that match builtins or keywords](https://github.com/dhall-lang/dhall-lang/pull/299)

    Before this change the following expression was technically legal, albeit
    potentially confusing:

    ```haskell
    let if = 1 in if
    ```

    After this change the above expression is no longer legal.

    One motivation for this change is to ensure better error messages.  Parsers
    can more convincingly explain parse failures to users when they don't have
    to consider the possibility that these keywords might have been variable
    names.

    Another motivation is to forbid users from writing misleading code by naming
    things after keywords.

New features:

*   [Standardize support for multi-line literals](https://github.com/dhall-lang/dhall-lang/pull/307)

    This is a feature that was part of the Haskell bindings to Dhall that has
    been upstreamed into the standard.

    The standard grammar specified how to parse multi-line string literals but
    not how to interpret them as `Text` literals.  This change specifies how to
    desugar them into ordinary double-quoted string literals.

    For example, this multi-line string literal:

    ```haskell
    λ(x : Text) → ''
      ${x}    baz
          bar
        foo
        ''
    ```

    ... is syntactic sugar for this expression:

    ```haskell
    λ(x : Text) → "${x}    baz\n    bar\n  foo\n  " 
    ```

*   [Standardize support for `as Text`](https://github.com/dhall-lang/dhall-lang/pull/303)

    This is a feature that was part of the Haskell bindings to Dhall that has
    been upstreamed into the standard.

    This allows an import to be imported as raw `Text` rather than being
    interpreted as a Dhall expression.  For example:

    ```
    $ FOO=1 dhall <<< 'env:FOO'
    1
    $ FOO=1 dhall <<< 'env:FOO as Text'
    "1"
    ```

    This can be used to read in text from imports without having to modify them
    to pre-quote the contents.  This comes in handy when modifying the original
    import is not an option.

*   [Forbid import cycles](https://github.com/dhall-lang/dhall-lang/pull/306)

    This is a feature that was part of the Haskell bindings to Dhall that has
    been upstreamed into the standard.

    This forbids import cycles, such as the following trivial cycle:

    ```haskell
    $ cat ./foo
    ./bar

    $ cat ./bar
    ./foo
    ```

    More generally, no import may transitively depend on itself.

    This is not treated as a breaking change since the code that this disallows
    was already broken.  Conceptually, all this change does is improve the user
    experience so that the program fails fast upon detecting a cycle instead of
    getting stuck in an infinite import loop.

*   [Allow nested records of types](https://github.com/dhall-lang/dhall-lang/pull/300)

    Before this change you could have a record of types, but you could not nest
    another record of types within that record.  After this change, a record of
    types counts as a type, meaning that you can mix it with other types within
    a record.

    For example, the following record was previously forbidden and is now legal:

    ```haskell
    { user = { name : Text, age : Natural }, region = Text }
    ```

Other changes:

*   [Update versioning process](https://github.com/dhall-lang/dhall-lang/pull/328)

    This changes the standard evolution process so that the `master` branch is
    always release-ready with respect to the version number.  In other words,
    each new change updates the version number as necessary instead of waiting
    until cutting a release to update the version number.

*   [Fix mistake in union typing judgment](https://github.com/dhall-lang/dhall-lang/pull/311)

    The standard inconsistently allowed unions that store types and kinds in
    some places, but not others.  This fixes the inconsistency by permitting
    them to store types and kinds throughout all judgements.

*   Fixes and improvements to tests:

    * [Sync tests from `dhall-haskell`](https://github.com/dhall-lang/dhall-lang/pull/309)
    * [Add additional type-checking tests for unions](https://github.com/dhall-lang/dhall-lang/pull/329)
    
*   [Fix typos in multiplication semantics](https://github.com/dhall-lang/dhall-lang/pull/331)

    This corrects a mistake in the specification for multiplication

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
