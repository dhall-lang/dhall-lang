# Changelog

All notable changes to the Dhall language standard will be documented in this
file.

For more info about our versioning policy, see [VERSIONING.md](VERSIONING.md).

## `v2.0.0`

Breaking changes:

* [Fix α-normalization semantics](https://github.com/dhall-lang/dhall-lang/pull/203)
* [Disallow merging records of types and records of terms](https://github.com/dhall-lang/dhall-lang/pull/209)
* [Fix grammar for `missing`](https://github.com/dhall-lang/dhall-lang/pull/213)
* [Require whitespace when parsing the + operator](https://github.com/dhall-lang/dhall-lang/pull/202)
* [Require non-empty whitespace after keywords](https://github.com/dhall-lang/dhall-lang/pull/222)
* [Sort fields/alternatives when β-normalizing records/unions](https://github.com/dhall-lang/dhall-lang/pull/223)

New features:

* [Standardize semantics for serializing Dhall expressions](https://github.com/dhall-lang/dhall-lang/pull/194)
* [Standardize semantics for hashing and caching](https://github.com/dhall-lang/dhall-lang/pull/208)

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
