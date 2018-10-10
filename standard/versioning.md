# Versioning Dhall

This document describes how the Dhall language standard is versioned,
and how new versions are released.

## Current version

The current version string is:


    ────────────────────────
    currentVersion = "3.0.0"


This version string is used by implementations to:

* identify which release(s) of the standard they comply with
* tag encoded Dhall expressions in their binary representation

## What is a "version"

A version of the Dhall language standard consists of a *git tag*
(i.e. what GitHub calls a "release").

## How are releases made

1. Compute the new version number by following the [versioning scheme](#versioning-scheme)
2. Open a Pull Request with the "release template" to get a checklist:
   you do it by appending `?template=release_template.md` to your PR url.
3. The PR will sit there for the minimum review period of 3 days (no exceptions).  
   This is to give time to merge the last things to `master` before cutting the new release.
4. Once that is merged, create a git tag/GitHub release with the new version number
5. Repeat the above step for the [Prelude repo](https://github.com/dhall-lang/Prelude) as well.  
   Note: the tagged version should be the same as the new version of the language standard.

## When are releases made

Anybody can cut a release if there is:

1. an unreleased change *at least 1 month old*, and:
2. no unreleased changes *less than 3 days old*.

The goal is to encourage releasing early and often and to give anybody permission
to cut a release.  
This includes people without the ["commit bit"](https://github.com/dhall-lang/dhall-lang/blob/master/.github/CONTRIBUTING.md#how-do-i-get-the-commit-bit):
they should be able to request that somebody cut a release on their behalf.

This implies that the `master` branch of the specification should always be "release-ready".

## Versioning scheme

The specification for the language standard is versioned in the form `"X.Y.Z"`,
where `X`, `Y` and `Z` are Natural numbers.

The version is currently a string of the form "X.Y.Z", where `X`, `Y`, and `Z`
are `Natural` numbers:

*   Changing the version from "X.Y.Z" to "{X + 1}.0.0" indicates a
    backwards-incompatible change

    This may require users to modify their code if the backwards-incompatible
    change affects their code

*   Changing the version from "X.Y.Z" to "X.{Y + 1}.0" indicates a
    backwards-compatible change that adds a new language feature

    This means that if users author code that takes advantage of the new
    language feature they will no longer be able to downgrade to an older
    version of the standard

*   Changing the version from "X.Y.Z" to "X.Y.{Z + 1}" indicates
    no semantic change

    Example: a change in the documentation or renaming terminals in the grammar

All three changes require users to update their semantic integrity checks if
they upgrade since the entire version string is an input to the hash in a
semantic integrity check.  Implementations SHOULD support older versions of the
standard that match in the first version component so that users can defer
upgrading their code until they are ready to regenerate their semantic
integrity checks.

This versioning scheme is only a convention that provides humans a convenient
mnemonic for tracking releases.  The version number selected for a release could
be incorrect and the release process only guarantees a best effort attempt to
select an accurate version number for the release.  For example, a new release
of the standard could mistakenly change a judgment to introduce a
non-backwards-compatible change without incrementing the first component of the
version number.  This would only imply that the version number is misleading as
a mnemonic, but semantically the correctness of the version number only depends
on being distinct from all prior version numbers.

## What is a "backwards-incompatible" change? (a.k.a. "breaking change")

The contract for a "non-breaking" change is that any code that
parsed/resolved/type-checked/normalized successfully before must continue to do
so (modulo imported resources vanishing).  The exception is the semantic
integrity check, which always changes with a new release of the standard.

## Compliance

The key words "MUST", "MUST NOT", "SHOULD", and "MAY" in this document are to
be interpreted as described in [RFC 2119](https://www.ietf.org/rfc/rfc2119.txt)

An implementation MUST support the latest version of the standard, which is
currently defined as:

An implementation MAY support older versions of the standard.  An implementation
that supports an older version of the standard MUST do so "as a whole" for any
given run of the interpreter.  During such a run, all functionality (including
import resolution, type-checking, and normalization) MUST be implemented as
specified in that release of the standard.  An implementation MUST NOT mix and
match functionality from different releases of the standard within any given
run.

For example, suppose that version "2.0.0" of the implementation imported an
expression protected by a semantic integrity check:

    ./example.dhall sha256:bb1e096305428d2e155282d156ddec47cca75cd61bc0ef1aa6ce4cf5eae91e38

... and that expression was cached underneath
`~/.cache/dhall/bb1e096305428d2e155282d156ddec47cca75cd61bc0ef1aa6ce4cf5eae91e38`
tagged with version "2.0.0".  Now suppose that an interpreter upgraded and
supported both versions "2.0.0" and "2.1.0" of the standard.  If the
interpreter enabled version "2.1.0" of the standard then the interpreter would
not be allowed to import the above expression (even though version "2.1.0" might
be otherwise backwards compatible with "2.0.0").  If the interpreter enabled
version "2.0.0" of the standard then the import would succeed, but that
expression (and the rest of the program) would be type-checked and normalized as
using only version "2.0.0" of the standard.

## Standard evolution

This section describes the recommended work-arounds for common standard
evolution pitfalls:

*   A new release of the standard misassigns the version number

    This is mostly harmless, albeit confusing.  The standard SHOULD publish a
    new release with a fix to the version number.  Compliant implementations
    need not do anything other than support the newer version once published.
    In particular, compliant implementations MAY drop support for the release
    with a misassigned version number but this is not required.

*   There is a specification bug in the standard

    A specification bug encompasses any inconsistency in the standard, including
    (but not limited to):

    * Unsoundness
    * Incompleteness
    * Inconsistency between the prose and the formal judgments

    To address this, an implementation MAY deviate from the standard in any way
    so long as the implementation formally documents the deviation.  An
    implementation that does so SHOULD upstream the fix to the standard.

*   A widely used implementation implements the standard incorrectly

    The widely used implementation SHOULD fix the deviation.  Other
    implementations MAY accommodate the deviation so long as they formally
    document the deviation from the standard.
