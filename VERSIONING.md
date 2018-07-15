# Versioning Dhall

This document describes how the Dhall language standard is versioned,
and how new versions are released.

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
1. an unreleased change *at least 1 month old*, and
2. no unreleased changes *less than 3 days old*.

The goal is to encourage releasing early and often and to give anybody permission
to cut a release.  
This includes people without the ["commit bit"](https://github.com/dhall-lang/dhall-lang/blob/master/.github/CONTRIBUTING.md#how-do-i-get-the-commit-bit)):
they should be able to request that somebody cut a release on their behalf.

This implies that the `master` branch of the specification should always be "release-ready".

## Versioning scheme

The specification for the language standard is versioned in the form `vX.Y.Z`,
where `X`, `Y` and `Z` are Natural numbers.

When introducing changes in a new release, the version should change in the following way:
- **A change to `X`** reflects a *backwards-incompatible* change to the specification
  - This means that Dhall code compatible with the previous version might not be
    compatible with the new one.  
    This is the only change that **needs action** from the user
    (to update the existing code to the new version).
- **A change to `Y`** reflects a *new language feature*
  - This means that any Dhall code that depends the new language feature will not
    be compatible with the older specifications (so you wouldn't be able to
    "downgrade" your interpreter).
- **A change to `Z`** is basically *semantics-preserving refactors* of the specification
  - i.e. documentation/whitespace changes or renaming terminals in the grammar, etc.
  - This means that some existing Dhall code compatible with the previous standard
    will have the same semantics with the new standard, and viceversa.

## What is a "backwards-incompatible" change? (aka "breaking")

The contract for a "non-breaking" change is that any code that
parsed/resolved/type-checked/normalized successfully before must continue to do so
(modulo imported resources changing - that is, if the imports are protected by
their semantic hash they should not break as well).

Carefully note that this does not include pretty-printing or formatting the code.
