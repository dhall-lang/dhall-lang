# Contributing to Dhall

## Am I in the right place?

This repository hosts language-independent features of the Dhall ecosystem:

* The [Dhall standard][standard] grammar and semantics
* [Dhall infrastructure][infrastructure]
* The [Prelude][prelude] of shared utility functions

Issues on this repository typically discuss language design and proposed
changes to the language that are not specific to an integration.

For example:

* [Natural / Integer syntax suggestion](https://github.com/dhall-lang/dhall-lang/issues/138)
* [Simplify if x then y else y to y](https://github.com/dhall-lang/dhall-lang/issues/108)

If you want to discuss a specific implementation of the Dhall language, then you
usually want to create an issue against the repository for that implementation.
The two most common repositories that we often redirect people to are:

*   [`dhall-haskell`][dhall-haskell-issues] - Questions about the Haskell
    integration, including the `dhall` executable provided by that package

    For example:

    * [Add `dhall --type` option](https://github.com/dhall-lang/dhall-haskell/issues/349)
    * [Error message seems wrong](https://github.com/dhall-lang/dhall-haskell/issues/299)

*   [`dhall-json`][dhall-json-issues] - Questions about the JSON integration,
    including the `dhall-to-json`/`dhall-to-yaml` executables provided by that
    package

    For example:

    * [Convert some association lists to homogeneous maps](https://github.com/dhall-lang/dhall-json/issues/27)
    * [Valid Dhall not parsed by dhall-to-{json,yaml}](https://github.com/dhall-lang/dhall-json/issues/4)

## What is the correct way to ask a question?

You can either open issues [here][dhall-lang-issues] to ask a question
or use the [Dhall discourse][dhall-discourse].  You can also use
[Stack Overflow][stack-overflow].  I (@Gabriel439) subscribe by e-mail
to any Stack Overflow issues created with the tag `dhall`.

## Do I belong here?

Everybody is welcome!  The Dhall community is not an "exclusive" or "elite"
club.  People of all experience levels can join and begin contributing and
should feel comfortable and safe making mistakes.

People of all backgrounds belong here as long as they treat others with dignity
and respect and do not harass or belittle others.

## How do I change the language?

We make all changes to the language through pull requests to change the files
located in the [standard][standard] directory of this repository.

For simple changes, just begin by opening a [pull request][dhall-lang-pulls].
You don't need the change to be pre-approved before opening a pull request
since the discussion can happen directly on the pull request conversation.

For more complex changes, you might want the feature to be approved before
spending time on creating a matching pull request.  You can open an
[issue][dhall-lang-issues] discussing your idea first and if the issue is
approved then you can proceed with a pull request.

You also don't have to understand how to translate your change to a formal
grammar or type-checking judgments in order to propose the change.  Just propose
your idea and if your idea is approved then you can ask somebody (including
@Gabriel439) to formally standardize your change via a pull request for you.

The proposed change should also include some examples that show how it should be 
used, eventual caveats, etc.  These examples should end up in the [tests][dhall-lang-tests]
folder, so that all the implementations can verify that their approach is 
conformant with the changes to the typechecker, import system, etc.

## How do changes get approved?

Changes only require approval if they change the [standard][standard] or
[Prelude][prelude].  If a pull request does not require approval, anybody with
the "commit bit" (i.e. write access to this repository) can merge it after a 24
hour waiting period.  See below for more details about how to obtain the commit
bit.

Changes to the [standard][standard] and [Prelude][prelude] are voted on by
actively maintained implementations of the Dhall language.  Votes are made
using the GitHub pull request review feature.  Each implementation gets one
vote.

At the time of this writing the five actively supported implementations of Dhall
are:

*   [`dhall-haskell` - Haskell bindings to Dhall](https://github.com/dhall-lang/dhall-haskell)

    Led by @Gabriel439

*   [`dhall-clj` - Clojure bindings to Dhall](https://github.com/f-f/dhall-clj)

    Led by @f-f

*   [`dhall-ruby` - ruby bindings to Dhall](https://git.sr.ht/~singpolyma/dhall-ruby)

    Led by @singpolyma

*   [`dhall-rust` - Rust bindings to Dhall](https://github.com/Nadrieril/dhall-rust)

    Led by @Nadrieril

*   [`dhall-golang` - Go bindings to Dhall](https://github.com/philandstuff/dhall-golang)

    Led by @philandstuff

Each of those implementations get one vote cast by the lead contributor for each
implementation.

Implementations do not need to be useful or widely used to get a vote.  If you
create a complete implementation as a side project that nobody uses and does not
integrate with anything, you still get to vote on changes to the standard.

Derived implementations do not count as a vote.  For example, there are many
integrations that are powered by the Haskell integration (such as `dhall-to-nix`
and `dhall-to-json`), but they do not count as extra votes because they are not
a separate reimplementation of the standard.

The rules for merging a change are:

* You can merge a change 7 days after the original submission, even without
  positive votes, so long as there are less than 50% negative votes

* You can merge a change 3 days after the original submission if you have
  50% or more positive votes

* You must wait at least 24 hours since the last change before merging

  Merging `master` into your branch does not count as a change for this
  purpose

  You also do not need to get new changes re-approved.  Just wait 24 hours

The process is designed to be quick with a bias to action since if we make
mistakes we can always roll them back with a new proposal.

### Soundness of proposed changes

For non-trivial changes there should be a proof-of-concept implementation as a
sanity check. Moreover, changes to the type system need to have an informal proof
sketch of the following four soundness rules:

- Type-inference won't diverge
- If an expression type-checks, normalizing that expression won't diverge
- Normalizing an inferred type won't diverge
- Normalizing an expression doesn't change its type
- All inferred types are in β-normal form

## How do I update generated test files?

There are two sets of files you may need to update in the course of a change:

* If you add or modify any `.cbor` files in the test suite then you will need to
  generate the matching `.diag` files

* If you add or update any Prelude utilities then you will need to lint them
  and re-freeze downstream files

CI will remind you to update these files if you forget.  Specifically, a test
failure will show up on your pull request displaying a diff between what CI
expected and what was found.

There are two ways you can apply the change:

* You can manually update the code to match the diff that CI displayed

* You can run a script that produces an output exactly matching what CI
  expected

Since CI uses Nix the latter scripts require you to install Nix:

* [Nix](https://nixos.org/nix/)

You can also use the same cache that CI uses to avoid rebuilding the world by
following these instructions:

* [Use our shared caches](https://github.com/dhall-lang/dhall-haskell#nix)

Once you do that, you can run the following scripts:

* `./script/generate-test-files.sh` - This generates `.diag` files from `.cbor`
  files

* `./script/lint-prelude.sh` - This freezes and lints the Prelude

## How do I get the "commit bit"?

Just ask!  I (@Gabriel439) hand out the commit bit freely to anybody who
displays sustained interest in the project, sometimes even if they don't ask.
However, you shouldn't assume that you are undeserving if I haven't given you
the commit bit.  I might just be busy or forgetful, so I appreciate a helpful
reminder.  You can ask directly by e-mail (see my account profile) or by opening
an issue, whichever you prefer.

I hand out the commit bit freely because mistakes are easy to fix or roll back.
Learn by doing and get your hands dirty!

[infrastructure]: https://github.com/dhall-lang/dhall-lang/tree/master/nixops
[standard]: https://github.com/dhall-lang/dhall-lang/tree/master/standard
[prelude]: https://github.com/dhall-lang/dhall-lang/tree/master/Prelude
[dhall-discourse]: https://discourse.dhall-lang.org/
[dhall-haskell-issues]: https://github.com/dhall-lang/dhall-haskell/issues
[dhall-lang-issues]: https://github.com/dhall-lang/dhall-lang/issues
[dhall-lang-pulls]: https://github.com/dhall-lang/dhall-lang/pulls
[dhall-lang-tests]: https://github.com/dhall-lang/dhall-lang/tree/master/tests
[dhall-json-issues]: https://github.com/dhall-lang/dhall-json/issues
[stack-overflow]: https://stackoverflow.com/
