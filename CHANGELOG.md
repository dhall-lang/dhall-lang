# Changelog

All notable changes to the Dhall language standard will be documented in this
file.

For more info about our versioning policy, see [versioning.md](standard/versioning.md).

## `v17.1.0`

New features:

* [Allow trailing delimiters](https://github.com/dhall-lang/dhall-lang/pull/956)

  The language now permits trailing delimiters, including:

  * Trailing commas for record literals:

    ```dhall
    { x = 1, y = 2, }
    ```

  * Trailing commas for record types:

    ```dhall
    { x : Natural, y : Natural, }
    ```

  * Trailing commas for list literals:

    ```dhall
    [ 1, 2, ]
    ```

  * Trailing bars for union types:

    ```dhall
    < Left : Natural | Right : Bool | >
    ```

  These trailing delimiters will allow you to format your code in a style
  more familiar to JavaScript / Python / Go / Rust programmers.

* [Add `.dhall` extensions on Prelude files](https://github.com/dhall-lang/dhall-lang/pull/1026)

  All Prelude files now include a version with a `.dhall` extension, both for
  consistency with the rest of the ecosystem and to automatically trigger
  correct syntax highlighting and editor support based on the `.dhall`
  extension.

  The old files without the extension are still present (so this change is
  backwards-compatible), but now all they do is re-export the file with the
  `.dhall` extension.

  New additions to the Prelude will require the `.dhall` extension and won't
  include the extension-free counterpart.

Other changes:

* Fixes and improvements to the standard:

  * [Update import section in standard for 17.0.0](https://github.com/dhall-lang/dhall-lang/pull/1030)
  * [Fix code block syntax for multi-line strings judgment](https://github.com/dhall-lang/dhall-lang/pull/1039)

## `v17.0.0`

Breaking changes:

* [Remove the ability to quote paths in URLs](https://github.com/dhall-lang/dhall-lang/pull/1005)

  After a deprecation period, quoted path components are no longer supported in
  URLs.  For example, this is no longer valid:

  ```dhall
  https://example.com/"foo bar"
  ```

  Instead, you will now need to percent-encode path components:

  ```dhall
  https://example.com/foo%20bar
  ```

  For more details, see: [Deprecation of quoted paths in URLs](https://docs.dhall-lang.org/howtos/migrations/Deprecation-of-quoted-paths-in-URLs.html)

* [Remove `Optional/build` and `Optional/fold`](https://github.com/dhall-lang/dhall-lang/pull/1014)

  After a deprecation period, `Optional/build` and `Optional/fold` are no longer
  built-ins and can be implemented in terms of other language constructs.

  Specifically, `Optional/build` can be replaced with `Prelude.Optional.build`,
  which is defined as:

  ```dhall
  λ(a : Type) →
  λ ( build
    : ∀(optional : Type) → ∀(some : a → optional) → ∀(none : optional) → optional
    ) →
    build (Optional a) (λ(x : a) → Some x) (None a)
  ```

  ... and `Optional/fold` can be replaced with `Prelude.Optional.fold`, which is
  defined as:

  ```dhall
  λ(a : Type) →
  λ(o : Optional a) →
  λ(optional : Type) →
  λ(some : a → optional) →
  λ(none : optional) →
    merge { None = none, Some = some } o
  ```

  For more details, see: [Deprecation of `Optional/fold` and `Optional/build`](https://docs.dhall-lang.org/howtos/migrations/Deprecation-of-Optional-fold-and-Optional-build.html)

New features:

* [Allow quoted labels to be empty](https://github.com/dhall-lang/dhall-lang/pull/980)

  You can now define records with blank labels, so long as you escape them with
  backticks, like this:

  ```dhall
  { `` = 1 }
  ```

* Fixes and improvements to the Prelude:

  * [Add `Prelude/List/unpackOptional`](https://github.com/dhall-lang/dhall-lang/pull/991)
  * [Fix `List/zip`](https://github.com/dhall-lang/dhall-lang/pull/995)
  * [Prelude: Reduce cache size of JSON rendering code](https://github.com/dhall-lang/dhall-lang/pull/1015)

Other changes:

* Fixes and improvements to the standard:

  * [Not all ABNF parsers like empty rules](https://github.com/dhall-lang/dhall-lang/pull/987)
  * [Ensure `keyword` rule only matches keywords](https://github.com/dhall-lang/dhall-lang/pull/1001)
  * [Remove Unicode character from `dhall.abnf`](https://github.com/dhall-lang/dhall-lang/pull/1004)

* Fixes and improvements to the standard test suite:

  * [Test that we typecheck `let` annotations](https://github.com/dhall-lang/dhall-lang/pull/1010)
  * [Add keyword record fields failure tests](https://github.com/dhall-lang/dhall-lang/pull/1013)

## `v16.0.0`

Breaking changes:

* [Adjust precedence of `===` and `with`](https://github.com/dhall-lang/dhall-lang/pull/954)

  This includes two precedence changes:

  * `with` expressions now forbid operators for their left-hand agument

    For example, now these parentheses are required:

    ```dhall
    ({ x = 0 } ∧ { y = 1 }) with x = 1
    ```

    Previously, if you omitted the parentheses, like this:

    ```dhall
    { x = 0 } ∧ { y = 1 } with x = 1
    ```

    ... that would have parsed as:

    ```dhall
    { x = 0 } ∧ ({ y = 1 } with x = 1)
    ```

    ... but now you would get a failed parse if you were to omit the
    parentheses.

    This restriction forbids expressions that might have been ambiguous to
    readers.  This is definitely a breaking change, since such expressions were
    previously valid now require explicit parentheses, otherwise they will fail
    to parse.

    Along the same lines, an expression like this:

    ```dhall
    record with x.y = {} // { x = 1 }
    ```

    ... used to parse as:

    ```dhall
    (record with x.y = {}) // { x = 1 }
    ```

    ... but now parses as:

    ```dhall
    record with x.y = ({} // { x = 1 })
    ```

    ... which is a different expression.

    The motivation for the latter change in precedence is to permit right-hand
    sides that contain operators without explicit parentheses, like this:

    ```dhall
    record with x.y = 2 + 2
    ```

  * The precedence of `===` is now lower than all of the operators

    The motivation for this change is to that you no longer need to parenthesize
    tests just because they use operators.  For example, previously the
    following parentheses were mandatory:

    ```dhall
    let example = assert : (2 + 2) === 4
    ```

    ... and now you can safely omit the parentheses:

    ```dhall
    let example = assert : 2 + 2 === 4
    ```

    This part is not a breaking change because any expression affected by this
    change in precedence would not have type-checked anyway.

* [Update encoding of floating point values to RFC7049bis](https://github.com/dhall-lang/dhall-lang/pull/958)

  This changes how `Double` literals are encoded to match a new standard
  recommendation how to canonically encode floating point values.  Specifically,
  these values are now encoded using the smallest available CBOR representation,
  (which could be half precision floating point values).  Previously, the Dhall
  standard would always use at least 32 bits for encoding floating point values
  (except for special values like `Infinity` or `NaN`).

  This is a technically breaking change because this affects the computed
  integrity checks for frozen imports, but you are less likely to hit this in
  practice because: (A) usually only Dhall packages containing reusable
  utilities are frozen using integrity checks and (B) there are essentially
  no useful utilities one could write in Dhall using specific `Double` literals
  since they are opaque.

New features:

* [Allow unions with mixed kinds](https://github.com/dhall-lang/dhall-lang/pull/957)

  Now, a union type like this one is valid:

  ```dhall
  < x : Bool | y | z : Type > 
  ```

  ... or more generally you can mix terms, types, and kinds within the same
  union (similar to how you can mix them within records).

  Besides permitting more expressions this change also simplifies the standard.

* New additions to the Prelude

  * [Add `Prelude.List.index`](https://github.com/dhall-lang/dhall-lang/pull/966)
  * [Add `Text/{replicate,spaces}`](https://github.com/dhall-lang/dhall-lang/pull/967)
  * [Add `List/zip`](https://github.com/dhall-lang/dhall-lang/pull/971)

Other changes:

* Fixes and improvements to the standard:

  * [Improve whitespace consistency](https://github.com/dhall-lang/dhall-lang/pull/947)
  * [Add missed type-inference context sets](https://github.com/dhall-lang/dhall-lang/pull/948)
  * [Clarify `as Location` semantics](https://github.com/dhall-lang/dhall-lang/pull/972)

* Fixes and improvements to the standard test suite:

  * [Add regression test for partially saturated `{List,Natural}/fold`](https://github.com/dhall-lang/dhall-lang/pull/950)
  * [Remove dependency on `csrng.net`](https://github.com/dhall-lang/dhall-lang/pull/969)
  * [Add test that we ignore poisoned cache entries](https://github.com/dhall-lang/dhall-lang/pull/983)

## `v15.0.0`

Deprecation notice:

* [URLs with quoted paths are deprecated](https://docs.dhall-lang.org/howtos/migrations/Deprecation-of-quoted-paths-in-URLs.html)

  In other words, you will no longer be able to do this:

  ```dhall
  https://example.com/"foo bar"
  ```

  Instead, you will need to percent-encode URLs, like this:

  ```dhall
  https://example.com/foo%20bar
  ```

  Support for quoted URL paths will be removed in two more releases in an effort
  to slowly simplify the standard.  See the above link to the migration guide
  for more details.

Breaking changes:

* [Add support for `with` keyword](https://github.com/dhall-lang/dhall-lang/pull/923)

  You can now update a deeply-nested record using the `with` keyword, like this:

  ```dhall
  let record = { x.y = 1 }

  in  record with x.y = 2 with x.z = True
  ```

  ... which evaluates to:

  ```dhall
  { x = { y = 2, z = True } }
  ```

  The `with` keyword is syntactic sugar for using `//` repeatedly to override
  nested fields.  For example, this expression:

  ```dhall
  record with x.y = 2
  ```

  ... is syntactic sugar for:

  ```dhall
  record // { x = record.x // { y = 2 } }
  ```

  This feature is a technically breaking change because the `with` keyword is
  now reserved.

* [JSON: support pretty JSON formatting](https://github.com/dhall-lang/dhall-lang/pull/936)

  `Prelude.JSON.render` now pretty-prints JSON instead of rendering on a single
  line

  For example:

  ```dhall
  JSON.render
    ( JSON.array
        [ JSON.bool True
        , JSON.string "Hello"
        , JSON.object
            [ { mapKey = "foo", mapValue = JSON.null }
            , { mapKey = "bar", mapValue = JSON.double 1.0 }
            ]
        ]
    )
  ```

  ... evaluates to:

  ```dhall
  ''
  [
    true,
    "Hello",
    {
      "foo": null,
      "bar": 1.0
    }
  ]
  ''
  ```

  This feature is a breaking change to the output (the old compact rendering
  was not preserved), but the generated JSON is semantically the same.

New features:

* [Standardize record puns](https://github.com/dhall-lang/dhall-lang/issues/867)

  The language now supports record "puns" to easily create fields from
  identifiers of the same name already in scope.

  For example, this expression:

  ```dhall
  let x = 1

  let y = 2

  in  { x, y }
  ```

  ... is the same thing as:

  ```dhall
  let x = 1

  let y = 2

  in  { x = x, y = y }
  ```

  This comes in handy when creating records that package several locally-defined
  identifiers.

  Note that this does not yet support the same syntax for destructuring records.
  Currently this feature only affects record assembly.

Other changes:

* Fixes and improvements to the standard:

  * [`using toMap` requires parentheses](https://github.com/dhall-lang/dhall-lang/pull/914)
  * [Don't parse single-quote strings too greedily](https://github.com/dhall-lang/dhall-lang/pull/911)
  * [Fix keyword ordering for simple-label rule](https://github.com/dhall-lang/dhall-lang/pull/916)
  * [Tweak instructions for how to run import tests](https://github.com/dhall-lang/dhall-lang/pull/917)
  * [Add a bunch of tests](https://github.com/dhall-lang/dhall-lang/pull/933)

* Fixes and improvements to the Prelude:

  * [Fix `JSON/renderYAML` for singleton arrays/objects](https://github.com/dhall-lang/dhall-lang/pull/929)
  * [Change `renderYAML` to not emit exclamation marks](https://github.com/dhall-lang/dhall-lang/pull/934)

## `v14.0.0`

Deprecation notice:

* [The `Optional/fold` and` `Optional/build` built-ins are deprecated](https://docs.dhall-lang.org/howtos/migrations/Deprecation-of-Optional-fold-and-Optional-build.html)

  These built-ins will be removed in three more releases in an effort to slowly
  simplify the standard.  See the above link to the migration guide for more
  details.

Breaking changes:

* [Disallow Natural literals with leading zeros](https://github.com/dhall-lang/dhall-lang/pull/898)

  `Natural` numbers can no longer start with a leading zero, for two main
  reasons:

  * So that users will not confuse them with octal numbers

  * So that implementations are less likely to inadvertently parse them as octal
    numbers

New features:

* [Add support for duplicate record fields](https://github.com/dhall-lang/dhall-lang/pull/896)

  Up until now, the Dhall interpreter would reject duplicate fields within
  records, such as:

  ```dhall
  { a = { b = 1 }, a = { c = 1 } }
  ```

  However, several other configuration languages permit duplicate fields so
  long as (A) they are records and (B) their nested fields do not conflict
  (such as in the above example).  Now the Dhall configuration language behaves
  that way, too.

  Specifically, the rules are that more than one occurrence of a field desugars
  to use of the `∧` operator to combine the duplicate occurrences.  So, for
  example, the above expression is now syntactic sugar for:

  ```dhall
  { a = { b = 1 } ∧ { c = 1 } }
  ```

  ... which then further evaluates to:

  ```dhall
  { a = { b = 1, c = 1 } }
  ```

  Other cases for duplicate fields are still rejected as type errors.  For
  example, this expression:

  ```dhall
  { a = 0, a = 0 }
  ```

  ... desugars to:

  ```dhall
  { a = 0 ∧ 0 }
  ```

  ... which is a type error.

  This feature combines well with the following feature for dotted field
  syntax.

* [Add support for dotted field syntax](https://github.com/dhall-lang/dhall-lang/pull/901)

  You can now compress deeply nested record hierarchies by chaining nested
  fields with dots.

  For example, the following record:

  ```dhall
  { a.b.c = 1 }
  ```

  ... is syntactic sugar for nesting records like this:

  ```dhall
  { a = { b = { c = 1 } } }
  ```

  You can combine this feature with the previous feature for duplicate
  fields.  For example, the following record:

  ```dhall
  { a.b.c = 1, a.b.d = True }
  ```

  ... desugars to:

  ```dhall
  { a = { b = { c = 1 } }, a = { b = { d = True } } }
  ```

  ... and the duplicate fields merge to produce:

  ```dhall
  { a = { b = { c = 1, d = True } } }
  ```

* [Fix typing rule for merging `Optional`s](https://github.com/dhall-lang/dhall-lang/pull/899)

  The previous released introduced `merge` support for `Optional` values, such
  as:

  ```dhall
  merge { None = 0, Some = λ(n : Natural) → n } (Some 4)
  ```

  However, the final argument to `merge` was limited to `Optional` literals
  (e.g. `Some` or `None`), but did not work on abstract `Optional` values, such
  as bound variables.  For example, the following example would fail to
  type-check:

  ```dhall
  λ(o : Optional Natural) → merge { None = 0, Some = λ(n : Natural) → n } o
  ```

  This release fixes that and the above example is now valid Dhall code.

## `v13.0.0`

Breaking changes:

* [Extend Prelude's JSON type to handle unlimited precision `Integer`s and `Natural`s](https://github.com/dhall-lang/dhall-lang/pull/873)

  This adds new `Prelude.JSON.{double,integer,natural}` utilities and the
  latter two utilities can store arbitrary `Integer`s and `Natural` without
  loss of precision.

  For example:

  ```dhall
  let JSON = https://prelude.dhall-lang.org/JSON/package.dhall

  in  JSON.render (JSON.natural 1000000000000000000000000000000)
  ```

  `Prelude.JSON.number` still remains and is a synonym for `Prelude.JSON.double`

  This is a technically breaking change because this alters `Prelude.JSON.Type`
  but in practice this will not break code that treats that type as a black box.

New features:

* [Extend `merge` to work on `Optional`s](https://github.com/dhall-lang/dhall-lang/pull/860)

  You can use `merge` on `Optional`s as if they were a union, like this:

  ```dhall
  merge { None = False, Some = λ(b : Bool) → b } (Some True)
  ```

* [Add support for hexadecimal numbers](https://github.com/dhall-lang/dhall-lang/pull/859)

  You can now use hexadecimal literals for `Natural`s and `Integer`s, like this:

  ```dhall
  0xFF

  -0xE1
  ```

* New additions to the Prelude

  * [Add `Prelude.Integer.{negative,nonNegative,nonPositive,positive}`](https://github.com/dhall-lang/dhall-lang/pull/857)
  * [Add `Prelude.Function.identity`](https://github.com/dhall-lang/dhall-lang/pull/865)

Other changes:

* Fixes and improvements to the Prelude:

  * [Simplify `Prelude.Integer.equal`](https://github.com/dhall-lang/dhall-lang/pull/842)
  * [Simplify `Prelude.Integer.{greater,less}Than{,Equal}`](https://github.com/dhall-lang/dhall-lang/pull/843)
  * [Simplify `Prelude.Integer.toNatural`](https://github.com/dhall-lang/dhall-lang/pull/844)
  * [Make `Prelude.Integer.multiply` slightly more efficient](https://github.com/dhall-lang/dhall-lang/pull/846)
  * [Improve `Prelude.Integer.subtract`](https://github.com/dhall-lang/dhall-lang/pull/845)

* Fixes and improvements to the standard:

  * [Some small grammar fixes](https://github.com/dhall-lang/dhall-lang/pull/871)

## `v12.0.0`

Breaking changes:

* [New `Integer/negate` and `Integer/clamp` builtins](https://github.com/dhall-lang/dhall-lang/pull/780)

  This change adds two new built-ins so that `Integer`s are no longer opaque.
  These two built-ins permit transforming back and forth between `Integer`s and
  `Natural` numbers, so you can in theory now implement arbitrary operations on
  `Integer`s.

  These two built-ins have the following types:

  * `Integer/clamp : Integer → Natural` - Converts an `Integer` to a `Natural`
     number, truncating to `0` if the `Integer` was negative

  * `Integer/negate : Integer → Integer` - Negates an `Integer`

  See below for the matching change to the Prelude to use these built-ins to
  power several new Prelude utilities.

* [Remove support for fusion](https://github.com/dhall-lang/dhall-lang/pull/792)

  This removes support for `Natural`/`List`/`Optional` "fusion".

  For example, before this change an expression such as:

  ```dhall
  λ(x : Natural) → Natural/build (Natural/fold x)
  ```

  ... would simplify to:

  ```dhall
  λ(x : Natural) → x
  ```

  ... or in other words paired occurrences of `Natural/build` and `Natural/fold`
  would "fuse" away and disappear since they are inverses of one another.
  Similarly, `List/build`/`List/fold` and `Optional/build`/`Optional/fold` would
  fuse away in the same way.

  After this change they no longer do so.  We removed language support for
  fusion for two reasons:

  * Fusion was specified in such a way that the language was no longer
    [confluent](https://en.wikipedia.org/wiki/Confluence_(abstract_rewriting))

    Note: We have not proven that the language is now confluent in the absence
    of fusion, but we are certain that fusion was interfering with confluence.

    A practical consequence of the absence confluence was that fusion-related
    optimizations were brittle

  * Fusion added implementation complexity

    ... which in turn increased the difficulty of porting new language bindings

  This is a technically breaking change because the normal forms for certain
  expressions will differ if they relied on fusion, which in turn would perturb
  semantic integrity checks protecting those expressions.  In practice, you are
  unlikely to be affected by this change.

New features:

* [Add new `Integer` functions to Prelude](https://github.com/dhall-lang/dhall-lang/pull/797)

  This change takes advantage of the newly added `Integer/{clamp,negate}`
  built-ins to add the following operations on `Integer`s to the Prelude:

  * `Integer.abs : Integer → Natural`

  * `Integer.add : Integer → Integer → Integer`

  * `Integer.clamp : Integer → Natural`

  * `Integer.equal : Integer → Integer → Bool`

  * `Integer.greaterThan : Integer → Integer → Bool`

  * `Integer.greaterThanEqual : Integer → Integer → Bool`

  * `Integer.lessThan : Integer → Integer → Bool`

  * `Integer.lessThanEqual : Integer → Integer → Bool`

  * `Integer.multiply : Integer → Integer → Integer`

  * `Integer.negate : Integer → Integer`

  * `Integer.subtract: Integer → Integer → Integer`

  * `Integer.toNatural : Integer → Optional Natural`

* [Implement `renderYAML`](https://github.com/dhall-lang/dhall-lang/pull/799)

  You can now render `JSON` values as YAML documents using a new
  `Prelude.JSON.renderYAML` utility

  Note that this utility is not intended to be as featureful as the
  `dhall-to-yaml` command-line tool, but can still prove to be useful for:

  * Rendering YAML documents in pure Dhall when appropriate to do so
  * Rendering JSON values as YAML `Text` for use in concise `assert` expressions

* [Add a `Prelude.JSON.omitNullFields`](https://github.com/dhall-lang/dhall-lang/pull/784)

  While `renderYAML` doesn't aim to be as comprehensive as `dhall-to-yaml` we
  can still provide an `omitNull` utility which behaves similarly to the
  `--omitNull` flag for the command-line tool.  This function removes all
  `null`-valued record fields from a `JSON` expression:

  * `JSON/omitNull : JSON → JSON`

* [Add `List/{take,drop}`](https://github.com/dhall-lang/dhall-lang/pull/789)

  This change exploits the recently-added `Natural/subtract` built-in to
  implement high-performance list truncation utilities of the following types:

  * `List/take : Natural → ∀(a : Type) → List a → List a`

  * `List/drop : Natural → ∀(a : Type) → List a → List a`

* [Add `JSON.tag{Inline,Nested}` helpers for union encoding](https://github.com/dhall-lang/dhall-lang/pull/810)

  These are convenience utilities to support the `dhall-to-{json,yaml}`
  executables which provide a mechanism for converting union literals to
  tagged JSON records:

  * `JSON/tagInline : ∀(tagFieldName : Text) → ∀(a : Type) → ∀(contents : a) → { contents : a, field : Text, nesting : < Inline | Nested : Text > }`

  * `JSON/tagNested : ∀(contentsFieldName : Text) → ∀(tagFieldName : Text) → ∀(a : Type) → ∀(contents : a) → { contents : a, field : Text, nesting : < Inline | Nested : Text > }`

Other changes:

* Fixes and improvements to the standard:

  * [`dhall.abnf: Treat `forall` as a keyword, not an operator](https://github.com/dhall-lang/dhall-lang/pull/790)
  * [Treat `missing` as referentially transparent](https://github.com/dhall-lang/dhall-lang/pull/804)
  * [Fix type annotation standard text](https://github.com/dhall-lang/dhall-lang/pull/816)
  * [Remove reference to `empty-collection` rule](https://github.com/dhall-lang/dhall-lang/pull/813)
  * [Permit spaces around completion operator](https://github.com/dhall-lang/dhall-lang/pull/820)

* Fixes and improvements to the Prelude:

  * [Add a `README` for the Prelude](https://github.com/dhall-lang/dhall-lang/pull/793)

* Fixes and improvements to the standard test suite:

  * [Test cases for parsing "missing"](https://github.com/dhall-lang/dhall-lang/pull/788)
  * [Add tests for decoding big `Integer`s and `Natural`s](https://github.com/dhall-lang/dhall-lang/pull/819)
  * [Add tests for invalid unbraced unicode escapes and for valid unassigned unicode escapes](https://github.com/dhall-lang/dhall-lang/pull/802)

## `v11.1.0`

New features:

* [Prelude: Add `List.partition` and `Natural.{max,listMax,min,listMin,sort}`](https://github.com/dhall-lang/dhall-lang/pull/774)

Other changes:

* [Fix referentially opaque Prelude test](https://github.com/dhall-lang/dhall-lang/pull/781)

  This fixes the Prelude so that the top-level `package.dhall` can be imported
  remotely

* [Fix `assert` missing from keyword rule](https://github.com/dhall-lang/dhall-lang/pull/785)

* [Fix "curresponding" typo in ABNF](https://github.com/dhall-lang/dhall-lang/pull/787)

## `v11.0.0`

Breaking changes:

* [Simplify `⫽` within record projection](https://github.com/dhall-lang/dhall-lang/pull/697)

  This change allows the interpreter to simplify an expression like this:

  ```dhall
  (r ⫽ { x = 1, y = True }).{ x, z }
  ```

  ... to this:

  ```dhall
  r.{ z } ⫽ { x = 1 }
  ```

  This is a technically breaking change because it changes the normal form for
  expressions that can be simplified in this way, which in turn perturbs
  their hash if they are protected by a semantic integrity check.  However, in
  practice this is unlikely to disrupt your code.

* [Simplify nested record projection](https://github.com/dhall-lang/dhall-lang/pull/729)

  This change allows the interpreter to simplify an expression like this:

  ```dhall
  r.{ x, y, z }.{ x, y }
  ```

  ... to this:

  ```dhall
  r.{ x, y }
  ```

  This is a technically breaking change because it changes the normal form for
  expressions that can be simplified in this way, which in turn perturbs
  their hash if they are protected by a semantic integrity check.  However, in
  practice this is unlikely to disrupt your code.

New features:

* [Add support for leading separators](https://github.com/dhall-lang/dhall-lang/pull/755)

  Record literals, record types, and union types all now support leading
  separators, like this:

  ```dhall
  { , x = 1, y = True }

  { , x : Natural, y : Bool }

  < | x : Natural | y : Bool >
  ```

  ... which are more commonly used when formatting a multi-line expression so
  that you can always add/remove entries with only a single-line change.

  ```dhall
  let example = {
        , x = 1
        , y = True
        }
  ```

* [Standardize support for record completion](https://github.com/dhall-lang/dhall-lang/pull/767)

  This adds a new operator:

  ```dhall
  T::r
  ```

  ... which is syntactic sugar for:

  ```dhall
  (T.default ⫽ r) : T.Type
  ```

  The motivation for this change is to handle configuration formats with a large
  number of defaultable fields so that users can omit any default-valued fields
  when creating a record:

  ```dhall
  let Example =
        { Type = { foo : Text, bar : Optional Natural }
        , default = { bar = None Natural }
        }

      -- No need to specify `bar` since it will default to `None Natural`
  in  Example::{ foo = "ABC" }
  ```

* [Improved Windows support for caching](https://github.com/dhall-lang/dhall-lang/pull/730)

  Interpreters will now use Windows-appropriate cache directories
  (i.e. `%LOCALAPPDATA%`) when available

* [Prelude: Include types in the `package.dhall` files](https://github.com/dhall-lang/dhall-lang/pull/732)

  You can now access `Prelude.Map.Type`, `Prelude.JSON.Type` and other types
  from the Prelude's `package.dhall`

* [Prelude: Add `List.{default,empty}`, `Map.empty`, `Optional.default`](https://github.com/dhall-lang/dhall-lang/pull/764)

  This adds the following new Prelude utilities:

  ```dhall
  Prelude.List.default
    : ∀(a : Type) → Optional (List a) → List a

  Prelude.List.empty
    : ∀(a : Type) → List a

  Prelude.Map.empty
    : ∀(k : Type) → ∀(v : Type) → Map k v

  Prelude.Optional.default
    : ∀(a : Type) → a → Optional a → a
  ```

* [Prelude: Move `JSON.key{Text,Value}` to `Map`](https://github.com/dhall-lang/dhall-lang/pull/772)

  `Prelude.JSON.keyText` is now also available from `Prelude.Map.keyText`

  Similarly, `Prelude.JSON.keyValue` is now also available from
  `Prelude.Map.keyValue`

Other changes:

* Fixes and improvements to the standard

  * [β-normalize all inferred types](https://github.com/dhall-lang/dhall-lang/pull/745)

    Now all inferred types are in β-normal form, which improves performance
    and also makes the standard simpler and more consistent

  * [Normalize the inferred type of a `∧`-expression](https://github.com/dhall-lang/dhall-lang/pull/722)
  * [Generalize empty record projections](https://github.com/dhall-lang/dhall-lang/pull/720)
  * [Clarify type inference for record literals](https://github.com/dhall-lang/dhall-lang/pull/740)
  * [Fix type-inference rules for `merge`](https://github.com/dhall-lang/dhall-lang/pull/762)
  * [Specify binary encoding of `-0.0`](https://github.com/dhall-lang/dhall-lang/pull/761)
  * [Specify how to encoded union constructors](https://github.com/dhall-lang/dhall-lang/pull/765)

* Fixes and improvements to the standard test suite:

  * [Merge `typecheck` and `type-inference` tests](https://github.com/dhall-lang/dhall-lang/pull/723)
  * [Add failure test for `toMap` of record of kind `Kind`](https://github.com/dhall-lang/dhall-lang/pull/728)
  * [Fix the `tests/type-inference/success/prelude` test](https://github.com/dhall-lang/dhall-lang/pull/736)
  * [Test that the type annotation on a `toMap` of an empty record is normalized](https://github.com/dhall-lang/dhall-lang/pull/744)
  * [Fix the `toMapEmptyNormalizeAnnotation` test case](https://github.com/dhall-lang/dhall-lang/pull/746)
  * [Replace type-inference failure test for mismatching `merge` annotation](https://github.com/dhall-lang/dhall-lang/pull/747)
  * [Add regression test](https://github.com/dhall-lang/dhall-lang/pull/750)
  * [More tests, mostly parsing and type-inference](https://github.com/dhall-lang/dhall-lang/pull/758)
  * [Merge type-inference tests using imports into import tests](https://github.com/dhall-lang/dhall-lang/pull/763)

* Fixes and improvements to the Prelude:

  * [Small improvements for `Prelude.Natural`](https://github.com/dhall-lang/dhall-lang/pull/719)

## `v10.0.0`

Breaking changes:

* [Remove old-style union literals from the language](https://github.com/dhall-lang/dhall-lang/pull/573)

  This is the final phase of removing support for the old union literal syntax
  and they are no longer supported.

  Now, instead of writing this:

  ```dhall
  < Left = 1 | Right : Bool >
  ```

  ... you must write this:

  ```dhall
  < Left : Natural | Right : Bool >.Left 1
  ```

  ... or more commonly:

  ```dhall
  let Example = < Left : Natural | Right : Bool >

  in  Example.Left 1
  ```

  For more details, including migration instructions, see: [Migration: Deprecation of old union literal syntax](https://github.com/dhall-lang/dhall-lang/wiki/Migration%3A-Deprecation-of-old-union-literal-syntax)

* [Add support for dependent types](https://github.com/dhall-lang/dhall-lang/pull/669)

  Dhall now provides rudimentary support for dependent types, which in turn
  enables a new `assert` keyword for testing code.

  For example, now you can write unit tests for your functions like this:

  ```dhall
  let not = λ(x : Bool) → x == False

  let example0 = assert : not True === False

  -- (≡) (U+2261) is the Unicode analog of (===)
  let example1 = assert : not False ≡ True

  in  not
  ```

  You can also write property tests in the same way, too:

  ```dhall
  let rightIdentity = λ(x : Natural) → assert : x ≡ (x + 0)

  let leftIdentity  = λ(x : Natural) → assert : x ≡ (0 + x)

  -- Note: This last assertion will fail because Dhall's simplifier currently
  -- cannot detect that re-associated expressions are equivalent
  let associativity =
          λ(x : Natural)
        → λ(y : Natural)
        → λ(z : Natural)
        → assert : (x + (y + z)) === ((x + y) + z)

  in  {=}
  ```

  Dhall is *technically* dependently typed, meaning that you can now have
  functions from terms to types, like this:

  ```dhall
  let Tagged = λ(label : Text) → λ(a : Type) → a
  
  in  1 : Tagged "Age" Natural
  ```

  ... but Dhall does not yet support other features that would be required to
  do more sophisticated things like statically sized `Vector`s.  The main
  new benefit for users is language support for tests.

  This is a technically breaking change because `assert` is now a reserved
  keyword.

* [New `Natural/subtract` built-in](https://github.com/dhall-lang/dhall-lang/pull/650)

  This adds a new `Natural/subtract` built-in which truncates to `0` if the
  result is negative.

  The built-in is useful in its own right, but can also be used to power
  other utilities (such as efficient comparison of `Natural` numbers, also
  included in this releases)

  This is a technically breaking change because `Natural/subtract` is now a
  reserved identifier, but in practice this is unlikely to break your code
  unless you defined your own inefficient `Natural/subtract` function in terms
  of `Natural/fold`.  If you did so, then just delete the old code and switch
  to the new built-in.

  See also: [Simplify `Natural/subtract` when its arguments are equivalent](https://github.com/dhall-lang/dhall-lang/pull/685)

* [New simplifications for field selection](https://github.com/dhall-lang/dhall-lang/pull/664)

  The interpreter will now intelligently normalize some unsaturated field
  selections and projections.

  For example, the following code:

  ```dhall
  λ(x : { a : Bool, b : Bool }) → (x ⫽ { c = 0 }).{ a, c }.c
  ```

  ... will simplify to:

  ```dhall
  λ(x : { a : Bool, b : Bool }) → 0 
  ```

  This is a technically breaking change because it changes the normal form for
  expressions that can be simplified in this way, which in turn perturbs
  their hash if they are protected by a semantic integrity check.  However, in
  practice this is unlikely to disrupt your code.

  See also: [Add missing symmetric rules for field selection normalization](https://github.com/dhall-lang/dhall-lang/pull/682)

* [Simplify `//` when its arguments are equivalent](https://github.com/dhall-lang/dhall-lang/pull/684)

  The interpreter will now simplify `x // x` to `x`

  This is a technically breaking change for the same reason as the previous
  change: because it changes the normal form for expressions using this
  feature.

* [Don't URL-decode path segments](https://github.com/dhall-lang/dhall-lang/pull/704)

  This changes the the binary representation of URLs to pass through path
  segments without decoding them for better interoperability with other tools.

  This is a technically breaking change because the binary format changes for
  URLs, but this does not disturb semantic integrity checks since they hash
  URL-free expressions.

New features:

* [Standardize mixed records](https://github.com/dhall-lang/dhall-lang/pull/689)

  You can now have mixed records of terms, types, and kinds.  For example,
  something like this is now legal:

  ```dhall
  { foo = 1, bar = Text }
  ```

  Practically, this means that Dhall packages can now export both types and
  terms from the same record, so that they no longer need a separate
  `types.dhall` record.

* [Prelude: Add `Natural` comparison functions](https://github.com/dhall-lang/dhall-lang/pull/674)

  You can now use high-performance `Natural` comparison functions which are
  internally powered by the newly-added `Natural/subtract` built-in.

Other changes:

* Fixes and improvements to the standard:

  * [Fix `toMap` type inference (and some `merge` tweaks)](https://github.com/dhall-lang/dhall-lang/pull/671)
  * [[`Natural/subtract`] minor clarification in normalization](https://github.com/dhall-lang/dhall-lang/pull/678)
  * [β-normalization: Simplify some equivalence-based rules](https://github.com/dhall-lang/dhall-lang/pull/687)
  * [Fix RFC5234-compliance of the grammar](https://github.com/dhall-lang/dhall-lang/pull/694)
  * [Small tweak for auto-generated parsers](https://github.com/dhall-lang/dhall-lang/pull/699)
  * [Small fix for β-normalization](https://github.com/dhall-lang/dhall-lang/pull/696)
  * [Use `⩓` to typecheck `∧`](https://github.com/dhall-lang/dhall-lang/pull/706)
  * [Clarify that dependent functions are allowed now](https://github.com/dhall-lang/dhall-lang/pull/715)

* Fixes and improvements to the Prelude:

  * [Document connection to `Monoid` for `Bool/even` and `Bool/odd`](https://github.com/dhall-lang/dhall-lang/pull/683)
  * [Add `Text/default{,Map}` to Prelude](https://github.com/dhall-lang/dhall-lang/pull/713)

* Fixes and improvements to the standard test suite:

  * [Test import alternative with type error](https://github.com/dhall-lang/dhall-lang/pull/662)
  * [Test binary decoding of label 28](https://github.com/dhall-lang/dhall-lang/pull/667)
  * [Make `as Location` tests chain in a consistent way](https://github.com/dhall-lang/dhall-lang/pull/633)
  * [Test binary decoding of label 27](https://github.com/dhall-lang/dhall-lang/pull/677)
  * [Test that nested lets are flattened in binary](https://github.com/dhall-lang/dhall-lang/pull/673)
  * [Test integrity check on nested imports](https://github.com/dhall-lang/dhall-lang/pull/672)
  * [Add normalization test for (`Natural/subtract 0`)](https://github.com/dhall-lang/dhall-lang/pull/692)
  * [Add a bunch of tests](https://github.com/dhall-lang/dhall-lang/pull/698)
  * [Swap A and B in `BuiltinNaturalSubtract` binary-decode tests](https://github.com/dhall-lang/dhall-lang/pull/695)
  * [Make normalization tests type-correct](https://github.com/dhall-lang/dhall-lang/pull/703)
  * [Fix `tests/typecheck/success/prelude`](https://github.com/dhall-lang/dhall-lang/pull/708)
  * [Various test fixes](https://github.com/dhall-lang/dhall-lang/pull/716)

## `v9.0.0`

Breaking changes:

* [Remove old `Optional` literal syntax](https://github.com/dhall-lang/dhall-lang/pull/572)

  This is phase 2 of removing support for the old `List`-like `Optional` literal
  syntax.

  This phase removes the old `Optional` literals from the language.  You now
  must use `Some` or `None`.

  For more details, including migration instructions, see:
  [Migration: Deprecation of old `Optional` literal syntax](https://github.com/dhall-lang/dhall-lang/wiki/Migration%3A-Deprecation-of-old-Optional-literal-syntax)

* [Forbid surrogate pairs and non-characters](https://github.com/dhall-lang/dhall-lang/pull/640)

  Dhall no longer supports surrogate pairs within escape sequences.  In other
  words, something like this is no longer valid:

  ```dhall
  "\uD834\uDD1E"
  ```

  ... and you must instead use a braced escape sequence to represent a Unicode
  character that would have previously required a surrogate pair:

  ```dhall
  "\u{1D11E}"
  ```

  Dhall also no longer supports escape sequences for non-characters, so
  something like this is also no longer valid: 

  ```dhall
  "\uFFFF"
  ```

  Surrogate pairs and non-characters are also now explicitly forbidden within
  the source code (i.e. within comments or unescaped text literals), but these
  were already forbidden before.  Dhall source code has to be valid UTF8, which
  already disallows those characters.

* [Add `toMap` keyword to create homogeneous maps from records](https://github.com/dhall-lang/dhall-lang/pull/610)

  You can now create an association list (a.k.a. a `Map`) from a Dhall record if
  all of the values stored within the record have the same type.  For
  example:

  ```dhall
  toMap { foo = 1, bar = 2 }

  = [ { mapKey = "foo", mapValue = 1 }, { mapKey = "bar", mapValue = 2 } ]
  ```

  This allows a Dhall binding to a configuration format to accept a list of
  key-value pairs if it doesn't know in advance what keys to expect, while
  still permitting the user to specify the key-value pairs using record-like
  syntax.

  Two other features within this same release take advantage of this:

  * You can now specify custom headers for imports using the record
    generated by `toMap`, like this:

    ```dhall
    https://example.com/foo
      using toMap { Authorization = "token ${env:GITHUB_TOKEN as Text }" }
    ```

  * You can specify key-value pairs for the `JSON` type just added to the
    Prelude using `toMap`, too:

    ```dhall
    let JSON = https://prelude.dhall-lang.org/JSON/package.dhall

    in  JSON.object (toMap { foo = JSON.number 1.0, bar = JSON.string "baz" })
    ```

  This is a technically breaking change because `toMap` is now a reserved
  keyword, but in practice most users will not be affected unless they used
  that label before this change.

* [Beta-normalization: Sort the fields of a record projection](https://github.com/dhall-lang/dhall-lang/pull/625)

  Normalization will now sort the fields of a record projection if it cannot
  be further reduced.  For example, this expression:

  ```dhall
  λ(x : { a : Bool, b : Bool, c : Bool }) → x.{ c, a }
  ```

  ... now normalizes to:

  ```dhall
  λ(x : { a : Bool, b : Bool, c : Bool }) → x.{ a, c }
  ```

  This is a technically breaking change because it might perturb the hash of
  any expression that contains an irreducible record projection, but in
  practice this should not affect most users.

New features:

* [Implement importing paths `as Location`](https://github.com/dhall-lang/dhall-lang/pull/599)

  Dhall now permits reflection on fully resolved import paths so that these
  paths remain up-to-date no matter where the configuration file is
  interpreted from.

  For example, suppose that you store the following configuration file at
  `/home/john/example.dhall`:

  ```dhall
  { packagePath = "./purescript-simple-json"
  }
  ```

  ... where `packagePath` is intended to point to
  `/home/john/purescript-simple-json`.

  ... but then you use that to configure a program running with a different
  current working directory, such as: `PWD=/home/alice/`.  That program will
  fail to resolve the package path because `/home/alice/purescript-simple-json`
  does not exist.

  However, if you change the configuration to:

  ```dhall
  { packagePath = ./purescript-simple-json as Location
  }
  ```

  Then the interpreter will replace the import with an expression encoding the
  absolute path to the import, generating an expression equivalent to this:

  ```dhall
  let Location = https://prelude.dhall-lang.org/Location/Type

  in  { packagePath = Location.Local "/home/john/purescript-simple-json" }
  ```

  ... so that the Dhall configuration file can be used to configure a program
  running within any working directory.

  If the same file were hosted at `https://example.com/john/example.dhall` then
  the expression would evaluate to:

  ```dhall
  let Location = https://prelude.dhall-lang.org/Location/Type

  in  { packagePath = Location.Remote "https://example.com/john/purescript-simple-json" }
  ```

* [Allow all RFC3986-compliant URLs](https://github.com/dhall-lang/dhall-lang/pull/604)

  Now all URLs are valid imports.

  For example, before this change `https://example.com` was not a valid import
  due to not having any path components and after this change URLs without
  paths are valid.

  This change also enables:

  * URLs with empty path components (i.e. `https://example.com///`)
  * Path components with unquoted special characters, such as `=`

* [Generalize empty list annotations](https://github.com/dhall-lang/dhall-lang/pull/630)

  You can now annotate an empty list with a type synonym, like this:

  ```dhall
  let Example = List Natural

  in  [] : Example
  ```

  Before this change, the `List` in `: List` was part of the grammar for empty
  lists, and you could only create a type synonym for the list element type,
  but not the list type as a whole.  Now you can create a type synonym for the
  list type.

* [Add `Map` type and utility functions to Prelude](https://github.com/dhall-lang/dhall-lang/pull/575)

  The Prelude is now enshrining the Dhall idiom of using
  `List { mapKey : Text, mapValue : a }` to represent homogeneous maps by
  adding basic types and utilities for working with values of that type.

  This change pairs well with the following matching changes in this release:

  * [Add `toMap` keyword to create homogeneous maps from records](https://github.com/dhall-lang/dhall-lang/pull/610)
  * [Use `Prelude/Map` for import headers](https://github.com/dhall-lang/dhall-lang/pull/611)

* [Use multihash for cache filenames](https://github.com/dhall-lang/dhall-lang/pull/584)

  Dhall caches imports protected by semantic integrity checks underneath
  `${XDG_CACHE_HOME}/dhall` or `~/.cache/dhall` and this change affects the
  names of the cache files, which are now preceded with the four characters
  `1220` to reflect
  [the multi-hash standard](https://github.com/multiformats/multihash#table-for-multihash).

  This change means that the interpreter will not reuse old cache files when
  upgrading from an older release, so the cache will be rebuilt upon the first
  run of the interpreter.  However, this is not a breaking change as this does
  not change the final result interpreting a protection protected by a semantic
  integrity check.

* [Add support for braced escape sequences](https://github.com/dhall-lang/dhall-lang/pull/580)

  You can now escape Unicode characters using braces, like this:

  ```dhall
  "Musical symbol G clef: \u{1D11E}"
  ```

  This allows Dhall to escape Unicode characters with code points greater than
  `0xFFFF` without the use of surrogate pairs.  This is necessary given that
  surrogate pair escape sequences are now forbidden within this same release.

* [Prelude: Add standard representation for weakly-typed JSON values](https://github.com/dhall-lang/dhall-lang/pull/586)

  Utilities like `dhall-to-{json,yaml}` and `{json,yaml}-to-dhall` have up until
  now only supported Dhall types with schemas known ahead-of-time.  However,
  some configuration file formats support fields that can store arbitrary JSON
  code (such as arbitrary JSON that the configuration intends to "pass through"
  to configure another step).

  This change adds a canonical type to the Prelude for representing an arbitrary
  JSON value and utilities for creating such JSON values.  For example:

  ```dhall
  let JSON = https://prelude.dhall-lang.org/JSON/package.dhall

  in  JSON.object
      ( toMap
        { foo = JSON.null
        , bar = JSON.array [ JSON.number 1.0, JSON.bool True ]
        }
      )
  ```

  Also, the matching release of the `dhall-json` package supports this
  weakly-typed representation anywhere within the schema when converting
  either way between JSON/YAML and Dhall configuration files.

* Use `Prelude/Map` for import headers

  You can now use a value of type `List { mapValue : Text, mapValue : Text }`
  to represent custom headers within an import's `using` clause instead of
  `List { header : Text, value : Text }`.  For example:

  ```dhall
  https://example.com/foo using
    [ { mapKey   = "Authorization"
      , mapValue = "token ${env:GITHUB_TOKEN as Text}"
      }
    ]
  ```

  ... or using the new `toMap` keyword:

  ```dhall
  https://example.com/foo using
    toMap { Authorization = "token ${env:GITHUB_TOKEN as Text}" }
  ```

  The old `header`/`value` form is still supported, so this is not a breaking
  change.  However, at some point in the future we may initiate the process of
  deprecating `header`/`value` support.

* [Add new `Prelude/XML` package](https://github.com/dhall-lang/dhall-lang/pull/637)

  There is now a `Prelude/XML` package that you can use to represent and
  render a subset of XML values using Dhall.  This will eventually be used to
  power a `dhall-to-xml` utility.

  For example:

  ```dhall
  let XML = https://prelude.dhall-lang.org/XML/package.dhall

  in  XML.render (XML.leaf { name = "foobar", attributes = XML.emptyAttributes })

  = "<foobar/>"
  ```

Other changes:

* Fixes and improvements to the standard:

  * [Use RFC3986 section 5 URL resolution algorithm](https://github.com/dhall-lang/dhall-lang/pull/593)
  * [Simpler way of incorporating RFC 3986 resolution](https://github.com/dhall-lang/dhall-lang/pull/603)
  * [Only allow valid HTTP(S) reg-names](https://github.com/dhall-lang/dhall-lang/pull/627)
  * [Treat "multi-lets" as syntactic sugar](https://github.com/dhall-lang/dhall-lang/pull/618)
  * [Clarify how semantic integrity checks work for `as Text`](https://github.com/dhall-lang/dhall-lang/pull/574)
  * [Normalize projection-by-expression via projection](https://github.com/dhall-lang/dhall-lang/pull/589)
  * [Fix mistake in `let` type-checking documentation](https://github.com/dhall-lang/dhall-lang/pull/596)
  * [Fix type inference rule for projection by type](https://github.com/dhall-lang/dhall-lang/pull/597)
  * [Tidy up record projection](https://github.com/dhall-lang/dhall-lang/pull/601)
  * [Beta normalization: Use `=` instead of `⇥` with the `keys` helper](https://github.com/dhall-lang/dhall-lang/pull/642)
  * [Binary encoding: Use 28 for empty lists with non-List annotations](https://github.com/dhall-lang/dhall-lang/pull/644)
  * [Fix grammar for empty list literals](https://github.com/dhall-lang/dhall-lang/pull/645)
  * [Left factor grammar for domains](https://github.com/dhall-lang/dhall-lang/pull/647)
  * [`dhall.abnf`: Replace (`ALPHA / DIGIT`) with `ALPHANUM`](https://github.com/dhall-lang/dhall-lang/pull/649)
  * [Fix first-application-expression rule](https://github.com/dhall-lang/dhall-lang/pull/652)
  * [Fix type-inference for `toMap`](https://github.com/dhall-lang/dhall-lang/pull/651)

* Fixes and improvements to standard test suite:

  * [Test for fetching imports from cache](https://github.com/dhall-lang/dhall-lang/pull/569)
  * [Add a bunch of binary decode tests](https://github.com/dhall-lang/dhall-lang/pull/582)
  * [Fix `parenthesizeUsing` parser test](https://github.com/dhall-lang/dhall-lang/pull/587)
  * [Fix projection by expression parser test and add more](https://github.com/dhall-lang/dhall-lang/pull/588)
  * [Commuting operators should not normalize commuted](https://github.com/dhall-lang/dhall-lang/pull/592)
  * [Fix normalization test for `Prelude/JSON/Type`](https://github.com/dhall-lang/dhall-lang/pull/599)
  * [Fix normalization tests containing unbound variables](https://github.com/dhall-lang/dhall-lang/pull/605)
  * [Fix `as Location` tests](https://github.com/dhall-lang/dhall-lang/pull/612)
  * [Add test to ensure that `constructors` doesn't typecheck anymore](https://github.com/dhall-lang/dhall-lang/pull/620)
  * [Add failure tests for deprecated `Optional` literal syntax](https://github.com/dhall-lang/dhall-lang/pull/629)
  * [Fix `toMap` normalization test](https://github.com/dhall-lang/dhall-lang/pull/641)
  * [Fix `ListLitEmptyPrecedence` parsing test](https://github.com/dhall-lang/dhall-lang/pull/654)
  * [`potPourriB.dhallb`: Fix encoding of query string](https://github.com/dhall-lang/dhall-lang/pull/655)
  * [Add CBOR diagnostic files for ease of code review](https://github.com/dhall-lang/dhall-lang/pull/659)

* Fixes and improvements to the Prelude:

  * [Map fix doc](https://github.com/dhall-lang/dhall-lang/pull/595)
  * [Relocate `./Prelude/Map.dhall` to `./Prelude/Map/Type.dhall](https://github.com/dhall-lang/dhall-lang/pull/608)
  * [Remove `./Prelude/Map.dhall`](https://github.com/dhall-lang/dhall-lang/pull/639)
  * [Add `Location` type to Prelude](https://github.com/dhall-lang/dhall-lang/pull/634)

## `v8.0.0`

Breaking changes:

* [Allow tabs and blank lines in multiline strings](https://github.com/dhall-lang/dhall-lang/pull/519)

  This changes two things about how multiline strings handle whitespace:

  * Blank lines are now ignored for the purpose of dedenting multiline strings

    Previously empty lines would still require the same number of leading spaces
    as other lines in order for multiline string literals to be properly
    dedented.

    For example, the following multi-line string is now dedented:

    ```haskell
    let example =
    ␠␠␠␠␠␠''
    ␠␠␠␠␠␠foo

    ␠␠␠␠␠␠bar
    ␠␠␠␠␠␠''

    in  …
    ```

    ... to this equivalent double-quoted string literal:

    ```haskell
    "foo\n\nbar\n"
    ```

  * Multiline literals with leading tabs will now also be dedented

    To be precise, multiline literals will now dedent any shared prefix
    consisting of tabs and spaces so long as each (non-blank) line begins with
    the same prefix, such as this code:

    ```haskell
    let example =
    ␉␠''
    ␉␠foo

    ␉␠bar
    ␉␠''

    in  …
    ```

    ... which also desugars to the same double-quoted string literal:

    ```haskell
    "foo\n\nbar\n"
    ```

  This is a breaking change because multi-line string literals with blank lines
  or leading tabs are now interpreted differently.  However, expressions that
  padded blank lines with leading whitespace are unaffected by this change.

* [Simplify bare interpolations](https://github.com/dhall-lang/dhall-lang/pull/515)

  String literals that do nothing but interpolate a single expression are now
  simplified to that expression.

  For example, this code:

  ```haskell
  λ(x : Text) → "${x}"
  ```

  ... now normalizes to this code:

  ```haskell
  λ(x : Text) → x
  ```

  This is technically a breaking change because semantic integrity checks will
  change for any expressions that can be simplified in this way.  However,
  functionally this has no change on the code's behavior as the simplified code
  is extensionally equal to the original code.

  There is also another related change within this same release:

  * [Simplify all eligible text literals](https://github.com/dhall-lang/dhall-lang/pull/529)

* [Encode integrity check as multihash](https://github.com/dhall-lang/dhall-lang/pull/549)

  This changes how imports with semantic integrity checks are serialized by
  updating them to follow the [multihash](https://github.com/multiformats/multihash)
  standard.

  This is a technically breaking change if you serialize uninterpreted
  expressions that contain imports, but this has no effect on semantic integrity
  checks, which are computed from fully-resolved expressions.

New features:

* [Record projection by expression](https://github.com/dhall-lang/dhall-lang/pull/499)

  You can now project out a subset of record fields by specifying the expected
  type.  For example, this expression:

  ```haskell
  let e = { a = 10, b = "Text" }

  let s = { a : Natural }

  in e.(s)
  ```

  ... normalizes to:

  ```haskell
  { a = 10 }
  ```

  In other words, the type can be used as a record selector if surrounded with
  parentheses.

* [Allow `Sort` as type annotation](https://github.com/dhall-lang/dhall-lang/pull/507)

  Before this change `Sort` was not permitted anywhere within an expression and
  could only appear as the inferred type of an expression.

  Now `Sort` can be used as a type annotation, such as:

  ```haskell
  Kind : Sort
  ```

  ... but is still forbidden elsewhere within expressions.

  This is not a breaking change: this only permits more expressions to
  type-check than before.

* [Standardize support for header forwarding and inline headers](https://github.com/dhall-lang/dhall-lang/pull/560)

  This makes two changes to the language:

  * You can now specify custom headers inline, like this:

    ```haskell
    https://httpbin.org/user-agent
      using [ { header = "User-Agent", value = "Dhall" } ]
      as Text
    ```

    ... instead of having to specify them in a separate import

  * Headers are now automatically forwarded to relative imports

    In other words, if you import
    `https://example.com/foo.dhall using someHeaders`
    and that in turn imports `./bar.dhall` then that will resolve to
    `https://example.com/bar.dhall using someHeaders`.  In other words, the
    same headers used to fetch `foo.dhall` will also be used to fetch
    `bar.dhall`.

    This is most commonly used resolve transitive imports for expressions hosted
    within a private repository that requires authenticated headers.

* [Allow self-describe-cbor when decoding](https://github.com/dhall-lang/dhall-lang/pull/526)

  This extends the binary decoding logic to permit (and ignore) CBOR tag 55799,
  as required by the CBOR RFC.

  This is not a breaking change: this only permits more CBOR expressions to be
  decoded than before.

Other changes:

* Fixes and improvements to the standard

  * [Clarify which judgement is introduced by each section of the semantics](https://github.com/dhall-lang/dhall-lang/pull/509)
  * [Use ASCII names for standard files](https://github.com/dhall-lang/dhall-lang/pull/510)
  * [Add missing commas](https://github.com/dhall-lang/dhall-lang/pull/513)
  * [Explain the encoding labelling scheme](https://github.com/dhall-lang/dhall-lang/pull/503)
  * [Clarify release process](https://github.com/dhall-lang/dhall-lang/pull/511)
  * [Update test `README` to match directory name](https://github.com/dhall-lang/dhall-lang/pull/516)
  * [Fix typo: β-normalization → α-normalization](https://github.com/dhall-lang/dhall-lang/pull/520)
  * [Fix IP parsing](https://github.com/dhall-lang/dhall-lang/pull/522)
  * [Add missing base cases for union type type inference](https://github.com/dhall-lang/dhall-lang/pull/530)
  * [Fix union constructor type inference judgments](https://github.com/dhall-lang/dhall-lang/pull/538)

* Fixes and improvements to the standard test suite

  * [Don't escape strings in CBOR encoding](https://github.com/dhall-lang/dhall-lang/pull/504)
  * [Split import tests up into unit tests](https://github.com/dhall-lang/dhall-lang/pull/517)
  * [A simple test case that found a bug in `dhall-ruby`](https://github.com/dhall-lang/dhall-lang/pull/518)
  * [Add regression test for alpha normalization](https://github.com/dhall-lang/dhall-lang/pull/521)
  * [Add some tests](https://github.com/dhall-lang/dhall-lang/pull/523)
  * [Fix multiline test](https://github.com/dhall-lang/dhall-lang/pull/527)
  * [Add unit test for type inference of empty alternative](https://github.com/dhall-lang/dhall-lang/pull/532)
  * [Isolate `FunctionNestedBindingXXFree`](https://github.com/dhall-lang/dhall-lang/pull/534)
  * [Fix self describe CBOR tests](https://github.com/dhall-lang/dhall-lang/pull/535)
  * [Don't use old optional syntax unless necessary](https://github.com/dhall-lang/dhall-lang/pull/539)
  * [Improve use of unions in tests](https://github.com/dhall-lang/dhall-lang/pull/540)
  * [More fixes to unit tests](https://github.com/dhall-lang/dhall-lang/pull/553)
  * [Add test for multiline strings with mixed line endings](https://github.com/dhall-lang/dhall-lang/pull/554)
  * [Upstream regression test](https://github.com/dhall-lang/dhall-lang/pull/559)
  * [Add test for hash mismatch](https://github.com/dhall-lang/dhall-lang/pull/561)

* Fixes and improvements to the Prelude

  * [Tidy up Prelude](https://github.com/dhall-lang/dhall-lang/pull/531)
  * [Prelude/JSON/Nesting: use empty union alternatives](https://github.com/dhall-lang/dhall-lang/pull/541)

## `v7.0.0`

Breaking changes:

*   [Protect transitive remote imports with CORS check](https://github.com/dhall-lang/dhall-lang/pull/411)

    This change protects against 
    [server-side request forgery](https://www.owasp.org/index.php/Server_Side_Request_Forgery)
    by preventing remote imports from importing transitive remote imports 
    that do not explicitly opt in via CORS.

    For example, a simple way to exploit an AWS EC2 instance before this change
    is to ask the instance to interpret `https://example.com/malicious`, where:

    * `https://example.com/malicious` imports `https://example.com/recordsHeaders using https://example.com/stealCredentials`
    * `https://example.com/stealCredentials` contains

      ```haskell
      [ { header = "Credentials"
        , value = http://169.254.169.254/latest/meta-data/iam/security-credentials/role as Text
        }
      ]
      ```

    This is a breaking change because now the import of `http://169.254.169.254`
    would be rejected, as the response would not include an `Access-Control-Allow-Origin` 
    header permitting itself to be transitively imported.

    Similarly, this change protects against an external internet import from
    triggering an interpreter request against a potentially sensitive intranet
    endpoint unless that intranet endpoint had enabled CORS whitelisting that
    external domain.

*   [Remove support for fragment identifiers](https://github.com/dhall-lang/dhall-lang/pull/406)

    Fragment identifiers are not useful for remote imports since:

    * They don't affect import resolution because:
        * They are not used to resolve the host
        * They are not transmitted to the host as part of the path to fetch
        * More generally, fragments are required by
          [RFC 3986](https://tools.ietf.org/html/rfc3986#section-3.5) to be
          interpreted client-side (if at all)
    * They don't identify any "sub-section" of a Dhall expression to fetch
    
    Therefore, we remove support for them in the grammar and the binary encoding.

    This is a breaking change to the binary encoding, although this does not
    affect semantic integrity checks because they are fully resolved and
    therefore don't include imports.

*   [Unescape unquoted URI path components](https://github.com/dhall-lang/dhall-lang/pull/489)

    With this change all unquoted URI paths will be unescaped on parsing, and
    all URI components will be escaped before importing.
    
    This changes the binary encoding, e.g. the following expressions used to encode
    to the same bytes, but now don't:
    * `https://example.com/a%20b/c`
    * `https://example.com/"a%20b"/c`

*   [Simplify text concatenation normalization](https://github.com/dhall-lang/dhall-lang/pull/497)

    From now on, the "text concatenation" operator is interpreted as two 
    interpolations together:

    ```
    "${l}${r}" ⇥ s₀
    ─────────────────────
    l ++ r ⇥ s₀
    ```

    This is a breaking change, as the following expression:
    
    ```hs
    λ( a : Text ) → λ( b : Text ) → a ++ b
    ```
    
    ..used to normalize to itself, while now it normalizes to:
    
    ```hs
    λ( a : Text ) → λ( b : Text ) → "${a}${b}"
    ```

New features:

*   [Add support for union alternatives without fields](https://github.com/dhall-lang/dhall-lang/pull/438)

    This adds support for unions with empty alternatives that don't store any
    values. In the simple case where the union has all empty alternatives it
    degenerates to an enum.
    
    For example this is now possible:
    
    ```hs
    let Role = < Wizard | Fighter | Rogue >

    let show : Role → Text
        show =
            λ(x : Role)
          → merge { Wizard = "Wizard", Fighter = "Fighter", Rogue = "Rogue" } x

    in  show Role.Wizard
    ```
    
    Note that the corresponding handlers don't need to take a function argument
    any longer; that is, handlers for empty alternatives no longer have to bind
    unused arguments of type `{}` and constructors for empty alternatives no 
    longer have to take an input of `{=}`.

*   [Expand character set for quoted labels](https://github.com/dhall-lang/dhall-lang/pull/408)

    This expands quoted labels to permit all non-control ASCII characters except
    backticks, so e.g. this is now allowed:
    
    ```hs
    { `<>.\!@#$%^&*()*` = 42 }
    ```

*   [Allow builtin names as fields](https://github.com/dhall-lang/dhall-lang/pull/437)

    Up until now it was not possible to use builtin names anywhere except when
    invoking builtins. This caused some common idioms to be uncomfortable to use,
    e.g. in order to use builtin names in record fields one needed to quote them:
    
    ```hs
    let Prelude = https://prelude.dhall-lang.org/package.dhall

    in  Prelude.`List`.map
    ```
    
    This change allows using builtin names for anything but bound variables, so
    this is now allowed:
    
    ```hs
    let Prelude = https://prelude.dhall-lang.org/package.dhall

    in  Prelude.List.map
    ```

*   [Fix typechecking of Sorts in records](https://github.com/dhall-lang/dhall-lang/pull/453)

    This fixes a limitation of the record typechecking, for which `Sort`s were forbidden
    in record types.
    
    So the following didn't use to typecheck but now do:
    
    * `{ a : Kind → Kind }` with type `Sort`
    * `{ a : { b : Kind } }` with type `Sort`
    * `{ a : { b : Kind → Kind } }` with type `Sort`
    * `{ a = { b = Type } }` with type `{ a : { b : Kind } }`

Other changes:

*   New implementations

    * [dhall-rust has been forked and is in progress](https://github.com/dhall-lang/dhall-lang/pull/468)
    * [Add a new complete implementation, dhall-ruby](https://github.com/dhall-lang/dhall-lang/pull/479)

*   Fixes and improvements to the grammar

    * [Remove special rules for builtins in the grammar](https://github.com/dhall-lang/dhall-lang/pull/399)
    * [Clarify how to decode variables named `_`](https://github.com/dhall-lang/dhall-lang/pull/407)
    * [Make grammar suitable for PEGs, take 2](https://github.com/dhall-lang/dhall-lang/pull/442)
    * [Imports have whitespace before the hash](https://github.com/dhall-lang/dhall-lang/pull/452)
    * [Improve grammar for auto-generated parsers](https://github.com/dhall-lang/dhall-lang/pull/455)
    * [Allow `merge x y z`](https://github.com/dhall-lang/dhall-lang/pull/467)
    * [Shuffle comment parsing rules so it's easier to patch to remove ambiguity](https://github.com/dhall-lang/dhall-lang/pull/482)
    * [Add 'builtin' rule to specify the parsing of builtins](https://github.com/dhall-lang/dhall-lang/pull/490)

*   Fixes and improvements to the semantics

    * [Fix decoding of imports](https://github.com/dhall-lang/dhall-lang/pull/405)
    * [Specify equality of expressions using the binary encoding](https://github.com/dhall-lang/dhall-lang/pull/426)
    * [`({ a = (t: T) } ⫽ { a = (n: N) }): { a: N }`](https://github.com/dhall-lang/dhall-lang/pull/432)
    * [Fixed typos and improved notation consistency in semantics](https://github.com/dhall-lang/dhall-lang/pull/449)
    * [Clarify when handler needs to be a function](https://github.com/dhall-lang/dhall-lang/pull/473)
    * [Fix minor typo](https://github.com/dhall-lang/dhall-lang/pull/477)
    * [Languages can decide how to marshal Dhall expressions](https://github.com/dhall-lang/dhall-lang/pull/474)
    * [Add other failures to the triggers of the `?` operator fallback](https://github.com/dhall-lang/dhall-lang/pull/481)
    * [Separate binary encoding of variables vs built-ins](https://github.com/dhall-lang/dhall-lang/pull/488)
    * [Simplify record typechecking rules](https://github.com/dhall-lang/dhall-lang/pull/495)

*   Fixes and improvements to tests

    * [Add normalization unit tests](https://github.com/dhall-lang/dhall-lang/pull/416)
    * [Update normalization tests](https://github.com/dhall-lang/dhall-lang/pull/415)
    * [Make normalization prelude tests import only the package they need](https://github.com/dhall-lang/dhall-lang/pull/420)
    * [Fix encoding of `largeExpression` parser test case](https://github.com/dhall-lang/dhall-lang/pull/431)
    * [Fix parsing test for large expression](https://github.com/dhall-lang/dhall-lang/pull/445)
    * [Fix the encoding of a test](https://github.com/dhall-lang/dhall-lang/pull/444)
    * [Some shuffling of parser tests](https://github.com/dhall-lang/dhall-lang/pull/443)
    * [Typecheck vs type inference tests](https://github.com/dhall-lang/dhall-lang/pull/447)
    * [Semantic hash tests](https://github.com/dhall-lang/dhall-lang/pull/450)
    * [Fix normalization/unit/EmptyAlternative test](https://github.com/dhall-lang/dhall-lang/pull/458)
    * [Improve documentation of tests/parser/failure/boundBuiltins.dhall](https://github.com/dhall-lang/dhall-lang/pull/459)
    * [Remove duplicate test](https://github.com/dhall-lang/dhall-lang/pull/460)
    * [Update largeExpression test to new Union syntax](https://github.com/dhall-lang/dhall-lang/pull/471)
    * [Test merging sort-level record types](https://github.com/dhall-lang/dhall-lang/pull/486)
    
* Fixes and improvements to infrastructure and docs

    * [Add `discourse.dhall-lang.org` configuration](https://github.com/dhall-lang/dhall-lang/pull/417)
    * [Clarify language changes approval in `CONTRIBUTING.md`](https://github.com/dhall-lang/dhall-lang/pull/427)
    * [Make self-caching Prelude](https://github.com/dhall-lang/dhall-lang/pull/424)
    * [Update `{prelude.,}.dhall-lang.org`](https://github.com/dhall-lang/dhall-lang/pull/429)
    * [Fix TOC](https://github.com/dhall-lang/dhall-lang/pull/485)
    * [Update websites](https://github.com/dhall-lang/dhall-lang/pull/491)
    * [Update hashes for Prelude functions that started using empty alternatives](https://github.com/dhall-lang/dhall-lang/pull/480)


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
    * [Small fixes to import semantics section](https://github.com/dhall-lang/dhall-lang/pull/289)

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
