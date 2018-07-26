# Binary semantics

This document formalizes the semantics for encoding and decoding Dhall
expressions to and from a binary representation

This is split into a separate document since this document will likely grow
as the protocol evolves in order to specify how to encode and decode multiple
versions of the binary protocol.

* [Motivation](#motivation)
* [CBOR](#cbor)
* [Versioning](#versioning)
* [CBOR expressions](#cbor-expressions)
* [Binary encoding judgment](#binary-encoding-judgment)
* [Variables](#variables)
* [Built-in constants](#built-in-constants)
* [Function application](#function-application)
* [Functions](#functions)
* [Operators](#operators)
* [`List`](#list)
* [`Optional`](#optional)
* [`merge`-expressions](#merge-expressions)
* [Records](#records)
* [Unions](#unions)
* [`Bool`](#bool)
* [`Natural`](#natural)
* [`Integer`](#integer)
* [`Double`](#double)
* [`Text`](#text)
* [Imports](#imports)
* [`let`-expressions](#let-expressions)
* [Type annotations](#type-annotations)
* [Versioning judgment](#versioning-judgment)

## Motivation

Dhall's import system requires a standard way to convert expressions to and from
a binary representation, for two reasons:

* Imported expressions can be protected by a "semantic integrity check", which
  is a hash of the binary representation of an expression's normal form

* Imported expressions are also locally cached if they are protected by a
  semantic integrity check and the local cache stores expressions using their
  binary representation

## CBOR

Dhall's semantics for binary serialization are defined in terms of CBOR
([RFC 7049](https://tools.ietf.org/html/rfc7049)).  This means that this
section first specifies a simplified grammar for the subset of CBOR that Dhall
uses (i.e. "CBOR expressions") and then specifies how to convert Dhall
expressions to and from these CBOR expressions.

The `encode` judgments in this section specify how to convert a Dhall
expression to a CBOR expression.  Once you have a CBOR expression you can
serialize that CBOR expression to binary according to RFC 7049.

The `decode` judgments in this section specify how to convert a CBOR
expression to a Dhall expression.  That implies that if you can deserialize
binary to a CBOR expression then you can further decode that CBOR expression to
a Dhall expression.

For efficiency reasons, an implementation can elect to not go through an
intermediate CBOR expression and instead serialize to or deserialize from the
binary representation directly.  These semantics only specify how to perform the
conversion through an intermediate CBOR expression for simplicity.

## Versioning

All serialized Dhall expressions are encoded alongside a version string.  This
version string is only encoded once as a header for the entire Dhall expression,
not for each subexpression.  This serialized version string is also not
necessarily the same as the version string for the standard and this section
will use the term "binary protocol version string" to refer to the version
string used to tag binary-encoded Dhall expressions.

This section provides two additional judgments to handle versioning Dhall
expressions:

* The `encodeWithVersion` judgment specifies how to encode a Dhall expression
  tagged with a version number
* The `decodeWithVersion` judgment specifies how to decode a Dhall expression
  tagged with a version number

For any given release of the standard the `encodeWithVersion` judgment emits
exactly one version of the binary protocol.  The binary protocol version
string is hard-coded directly into the `encodeWithVersion` judgment and is
only updated via a new release of the standard (typically when the binary
protocol changes).

The corresponding `decodeWithVersion` judgment may decode more than one
version of the protocol.  The standard guarantees that the `decodeWithVersion`
judgment will decode at least the same version string that the `encodeWith
Version` judgment emits but may also decode other version strings for
backwards compatibility.

For example, the `encodeWithVersion` judgment might specify a binary protocol
version string of "1.1" while the `decodeWithVersion` judgment might specify how
to decode Dhall expressions tagged with either the "1.0" or "1.1" binary
protocol version strings.

The version strings will typically be version numbers of the form "X.Y", where:

* Changing the version from "X.Y" to "X.{Y + 1}" indicates a non-breaking change
* Changing the version from "X.Y" to "{X + 1}.0" indicates a breaking change

"X" is "major version number" and "Y" is the "minor version number".

This versioning number convention is purely a fallible convention and is not
necessarily enforced by the standardized semantics.  Whenever the version
numbering convention conflicts with either the `encodeWithVersion` or the
`decodeWithVersion` judgment the judgment is authoritative.

## Protocol evolution

The standardization process is fallible and this section addresses how to
mitigate the following types of protocol specification errors:

*   A new binary protocol version string misassigns the major or minor number

    The major/minor numbering convention is purely a mnemonic convention for
    humans to use when keeping track of versions.  This convention is not
    necessarily enforced by the standardized judgments, given that the process
    of assigning version numbers is fallible.

    For example, a `decodeWithVersion` judgment that successfully decodes an
    expression tagged with a binary protocol version string of "1.1" does not
    necessarily decode expressions tagged with a binary protocol version
    string of "1.0" unless explicitly standardized to do so.  This implies that
    any backwards compatibility support has to be explicitly implemented in the
    judgment by enumerating all the binary protocol version strings that
    judgment is compatible with.

    The `decodeWithJudgment` (and any standards-compliant implementation) must
    only match on exact binary protocol version strings and must not match a
    prefix of the version string, nor a version range, nor parse the string in
    any way.

    Implementations are required to do exact matching so that the binary
    protocol version string is not constrained to be a version number.  For
    example, in the future the version number could hypothetically become a
    SHA-256 hash or a URI.  Such a change in the version string format would
    cause no issues if implementations stick to exact string matching and don't
    attempt to parse the string.

    This also implies that the set of supported versions to decode need not be
    contiguous.  For example, the `decodeWithVersion` judgment might specify how
    to decode the protocol version "1.0" or "1.2" but not "1.1".  Similarly, the
    same judgment might specify how to decode more than one major protocol
    version (i.e. protocol version "1.4" or "2.0).

*   There is a specification bug in the encoding logic

    An protocol version that incorrectly specifies how to encode expressions
    must be blacklisted and the fixed encoding logic must be released under
    a new protocol version.  This is necessary since there is no way to fix all
    possible encoded data once the encoding bug has been published.

*   New encoding logic is published without changing the version number

    Treat this the same as a specification bug in the encoding logic.  Blacklist
    the old binary protocol version string and release the same logic under a
    new binary protocol version string.

*   There is a specification bug in the decoding logic

    Decoding bugs can be fixed by releasing a new verson of the standard
    fixing the error.  The binary protocol version does not need to be
    blacklisted nor changed since decoding logic does not persist any data.

*   The decoding logic is changed without changing the version number at all

    Treat this the same as a specification bug in the decoding logic.  Publish
    a new release of the standard fixing the version number that the logic
    decodes, but there is no need to blacklist the old binary protocol version
    since no invalid data was persisted.

## CBOR expressions

The following notation will be used for CBOR expressions in serialization
judgments:

```
e =   n              ; Unsigned integer    (Section 2.1, Major type = 0)
  /  -n              ; Negative integer    (Section 2.1, Major type = 1)
  /  "…"             ; Text                (Section 2.1, Major type = 3)
  /  [ e, es… ]      ; Heterogeneous array (Section 2.1, Major type = 4)
  /  { e = e, es… }  ; Heterogeneous map   (Section 2.1, Major type = 5)
  /  False           ; False               (Section 2.3, Value = 20)
  /  True            ; True                (Section 2.3, Value = 21)
  /  null            ; Null                (Section 2.3, Value = 22)
  /  nn              ; Unsigned bignum     (Section 2.4, Tag = 2)
  / -nn              ; Negative bignum     (Section 2.4, Tag = 3)
  /  n.n             ; Decimal fraction    (Section 2.4, Tag = 4)
```

## Binary encoding judgment

Binary serialization of a naked Dhall expression is a function of the following
form:

    encode(dhall) = cbor

... where:

* `dhall` (the input) is a Dhall expression
* `cbor` (the output) is a CBOR expression

The encoding judgment is specified in such a way that given the encoding you
can uniquely determine the decoding rules.

For example, most Dhall expressions are encoded as a heterogeneous array where
the first element is an integer tag and the remaining elements are the
expression contents.  However, variables are one exception and are encoded as 
either a naked integer, naked text string or a heterogeneous array beginning
with a text string.  No other Dhall expression overlaps with either of these
encodings, so a decoder can safely decode them to a variable wherever the
decoder expects to produce a Dhall expression.

Also, the encoding uses tags less than 24 for language features that can
survive `β`-normalization (such as a function type) since those tags fit in one
byte.  Language features that don't survive normalization (such as a `let`
expression) use tags of 24 or above.

## Variables

The binary representation encodes variables as compactly as possible, because:

* They occur frequently in Dhall expressions
* Built-in constants are encoded the same as free variables

A variable named `"_"` is encoded as its index, using the smallest numeric
representation available:


    ───────────────  ; n < 2^64
    encode(_@n) = n


    ────────────────  ; 2^64 <= n
    encode(_@n) = nn


The reason for this optimization is because expressions are commonly
α-normalized before encoding them, such as when computing their semantic
integrity check and also when caching them.

A variable that is not named `"_"` with index `0` is encoded as a naked CBOR
text string matching the variable's identifier:


    ─────────────────  ; x ≠ "_"
    encode(x@0) = "x"


Otherwise a variable is stored as a two-element CBOR list where the first
element is the identifier and the second element is the encoded index (using
the smallest numeric representation available):


    ────────────────────────  ; n < 2^64
    encode(x@n) = [ "x", n ]


    ─────────────────────────  ; 2^64 <= nn
    encode(x@n) = [ "x", nn ]


## Built-in constants

All built-in constants (except boolean values) are encoded as a variable of the
same name with an index of 0, which is equivalent to encoding them as a naked
string matching their identifier.


    ───────────────────────────────────────
    encode(Natural/build) = "Natural/build"


    ─────────────────────────────────────
    encode(Natural/fold) = "Natural/fold"


    ─────────────────────────────────────────
    encode(Natural/isZero) = "Natural/isZero"


    ─────────────────────────────────────
    encode(Natural/even) = "Natural/even"


    ───────────────────────────────────
    encode(Natural/odd) = "Natural/odd"


    ───────────────────────────────────────────────
    encode(Natural/toInteger) = "Natural/toInteger"


    ─────────────────────────────────────
    encode(Natural/show) = "Natural/show"


    ─────────────────────────────────────────────
    encode(Integer/toDouble) = "Integer/toDouble"


    ─────────────────────────────────────
    encode(Integer/show) = "Integer/show"


    ───────────────────────────────────
    encode(Double/show) = "Double/show"


    ─────────────────────────────────
    encode(List/build) = "List/build"


    ───────────────────────────────
    encode(List/fold) = "List/fold"


    ───────────────────────────────────
    encode(List/length) = "List/length"


    ───────────────────────────────
    encode(List/head) = "List/head"


    ───────────────────────────────
    encode(List/last) = "List/last"


    ─────────────────────────────────────
    encode(List/indexed) = "List/indexed"


    ─────────────────────────────────────
    encode(List/reverse) = "List/reverse"


    ───────────────────────────────────────
    encode(Optional/fold) = "Optional/fold"


    ─────────────────────────────────────────
    encode(Optional/build) = "Optional/build"


    ─────────────────────
    encode(Bool) = "Bool"


    ─────────────────────────────
    encode(Optional) = "Optional"


    ───────────────────────────
    encode(Natural) = "Natural"


    ───────────────────────────
    encode(Integer) = "Integer"


    ─────────────────────────
    encode(Double) = "Double"


    ─────────────────────
    encode(Text) = "Text"


    ─────────────────────
    encode(List) = "List"


    ─────────────────────
    encode(Type) = "Type"


    ───────────────────────
    encode(Kind) = "Kind"


## Function application

Function application is encoded as a heterogeneous array where a function
applied to multiple arguments is stored within a single array:


    encode(f₀) = f₁   encode(a₀) = a₁   encode(b₀) = b₁   …
    ───────────────────────────────────────────────────────
    encode(f₀ a₀ b₀ …) = [ 0, f₁, a₁, b₁, … ]


## Functions

Functions that bind variables named `_` have a more compact representation:


    encode(A₀) = A₁   encode(b₀) = b₁
    ──────────────────────────────────────
    encode(λ(_ : A₀) → b₀) = [ 1, A₁, b₁ ]


... than functions that bind variables of other names:


    encode(A₀) = A₁   encode(b₀) = b₁
    ───────────────────────────────────────────  ; x ≠ "_"
    encode(λ(x : A₀) → b₀) = [ 1, "x", A₁, b₁ ]


Function types that bind variables named `_` also have a more compact
representation:


    encode(A₀) = A₁   encode(B₀) = B₁
    ──────────────────────────────────────
    encode(∀(_ : A₀) → B₀) = [ 2, A₁, B₁ ]


... than function types that bind variables of other names:


    encode(A₀) = A₁   encode(B₀) = B₁
    ───────────────────────────────────────────  ; x ≠ "_"
    encode(∀(x : A₀) → B₀) = [ 2, "x", A₁, B₁ ]


## Operators

Operators are encoded as tagged integers alongside their two arguments:


    encode(l₀) = l₁   encode(r₀) = r₁
    ──────────────────────────────────────
    encode(l₀ || r₀) = [ 3, 0, l₁, r₁ ]


    encode(l₀) = l₁   encode(r₀) = r₁
    ──────────────────────────────────────
    encode(l₀ && r₀) = [ 3, 1, l₁, r₁ ]


    encode(l₀) = l₁   encode(r₀) = r₁
    ──────────────────────────────────────
    encode(l₀ == r₀) = [ 3, 2, l₁, r₁ ]


    encode(l₀) = l₁   encode(r₀) = r₁
    ──────────────────────────────────────
    encode(l₀ != r₀) = [ 3, 3, l₁, r₁ ]


    encode(l₀) = l₁   encode(r₀) = r₁
    ────────────────────────────────────
    encode(l₀ + r₀) = [ 3, 4, l₁, r₁ ]


    encode(l₀) = l₁   encode(r₀) = r₁
    ────────────────────────────────────
    encode(l₀ * r₀) = [ 3, 5, l₁, r₁ ]


    encode(l₀) = l₁   encode(r₀) = r₁
    ──────────────────────────────────────
    encode(l₀ ++ r₀) = [ 3, 6, l₁, r₁ ]


    encode(l₀) = l₁   encode(r₀) = r₁
    ────────────────────────────────────
    encode(l₀ # r₀) = [ 3, 7, l₁, r₁ ]


    encode(l₀) = l₁   encode(r₀) = r₁
    ─────────────────────────────────────
    encode(l₀ ∧ r₀) = [ 3, 8, l₁, r₁ ]


    encode(l₀) = l₁   encode(r₀) = r₁
    ─────────────────────────────────────
    encode(l₀ ⫽ r₀) = [ 3, 9, l₁, r₁ ]


    encode(l₀) = l₁   encode(r₀) = r₁
    ───────────────────────────────────────
    encode(l₀ ⩓ r₀) = [ 3, 10, l₁, r₁ ]


    encode(l₀) = l₁   encode(r₀) = r₁
    ──────────────────────────────────────
    encode(l₀ ? r₀) = [ 3, 11, l₁, r₁ ]


## `List`

Empty `List`s only store their type:


    encode(T₀) = T₁
    ────────────────────────────────
    encode([] : List T₀) = [ 4, T₁ ]


Non-empty `List`s don't store their type, but do store their elements inline:


    encode(a₀) = a₁   encode(b₀) = b₁
    ──────────────────────────────────────────────
    encode([ a₀, b₀, … ]) = [ 4, null, a₁, b₁, … ]


## `Optional`

Empty `Optional` literals only store their type:


    encode(T₀) = T₁
    ────────────────────────────────────
    encode([] : Optional T₀) = [ 5, T₁ ]


Non-empty `Optional` literals also store their value:


    encode(t₀) = t₁   encode(T₀) = T₁
    ────────────────────────────────────────────
    encode([ t₀ ] : Optional T₀) = [ 5, T₁, t₁ ]


## `merge` expressions

`merge` expressions differ in their encoding depending on whether or not they
have a type annotation:


    encode(t₀) = t₁   encode(u₀) = u₁
    ───────────────────────────────────
    encode(merge t₀ u₀) = [ 6, t₁, u₁ ]


    encode(t₀) = t₁   encode(u₀) = u₁   encode(T₀) = T₁
    ───────────────────────────────────────────────────
    encode(merge t₀ u₀ : T₀) = [ 6, t₁, u₁, T₁ ]


## Records

Dhall record types translate to CBOR maps:


    encode(T₀) = T₁
    ──────────────────────────────────────────────
    encode({ x : T₀, … }) = [ 7, { "x" = T₁, … } ]


Dhall record literals translate to CBOR maps:


    encode(t₀) = t₁
    ──────────────────────────────────────────────
    encode({ x = t₀, … }) = [ 8, { "x" = t₁, … } ]


Field access:


    encode(t₀) = t₁
    ─────────────────────────────
    encode(t₀.x) = [ 9, t₁, "x" ]


... is encoded differently than record projection:


    encode(t₀) = t₁
    ────────────────────────────────────────────────
    encode(t₀.{ x, y, … }) = [ 10, t₁, "x", "y", … ]


## Unions

Dhall union types translate to CBOR maps:


    encode(T₀) = T₁
    ────────────────────────────────────────────────
    encode(< x : T₀ | … >) = [ 11, { "x" = T₁, … } ]


Dhall union literals store the specified alternative followed by the alternative
types encoded as CBOR map:


    encode(t₀) = t₁   encode(T₀) = T₁   …
    ───────────────────────────────────────────────────────────────────
    encode(< x = t₀ | y : T₀ | … >) = [ 12, "x", t₁, { "y" = T₁, … } ]


The `constructors` keyword is encoded as:


    encode(u₀) = u₁
    ──────────────────────────────────────────────────
    encode(constructors u₀) = [ 13, u₁]


## `Bool`

Boolean literals are encoded using CBOR's built-in support for boolean values:


    ───────────────────
    encode(True) = True


    ─────────────────────
    encode(False) = False


`if` expressions are encoded with a tag:


    encode(t₀) = t₁   encode(l₀) = l₁   encode(r₀) = r₁
    ───────────────────────────────────────────────────────
    encode(if t₀ then l₀ else r₀) = [ 14, t₁, l₁, r₁ ]


## `Natural`

`Natural` literals are encoded using the smallest available numeric
representation:


    ─────────────────────  ; n < 2^64
    encode(n) = [ 15, n ]


    ──────────────────────  ; 2^64 <= nn
    encode(n) = [ 15, nn ]


## `Integer`

`Integer` literals are encoded using the smallest available numeric
representation:


    ────────────────────────  ; ±n < -2^64
    encode(±n) = [ 16, -nn ]


    ───────────────────────  ; -2^64 <= ±n < 0
    encode(±n) = [ 16, -n ]


    ──────────────────────  ; 0 <= ±n < 2^64
    encode(±n) = [ 16, n ]


    ───────────────────────  ; 2^64 <= ±n
    encode(±n) = [ 16, nn ]


## `Double`

`Double`s are always encoded as CBOR decimal fractions in order to avoid loss
of precision when converting to and from their textual representation:


    ─────────────────────────
    encode(n.n) = [ 17, n.n ]


## `Text`

`Text` literals are encoded as an alternation between their text chunks and
any interpolated expressions:


    encode(b₀) = b₁   encode(d₀) = d₁   …   encode(y₀) = y₁
    ───────────────────────────────────────────────────────────────────────────────
    encode("a${b₀}c${d}e…x${y₀}z") = [ 18, "a", b₁, "c", d₁, "e", …, "x", y₁, "z" ]

Note that this encoding of a text literal always begins and ends with a string,
even if the first or last chunk is the empty string.

## Imports

URL imports are encoded in a tokenized form with the following elements in
order:

* The first element is 0 (if the scheme is http) or 1 (if the scheme is https)
* The next element is the authority
    * This includes user information and port if present
    * This does not include the preceding "//" or the following "/"
    * For example, the authority of `http://user@host:port/foo` is encoded as
      `"user@host:port"`
* Then comes one element per path component
    * The encoded path components do not include their separating slashes
    * For example, `/foo/bar/baz` is stored as `…, "foo", "bar", "baz", …`
* Then comes the file component
    * Also no slashes
* Then comes one element for the query component
    * If there is no query component then it is encoded as `null`
    * If there is a query component then it is stored without the `?`
    * A query component with internal `&` separators is still one element
    * For example `?foo=1&bar=true` is stored as `"foo=1&bar=true"`
* Then comes one element for the fragment component
    * If there is no fragment component then it is encoded as `null`
    * If there is a fragment component then it is stored without the `#`
    * For example, `#bar` is stored as `"bar"`

The full rules are:


    ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    encode(http://authority/path₀/path₁/…/file?query#fragment) = [ 24, 0, "authority" "path₀", "path₁", …, "file", "query", "fragment" ]


    ────────────────────────────────────────────────────────────────────────────────────────────────────────────
    encode(http://authority/path₀/path₁/…/file) = [ 24, 0, "authority" "path₀", "path₁", …, "file", null, null ]



    ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    encode(https://authority/path₀/path₁/…/file?query#fragment) = [ 24, 1, "authority" "path₀", "path₁", …, "file", "query", "fragment" ]


    ─────────────────────────────────────────────────────────────────────────────────────────────────────────────
    encode(https://authority/path₀/path₁/…/file) = [ 24, 1, "authority" "path₀", "path₁", …, "file", null, null ]


Absolute file paths are tokenized in the same way:


    ────────────────────────────────────────────────────────────────────
    encode(/path₀/path₁/…/file) = [ 24, 2, "path₀", "path₁", …, "file" ]


Each path type is treated as another "scheme" (i.e. they are distinguished by
the second tag):


    ─────────────────────────────────────────────────────────────────────
    encode(./path₀/path₁/…/file) = [ 24, 3, "path₀", "path₁", …, "file" ]


    ──────────────────────────────────────────────────────────────────────
    encode(../path₀/path₁/…/file) = [ 24, 4, "path₀", "path₁", …, "file" ]


    ─────────────────────────────────────────────────────────────────────
    encode(~/path₀/path₁/…/file) = [ 24, 5, "path₀", "path₁", …, "file" ]


Environment variables are also treated as another scheme:


    ──────────────────────────────
    encode(env:x) = [ 24, 6, "x" ]


The `missing` keyword is also treated as another import type:


    ───────────────────────────
    encode(missing) = [ 24, 7 ]


## `let` expressions

`let` expressions differ in their encoding depending on whether or not they have
a type annotation:


    encode(a₀) = a₁   encode(b₀) = b₁
    ─────────────────────────────────────────────
    encode(let x = a₀ in b₀) = [ 25, "x", a₁, b₁ ]


    encode(a₀) = a₁   encode(A₀) = A₁   encode(b₀) = b₁
    ──────────────────────────────────────────────────────
    encode(let x : A₀ = a₀ in b₀) = [ 25, "x", A₁, a₁, b₁ ]


## Type annotations


    encode(t₀) = t₁   encode(T₀) = T₁
    ─────────────────────────────────
    encode(t₀ : T₀) = [ 26, t₁, T₁ ]


## Versioning judgment

Binary serialization of a Dhall expression encoded with a version tag is a
function of the following form:

    encodeWithVersion(dhall) = cbor

... where:

* `dhall` (the input) is a Dhall expression
* `cbor` (the output) is a CBOR expression

The rule is simple:


    encode(e₀) = e₁
    ───────────────────────────────────────
    encodeWithVersion(e₀) = [ "X.Y.Z", e₁ ]


... replacing `X.Y.Z` with the version number of the standard that the encoder
is based on.

The corresponding decoder should compare the expected version number of the
standard that the decoder is based on with the actual decoded number.  A
decoder is only required to decode an expression where the actual version
exactly matches the expected version.  A decoder may support decoding other
versions of the standard.  In particular, a decoder should support actual
versions that only differ in the last version component (i.e.  `Z`) since that
indicates a version of the standard that should be binary-compatible.
