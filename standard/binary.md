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
will use the term "protocol version string" to refer to the version string used
to tag binary-encoded Dhall expressions.

This document specifies how to encode and decode specific versions of the
protocol.  The latest protocol version string is:

    "1.0"

A compliant implementation MUST be able to encode and decode the latest
version of the protocol (i.e. an expression tagged with the above binary
protocol version string).

A compliant implementation MAY support encoding or decoding other versions of
the protocol standardized within this document for backwards compatibility.

The version strings will typically be version numbers of the form "X.Y", where:

* Changing the version from "X.Y" to "X.{Y + 1}" indicates a non-breaking change
* Changing the version from "X.Y" to "{X + 1}.0" indicates a breaking change

"X" is "major version number" and "Y" is the "minor version number".

This version number convention is purely a fallible convention and is not
necessarily enforced by the standardized semantics.  Whenever the version
numbering convention is inconsistent with the standardized encoding/decoding
judgments, the judgments are authoritative.

For example, a judgment that decodes expressions tagged with a protocol version
string of "X.{Y + 1}" will not necessarily decode expressions tagged with a
protocol version string of "X.Y".  In theory the decoder should be backwards
compatible with expressions tagged with smaller minor numbers, but in practice
it might not due to standardization errors.

Backwards compatibility is not assumed and is always made explicit by the
standardized judgments.  For example, if the decoder for protocol version string
`X.Z` is backward compatible with protocol version string `X.Y` then you will
see two judgments like this:


        decode-X.Z(e₀) = e₁
        ───────────────────────────────────────
        decodeWithVersion-X.Y([ "X.Y", e₀) = e₁

        decode-X.Z(e₀) = e₁
        ───────────────────────────────────────
        decodeWithVersion-X.Z([ "X.Z", e₀) = e₁


Notice that each judgment expects a different protocol version string but both
judgments reuse the same decoding logic (for protocol version "X.Z" in this
case) in order to avoid duplicate logic.

If you do not see backwards compatibility standardized in this way then you
should not assume backwards compatibility between two protocol versions, even if
they share the same major version number.

The protocol version string format might change in the future.  For example,
at some point the protocol version string might become a URI or a SHA256 hash.
Therefore, implementations MUST only match on the exact protocol version strings
enumerated in the decoding judgments.  Implementations MUST NOT match a prefix
of the version string and MUST NOT attempt to parse the protocol version string
as a version number.

## Protocol evolution

The standardization process is fallible and this section addresses how to
mitigate the following types of protocol specification errors:

*   A new protocol version string misassigns the major or minor number

    In theory, no action is required.  Standards compliant decoders are
    unaffected if they match on the exact protocol version string and do not
    attempt to interpret the protocol version string.  The only harm is that the
    incorrect protocol version number is misleading to humans by incorrectly
    suggesting the presence or absence of a breaking change.

    In practice, you might want to publish a new release of the standard
    correcting the version number and permanently removing standardized support
    for encoding or decoding the incorrect version number (i.e. "blacklisting"
    the number).

*   There is a specification bug in the encoding logic

    An protocol version that incorrectly specifies how to encode expressions
    MUST be blacklisted and the fixed encoding logic MUST be released under
    a new protocol version.  This is necessary since there is no way to fix all
    possible encoded data once the encoding bug has been published.

*   New encoding logic is published without changing the version number

    Treat this the same as a specification bug in the encoding logic.  Blacklist
    the old protocol version string and release the same logic under a new
    protocol version string.

*   There is a specification bug in the decoding logic

    Decoding bugs MUST be fixed by releasing a new verson of the standard
    fixing the error.  However, the corresponding protocol version string does
    not need to be blacklisted nor changed since decoding logic does not
    persist any data.

*   The decoding logic is changed without changing the version number at all

    Treat this the same as a specification bug in the decoding logic.  Publish
    a new release of the standard fixing the version number that the logic
    decodes, but there is no need to blacklist the old protocol version string
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

    encode-*(dhall) = cbor

... where:

* `dhall` (the input) is a Dhall expression
* `cbor` (the output) is a CBOR expression

... and replacing `*` with the protocol version string of the expression to
encode.

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


    ───────────────────  ; n < 2^64
    encode-1.0(_@n) = n


    ────────────────────  ; 2^64 <= n
    encode-1.0(_@n) = nn


The reason for this optimization is because expressions are commonly
α-normalized before encoding them, such as when computing their semantic
integrity check and also when caching them.

A variable that is not named `"_"` with index `0` is encoded as a naked CBOR
text string matching the variable's identifier:


    ─────────────────────  ; x ≠ "_"
    encode-1.0(x@0) = "x"


Otherwise a variable is stored as a two-element CBOR list where the first
element is the identifier and the second element is the encoded index (using
the smallest numeric representation available):


    ────────────────────────────  ; n < 2^64
    encode-1.0(x@n) = [ "x", n ]


    ─────────────────────────────  ; 2^64 <= nn
    encode-1.0(x@n) = [ "x", nn ]


## Built-in constants

All built-in constants (except boolean values) are encoded as a variable of the
same name with an index of 0, which is equivalent to encoding them as a naked
string matching their identifier.


    ───────────────────────────────────────────
    encode-1.0(Natural/build) = "Natural/build"


    ─────────────────────────────────────────
    encode-1.0(Natural/fold) = "Natural/fold"


    ─────────────────────────────────────────────
    encode-1.0(Natural/isZero) = "Natural/isZero"


    ─────────────────────────────────────────
    encode-1.0(Natural/even) = "Natural/even"


    ───────────────────────────────────────
    encode-1.0(Natural/odd) = "Natural/odd"


    ───────────────────────────────────────────────────
    encode-1.0(Natural/toInteger) = "Natural/toInteger"


    ─────────────────────────────────────────
    encode-1.0(Natural/show) = "Natural/show"


    ─────────────────────────────────────────────────
    encode-1.0(Integer/toDouble) = "Integer/toDouble"


    ─────────────────────────────────────────
    encode-1.0(Integer/show) = "Integer/show"


    ───────────────────────────────────────
    encode-1.0(Double/show) = "Double/show"


    ─────────────────────────────────────
    encode-1.0(List/build) = "List/build"


    ───────────────────────────────────
    encode-1.0(List/fold) = "List/fold"


    ───────────────────────────────────────
    encode-1.0(List/length) = "List/length"


    ───────────────────────────────────
    encode-1.0(List/head) = "List/head"


    ───────────────────────────────────
    encode-1.0(List/last) = "List/last"


    ─────────────────────────────────────────
    encode-1.0(List/indexed) = "List/indexed"


    ─────────────────────────────────────────
    encode-1.0(List/reverse) = "List/reverse"


    ───────────────────────────────────────────
    encode-1.0(Optional/fold) = "Optional/fold"


    ─────────────────────────────────────────────
    encode-1.0(Optional/build) = "Optional/build"


    ─────────────────────────
    encode-1.0(Bool) = "Bool"


    ─────────────────────────────────
    encode-1.0(Optional) = "Optional"


    ───────────────────────────────
    encode-1.0(Natural) = "Natural"


    ───────────────────────────────
    encode-1.0(Integer) = "Integer"


    ─────────────────────────────
    encode-1.0(Double) = "Double"


    ─────────────────────────
    encode-1.0(Text) = "Text"


    ─────────────────────────
    encode-1.0(List) = "List"


    ─────────────────────────
    encode-1.0(Type) = "Type"


    ─────────────────────────
    encode-1.0(Kind) = "Kind"


## Function application

Function application is encoded as a heterogeneous array where a function
applied to multiple arguments is stored within a single array:


    encode-1.0(f₀) = f₁   encode-1.0(a₀) = a₁   encode-1.0(b₀) = b₁   …
    ───────────────────────────────────────────────────────────────────
    encode-1.0(f₀ a₀ b₀ …) = [ 0, f₁, a₁, b₁, … ]


## Functions

Functions that bind variables named `_` have a more compact representation:


    encode-1.0(A₀) = A₁   encode-1.0(b₀) = b₁
    ──────────────────────────────────────────
    encode-1.0(λ(_ : A₀) → b₀) = [ 1, A₁, b₁ ]


... than functions that bind variables of other names:


    encode-1.0(A₀) = A₁   encode-1.0(b₀) = b₁
    ───────────────────────────────────────────────  ; x ≠ "_"
    encode-1.0(λ(x : A₀) → b₀) = [ 1, "x", A₁, b₁ ]


Function types that bind variables named `_` also have a more compact
representation:


    encode-1.0(A₀) = A₁   encode-1.0(B₀) = B₁
    ──────────────────────────────────────────
    encode-1.0(∀(_ : A₀) → B₀) = [ 2, A₁, B₁ ]


... than function types that bind variables of other names:


    encode-1.0(A₀) = A₁   encode-1.0(B₀) = B₁
    ───────────────────────────────────────────────  ; x ≠ "_"
    encode-1.0(∀(x : A₀) → B₀) = [ 2, "x", A₁, B₁ ]


## Operators

Operators are encoded as tagged integers alongside their two arguments:


    encode-1.0(l₀) = l₁   encode-1.0(r₀) = r₁
    ─────────────────────────────────────────
    encode-1.0(l₀ || r₀) = [ 3, 0, l₁, r₁ ]


    encode-1.0(l₀) = l₁   encode-1.0(r₀) = r₁
    ─────────────────────────────────────────
    encode-1.0(l₀ && r₀) = [ 3, 1, l₁, r₁ ]


    encode-1.0(l₀) = l₁   encode-1.0(r₀) = r₁
    ─────────────────────────────────────────
    encode-1.0(l₀ == r₀) = [ 3, 2, l₁, r₁ ]


    encode-1.0(l₀) = l₁   encode-1.0(r₀) = r₁
    ─────────────────────────────────────────
    encode-1.0(l₀ != r₀) = [ 3, 3, l₁, r₁ ]


    encode-1.0(l₀) = l₁   encode-1.0(r₀) = r₁
    ─────────────────────────────────────────
    encode-1.0(l₀ + r₀) = [ 3, 4, l₁, r₁ ]


    encode-1.0(l₀) = l₁   encode-1.0(r₀) = r₁
    ─────────────────────────────────────────
    encode-1.0(l₀ * r₀) = [ 3, 5, l₁, r₁ ]


    encode-1.0(l₀) = l₁   encode-1.0(r₀) = r₁
    ─────────────────────────────────────────
    encode-1.0(l₀ ++ r₀) = [ 3, 6, l₁, r₁ ]


    encode-1.0(l₀) = l₁   encode-1.0(r₀) = r₁
    ─────────────────────────────────────────
    encode-1.0(l₀ # r₀) = [ 3, 7, l₁, r₁ ]


    encode-1.0(l₀) = l₁   encode-1.0(r₀) = r₁
    ─────────────────────────────────────────
    encode-1.0(l₀ ∧ r₀) = [ 3, 8, l₁, r₁ ]


    encode-1.0(l₀) = l₁   encode-1.0(r₀) = r₁
    ─────────────────────────────────────────
    encode-1.0(l₀ ⫽ r₀) = [ 3, 9, l₁, r₁ ]


    encode-1.0(l₀) = l₁   encode-1.0(r₀) = r₁
    ─────────────────────────────────────────
    encode-1.0(l₀ ⩓ r₀) = [ 3, 10, l₁, r₁ ]


    encode-1.0(l₀) = l₁   encode-1.0(r₀) = r₁
    ─────────────────────────────────────────
    encode-1.0(l₀ ? r₀) = [ 3, 11, l₁, r₁ ]


## `List`

Empty `List`s only store their type:


    encode-1.0(T₀) = T₁
    ────────────────────────────────────
    encode-1.0([] : List T₀) = [ 4, T₁ ]


Non-empty `List`s don't store their type, but do store their elements inline:


    encode-1.0(a₀) = a₁   encode-1.0(b₀) = b₁
    ──────────────────────────────────────────────────
    encode-1.0([ a₀, b₀, … ]) = [ 4, null, a₁, b₁, … ]


## `Optional`

Empty `Optional` literals only store their type:


    encode-1.0(T₀) = T₁
    ────────────────────────────────────────
    encode-1.0([] : Optional T₀) = [ 5, T₁ ]


Non-empty `Optional` literals also store their value:


    encode-1.0(t₀) = t₁   encode-1.0(T₀) = T₁
    ────────────────────────────────────────────────
    encode-1.0([ t₀ ] : Optional T₀) = [ 5, T₁, t₁ ]


## `merge` expressions

`merge` expressions differ in their encoding depending on whether or not they
have a type annotation:


    encode-1.0(t₀) = t₁   encode-1.0(u₀) = u₁
    ─────────────────────────────────────────
    encode-1.0(merge t₀ u₀) = [ 6, t₁, u₁ ]


    encode-1.0(t₀) = t₁   encode-1.0(u₀) = u₁   encode-1.0(T₀) = T₁
    ───────────────────────────────────────────────────────────────
    encode-1.0(merge t₀ u₀ : T₀) = [ 6, t₁, u₁, T₁ ]


## Records

Dhall record types translate to CBOR maps:


    encode-1.0(T₀) = T₁
    ──────────────────────────────────────────────────
    encode-1.0({ x : T₀, … }) = [ 7, { "x" = T₁, … } ]


Dhall record literals translate to CBOR maps:


    encode-1.0(t₀) = t₁
    ──────────────────────────────────────────────────
    encode-1.0({ x = t₀, … }) = [ 8, { "x" = t₁, … } ]


Field access:


    encode-1.0(t₀) = t₁
    ─────────────────────────────────
    encode-1.0(t₀.x) = [ 9, t₁, "x" ]


... is encoded differently than record projection:


    encode-1.0(t₀) = t₁
    ────────────────────────────────────────────────────
    encode-1.0(t₀.{ x, y, … }) = [ 10, t₁, "x", "y", … ]


## Unions

Dhall union types translate to CBOR maps:


    encode-1.0(T₀) = T₁
    ────────────────────────────────────────────────────
    encode-1.0(< x : T₀ | … >) = [ 11, { "x" = T₁, … } ]


Dhall union literals store the specified alternative followed by the alternative
types encoded as CBOR map:


    encode-1.0(t₀) = t₁   encode-1.0(T₀) = T₁   …
    ──────────────────────────────────────────────────────────────────────
    encode-1.0(< x = t₀ | y : T₀ | … >) = [ 12, "x", t₁, { "y" = T₁, … } ]


The `constructors` keyword is encoded as:


    encode-1.0(u₀) = u₁
    ───────────────────────────────────────
    encode-1.0(constructors u₀) = [ 13, u₁]


## `Bool`

Boolean literals are encoded using CBOR's built-in support for boolean values:


    ───────────────────────
    encode-1.0(True) = True


    ─────────────────────────
    encode-1.0(False) = False


`if` expressions are encoded with a tag:


    encode-1.0(t₀) = t₁   encode-1.0(l₀) = l₁   encode-1.0(r₀) = r₁
    ───────────────────────────────────────────────────────────────
    encode-1.0(if t₀ then l₀ else r₀) = [ 14, t₁, l₁, r₁ ]


## `Natural`

`Natural` literals are encoded using the smallest available numeric
representation:


    ─────────────────────────  ; n < 2^64
    encode-1.0(n) = [ 15, n ]


    ──────────────────────────  ; 2^64 <= nn
    encode-1.0(n) = [ 15, nn ]


## `Integer`

`Integer` literals are encoded using the smallest available numeric
representation:


    ────────────────────────────  ; ±n < -2^64
    encode-1.0(±n) = [ 16, -nn ]


    ───────────────────────────  ; -2^64 <= ±n < 0
    encode-1.0(±n) = [ 16, -n ]


    ──────────────────────────  ; 0 <= ±n < 2^64
    encode-1.0(±n) = [ 16, n ]


    ───────────────────────────  ; 2^64 <= ±n
    encode-1.0(±n) = [ 16, nn ]


## `Double`

`Double`s are always encoded as CBOR decimal fractions in order to avoid loss
of precision when converting to and from their textual representation:


    ─────────────────────────────
    encode-1.0(n.n) = [ 17, n.n ]


## `Text`

`Text` literals are encoded as an alternation between their text chunks and
any interpolated expressions:


    encode-1.0(b₀) = b₁   encode-1.0(d₀) = d₁   …   encode-1.0(y₀) = y₁
    ───────────────────────────────────────────────────────────────────────────────────
    encode-1.0("a${b₀}c${d}e…x${y₀}z") = [ 18, "a", b₁, "c", d₁, "e", …, "x", y₁, "z" ]


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


    ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    encode-1.0(http://authority/path₀/path₁/…/file?query#fragment) = [ 24, 0, "authority" "path₀", "path₁", …, "file", "query", "fragment" ]


    ────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    encode-1.0(http://authority/path₀/path₁/…/file) = [ 24, 0, "authority" "path₀", "path₁", …, "file", null, null ]



    ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    encode-1.0(https://authority/path₀/path₁/…/file?query#fragment) = [ 24, 1, "authority" "path₀", "path₁", …, "file", "query", "fragment" ]


    ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    encode-1.0(https://authority/path₀/path₁/…/file) = [ 24, 1, "authority" "path₀", "path₁", …, "file", null, null ]


Absolute file paths are tokenized in the same way:


    ────────────────────────────────────────────────────────────────────────
    encode-1.0(/path₀/path₁/…/file) = [ 24, 2, "path₀", "path₁", …, "file" ]


Each path type is treated as another "scheme" (i.e. they are distinguished by
the second tag):


    ─────────────────────────────────────────────────────────────────────────
    encode-1.0(./path₀/path₁/…/file) = [ 24, 3, "path₀", "path₁", …, "file" ]


    ──────────────────────────────────────────────────────────────────────────
    encode-1.0(../path₀/path₁/…/file) = [ 24, 4, "path₀", "path₁", …, "file" ]


    ─────────────────────────────────────────────────────────────────────────
    encode-1.0(~/path₀/path₁/…/file) = [ 24, 5, "path₀", "path₁", …, "file" ]


Environment variables are also treated as another scheme:


    ──────────────────────────────────
    encode-1.0(env:x) = [ 24, 6, "x" ]


The `missing` keyword is also treated as another import type:


    ───────────────────────────────
    encode-1.0(missing) = [ 24, 7 ]


## `let` expressions

`let` expressions differ in their encoding depending on whether or not they have
a type annotation:


    encode-1.0(a₀) = a₁   encode-1.0(b₀) = b₁
    ──────────────────────────────────────────────────
    encode-1.0(let x = a₀ in b₀) = [ 25, "x", a₁, b₁ ]


    encode-1.0(a₀) = a₁   encode-1.0(A₀) = A₁   encode-1.0(b₀) = b₁
    ───────────────────────────────────────────────────────────────
    encode-1.0(let x : A₀ = a₀ in b₀) = [ 25, "x", A₁, a₁, b₁ ]


## Type annotations


    encode-1.0(t₀) = t₁   encode-1.0(T₀) = T₁
    ─────────────────────────────────────────
    encode-1.0(t₀ : T₀) = [ 26, t₁, T₁ ]


## Versioning judgments

Binary serialization of a Dhall expression encoded with a version tag is a
function of the following form:

    encodeWithVersion-*(dhall) = cbor

... where:

* `dhall` (the input) is a Dhall expression
* `cbor` (the output) is a CBOR expression

... and replacing `*` with the protocol version string of the expression to
encode.

The rule is simple:


    encode-1.0(e₀) = e₁
    ───────────────────────────────────────
    encodeWithVersion-1.0(e₀) = [ "1.0", e₁ ]
