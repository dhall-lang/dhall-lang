# Binary semantics

This document formalizes the semantics for encoding and decoding Dhall
expressions to and from a binary representation

This exists as a separate document since this will likely grow as the binary
protocol evolves in order to specify how to encode and decode multiple
versions of the binary protocol.

* [Motivation](#motivation)
* [CBOR](#cbor)
* [Versioning](#versioning)
* [CBOR expressions](#cbor-expressions)
* [Encoding judgment](#encoding-judgment)
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
* [Decoding judgment](#decoding-judgment)
    * [Built-in constants](#built-in-constants-1)
    * [Variables](#variables-1)
    * [Function application](#function-application-1)
    * [Functions](#functions-1)
    * [Operators](#operators-1)
    * [`List`](#list-1)
    * [`Optional`](#optional-1)
    * [`merge`-expressions](#merge-expressions-1)
    * [Records](#records-1)
    * [Unions](#unions-1)
    * [`Bool`](#bool-1)
    * [`Natural`](#natural-1)
    * [`Integer`](#integer-1)
    * [`Double`](#double-1)
    * [`Text`](#text-1)
    * [Imports](#imports-1)
    * [`let`-expressions](#let-expressions-1)
    * [Type annotations](#type-annotations-1)
* [Versioning judgment](#versioning-judgment)
* [Protocol evolution](#protocol-evolution)

## Motivation

Dhall's import system requires a standard way to convert expressions to and from
a binary representation, for two reasons:

* Users can imported expressions protected by a "semantic integrity check",
  which is a SHA-256 hash of the binary representation of an expression's
  normal form

* Interpreters can locally cache imported expressions if the user protects them
  with a semantic integrity check.  The local cache stores expressions using
  the SHA-256 hash as the lookup key and the binary representation of the
  expression as the cached value.

Implementations also may want to support encoding and decoding multiple
versions of the protocol for backwards compatibility.  An implementation that
supports older protocol versions allows users to defer updating their semantic
integrity checks or rebuilding their cache until they require newer language
features.

## CBOR

Dhall's semantics for binary serialization are defined in terms of the
Concise Binary Object Representation (CBOR)
([RFC 7049](https://tools.ietf.org/html/rfc7049)).  This section first specifies
a simplified grammar for the subset of CBOR that Dhall uses
(i.e. "CBOR expressions") and then specifies how to convert Dhall expressions to
and from these CBOR expressions.

The `encode` judgments in this section specify how to convert a Dhall
expression to a CBOR expression.  Once you have a CBOR expression you can
serialize that CBOR expression to a binary representation according to RFC 7049.

The `decode` judgments in this section specify how to convert a CBOR
expression to a Dhall expression.  Once you deserialize a CBOR expression from
a binary representation then you can further decode that CBOR expression to a
Dhall expression.

For efficiency reasons, an implementation MAY elect to not go through an
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

    "1.1"

A compliant implementation MUST be able to encode and decode the latest
version of the protocol (i.e. an expression tagged with the above binary
protocol version string).

A compliant implementation MAY support encoding or decoding other versions of
the protocol standardized within this document for backwards compatibility.

The protocol version strings will typically be version numbers of the form
"X.Y", where:

* Changing the version from "X.Y" to "X.{Y + 1}" indicates a non-breaking change
* Changing the version from "X.Y" to "{X + 1}.0" indicates a breaking change

"X" is "major version number" and "Y" is the "minor version number".

This version number convention is a convention and is not enforced by the
standardized semantics.  Whenever the version numbering convention is
inconsistent with the standardized encoding/decoding judgments, the judgments
are authoritative.

For example, a judgment that decodes expressions tagged with a protocol version
string of "X.{Y + 1}" should in theory also be able to decode expressions tagged
with a protocol version string of "X.Y".  In practice a standardization error
might cause the newer protocol version to be incompatible with the older
protocol version despite sharing the same major version number.

Implementations MUST NOT assume backwards compatibility unless that
compatibility is explicitly standardized.  For example, if the decoder for
protocol version string `X.Z` is backward compatible with protocol version
string `X.Y` then you will see two judgments making that compatibility
explicit:


        decode-X.Z(e₀) = e₁
        ───────────────────────────────────────
        decodeWithVersion-X.Y([ "X.Y", e₀) = e₁

        decode-X.Z(e₀) = e₁
        ───────────────────────────────────────
        decodeWithVersion-X.Z([ "X.Z", e₀) = e₁


Notice that each judgment expects a different protocol version string but both
judgments reuse the same decoding logic (for protocol version "X.Z" in this
case) in order to avoid independently specifying the decoding logic for both
protocol versions.

The protocol version string format might change in the future.  For example,
at some point the protocol version string might become a URI or a SHA256 hash.
Therefore, implementations MUST only match on the exact protocol version strings
enumerated in the decoding judgments.  Implementations MUST NOT match a prefix
of the version string, MUST NOT attempt to parse the protocol version string
as a version number, and MUST NOT attempt to interpret the protocol version
string in any way other than to test the string for equality with an expected
protocol version string.

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

## Encoding judgment

You can encode a naked Dhall expression using the following judgment:

    encode-*(dhall) = cbor

... where:

* `dhall` (the input) is a Dhall expression
* `cbor` (the output) is a CBOR expression

... and replacing `*` with the protocol version string of the expression to
encode.

The encoding logic includes several optimizations for more compactly encoding
expressions that are fully resolved and αβ-normalized because expressions are
fully interpreted before they are hashed or cached.  For example, the encoding
uses tags less than 24 for language features that survive both import resolution
and normalization (such as a function type) since those tags fit in one byte.
Language features that don't survive full interpretation (such as a `let`
expression or an import) use tags of 24 or above.  Similarly, the encoding using
a more compact representation for variables named `_` because no other variable
names remain after α-normalization.

Do not construe these optimizations to mean that you need to resolve imports
or αβ-normalize expressions.  You may encode or decode expressions that have
not been resolved in any way.

### Variables

The binary representation encodes variables as compactly as possible, because:

* Variables occur frequently in Dhall expressions
* Built-in constants occur frequently and they are encoded as free variables

Encode a variable named `"_"` as its index, using the smallest numeric
representation available:


    ───────────────────  ; n < 2^64
    encode-1.1(_@n) = n


    ────────────────────  ; 2^64 <= n
    encode-1.1(_@n) = nn


This optimization takes advantage of the fact that α-normalized expressions
only use variables named `_`.  Encoding an α-normalized expression is equivalent
to using De-Bruijn indices to label variables.

The reason for this optimization is because expressions are commonly
α-normalized before encoding them, such as when computing their semantic
integrity check and also when caching them.

Encode a variable that is not named `"_"` with an index of `0` as a naked CBOR
text string matching the variable's identifier:


    ─────────────────────  ; x ≠ "_"
    encode-1.1(x@0) = "x"


Otherwise encode a variable as a two-element CBOR array where the first element
is the identifier and the second element is the encoded index (using the
smallest numeric representation available):


    ────────────────────────────  ; n < 2^64
    encode-1.1(x@n) = [ "x", n ]


    ─────────────────────────────  ; 2^64 <= nn
    encode-1.1(x@n) = [ "x", nn ]


### Built-in constants

Encode all built-in constants (except boolean values) as free variables of the
same name with an index of 0, which is equivalent to encoding them as a naked
string matching their identifier.


    ───────────────────────────────────────────
    encode-1.1(Natural/build) = "Natural/build"


    ─────────────────────────────────────────
    encode-1.1(Natural/fold) = "Natural/fold"


    ─────────────────────────────────────────────
    encode-1.1(Natural/isZero) = "Natural/isZero"


    ─────────────────────────────────────────
    encode-1.1(Natural/even) = "Natural/even"


    ───────────────────────────────────────
    encode-1.1(Natural/odd) = "Natural/odd"


    ───────────────────────────────────────────────────
    encode-1.1(Natural/toInteger) = "Natural/toInteger"


    ─────────────────────────────────────────
    encode-1.1(Natural/show) = "Natural/show"


    ─────────────────────────────────────────────────
    encode-1.1(Integer/toDouble) = "Integer/toDouble"


    ─────────────────────────────────────────
    encode-1.1(Integer/show) = "Integer/show"


    ───────────────────────────────────────
    encode-1.1(Double/show) = "Double/show"


    ─────────────────────────────────────
    encode-1.1(List/build) = "List/build"


    ───────────────────────────────────
    encode-1.1(List/fold) = "List/fold"


    ───────────────────────────────────────
    encode-1.1(List/length) = "List/length"


    ───────────────────────────────────
    encode-1.1(List/head) = "List/head"


    ───────────────────────────────────
    encode-1.1(List/last) = "List/last"


    ─────────────────────────────────────────
    encode-1.1(List/indexed) = "List/indexed"


    ─────────────────────────────────────────
    encode-1.1(List/reverse) = "List/reverse"


    ───────────────────────────────────────────
    encode-1.1(Optional/fold) = "Optional/fold"


    ─────────────────────────────────────────────
    encode-1.1(Optional/build) = "Optional/build"


    ─────────────────────────
    encode-1.1(Bool) = "Bool"


    ─────────────────────────────────
    encode-1.1(Optional) = "Optional"


    ─────────────────────────
    encode-1.1(None) = "None"


    ───────────────────────────────
    encode-1.1(Natural) = "Natural"


    ───────────────────────────────
    encode-1.1(Integer) = "Integer"


    ─────────────────────────────
    encode-1.1(Double) = "Double"


    ─────────────────────────
    encode-1.1(Text) = "Text"


    ─────────────────────────
    encode-1.1(List) = "List"


    ─────────────────────────
    encode-1.1(Type) = "Type"


    ─────────────────────────
    encode-1.1(Kind) = "Kind"


### Function application

Function application is encoded as a heterogeneous array where a function
applied to multiple arguments is stored within a single array:


    encode-1.1(f₀) = f₁   encode-1.1(a₀) = a₁   encode-1.1(b₀) = b₁   …
    ───────────────────────────────────────────────────────────────────
    encode-1.1(f₀ a₀ b₀ …) = [ 0, f₁, a₁, b₁, … ]


### Functions

Functions that bind variables named `_` have a more compact representation:


    encode-1.1(A₀) = A₁   encode-1.1(b₀) = b₁
    ──────────────────────────────────────────
    encode-1.1(λ(_ : A₀) → b₀) = [ 1, A₁, b₁ ]


... than functions that bind variables of other names:


    encode-1.1(A₀) = A₁   encode-1.1(b₀) = b₁
    ───────────────────────────────────────────────  ; x ≠ "_"
    encode-1.1(λ(x : A₀) → b₀) = [ 1, "x", A₁, b₁ ]


Function types that bind variables named `_` also have a more compact
representation:


    encode-1.1(A₀) = A₁   encode-1.1(B₀) = B₁
    ──────────────────────────────────────────
    encode-1.1(∀(_ : A₀) → B₀) = [ 2, A₁, B₁ ]


... than function types that bind variables of other names:


    encode-1.1(A₀) = A₁   encode-1.1(B₀) = B₁
    ───────────────────────────────────────────────  ; x ≠ "_"
    encode-1.1(∀(x : A₀) → B₀) = [ 2, "x", A₁, B₁ ]


### Operators

Operators are encoded as integer tags alongside their two arguments:


    encode-1.1(l₀) = l₁   encode-1.1(r₀) = r₁
    ─────────────────────────────────────────
    encode-1.1(l₀ || r₀) = [ 3, 0, l₁, r₁ ]


    encode-1.1(l₀) = l₁   encode-1.1(r₀) = r₁
    ─────────────────────────────────────────
    encode-1.1(l₀ && r₀) = [ 3, 1, l₁, r₁ ]


    encode-1.1(l₀) = l₁   encode-1.1(r₀) = r₁
    ─────────────────────────────────────────
    encode-1.1(l₀ == r₀) = [ 3, 2, l₁, r₁ ]


    encode-1.1(l₀) = l₁   encode-1.1(r₀) = r₁
    ─────────────────────────────────────────
    encode-1.1(l₀ != r₀) = [ 3, 3, l₁, r₁ ]


    encode-1.1(l₀) = l₁   encode-1.1(r₀) = r₁
    ─────────────────────────────────────────
    encode-1.1(l₀ + r₀) = [ 3, 4, l₁, r₁ ]


    encode-1.1(l₀) = l₁   encode-1.1(r₀) = r₁
    ─────────────────────────────────────────
    encode-1.1(l₀ * r₀) = [ 3, 5, l₁, r₁ ]


    encode-1.1(l₀) = l₁   encode-1.1(r₀) = r₁
    ─────────────────────────────────────────
    encode-1.1(l₀ ++ r₀) = [ 3, 6, l₁, r₁ ]


    encode-1.1(l₀) = l₁   encode-1.1(r₀) = r₁
    ─────────────────────────────────────────
    encode-1.1(l₀ # r₀) = [ 3, 7, l₁, r₁ ]


    encode-1.1(l₀) = l₁   encode-1.1(r₀) = r₁
    ─────────────────────────────────────────
    encode-1.1(l₀ ∧ r₀) = [ 3, 8, l₁, r₁ ]


    encode-1.1(l₀) = l₁   encode-1.1(r₀) = r₁
    ─────────────────────────────────────────
    encode-1.1(l₀ ⫽ r₀) = [ 3, 9, l₁, r₁ ]


    encode-1.1(l₀) = l₁   encode-1.1(r₀) = r₁
    ─────────────────────────────────────────
    encode-1.1(l₀ ⩓ r₀) = [ 3, 10, l₁, r₁ ]


    encode-1.1(l₀) = l₁   encode-1.1(r₀) = r₁
    ─────────────────────────────────────────
    encode-1.1(l₀ ? r₀) = [ 3, 11, l₁, r₁ ]


### `List`

Empty `List`s only store their type:


    encode-1.1(T₀) = T₁
    ────────────────────────────────────
    encode-1.1([] : List T₀) = [ 4, T₁ ]


Non-empty `List`s don't store their type, but do store their elements inline:


    encode-1.1(a₀) = a₁   encode-1.1(b₀) = b₁
    ──────────────────────────────────────────────────
    encode-1.1([ a₀, b₀, … ]) = [ 4, null, a₁, b₁, … ]


### `Optional`

Empty `Optional` literals using the legacy `List`-like syntax only store their
type:


    encode-1.1(T₀) = T₁
    ────────────────────────────────────────
    encode-1.1([] : Optional T₀) = [ 5, T₁ ]


Non-empty `Optional` literals store the type (if present) and their value:


    encode-1.1(t₀) = t₁   encode-1.1(T₀) = T₁
    ────────────────────────────────────────────────
    encode-1.1([ t₀ ] : Optional T₀) = [ 5, T₁, t₁ ]


    encode-1.1(t₀) = t₁
    ─────────────────────────────────────
    encode-1.1(Some t₀) = [ 5, null, t₁ ]


### `merge` expressions

`merge` expressions differ in their encoding depending on whether or not they
have a type annotation:


    encode-1.1(t₀) = t₁   encode-1.1(u₀) = u₁
    ─────────────────────────────────────────
    encode-1.1(merge t₀ u₀) = [ 6, t₁, u₁ ]


    encode-1.1(t₀) = t₁   encode-1.1(u₀) = u₁   encode-1.1(T₀) = T₁
    ───────────────────────────────────────────────────────────────
    encode-1.1(merge t₀ u₀ : T₀) = [ 6, t₁, u₁, T₁ ]


### Records

Dhall record types translate to CBOR maps:


    encode-1.1(T₀) = T₁   …
    ──────────────────────────────────────────────────
    encode-1.1({ x : T₀, … }) = [ 7, { "x" = T₁, … } ]


Dhall record literals translate to CBOR maps:


    encode-1.1(t₀) = t₁   …
    ──────────────────────────────────────────────────
    encode-1.1({ x = t₀, … }) = [ 8, { "x" = t₁, … } ]


Field access:


    encode-1.1(t₀) = t₁   …
    ─────────────────────────────────
    encode-1.1(t₀.x) = [ 9, t₁, "x" ]


... is encoded differently than record projection:


    encode-1.1(t₀) = t₁   …
    ────────────────────────────────────────────────────
    encode-1.1(t₀.{ x, y, … }) = [ 10, t₁, "x", "y", … ]


### Unions

Dhall union types translate to CBOR maps:


    encode-1.1(T₀) = T₁   …
    ────────────────────────────────────────────────────
    encode-1.1(< x : T₀ | … >) = [ 11, { "x" = T₁, … } ]


Dhall union literals store the specified alternative followed by the alternative
types encoded as CBOR map:


    encode-1.1(t₀) = t₁   encode-1.1(T₀) = T₁   …
    ──────────────────────────────────────────────────────────────────────
    encode-1.1(< x = t₀ | y : T₀ | … >) = [ 12, "x", t₁, { "y" = T₁, … } ]


Encode the `constructors` keyword as:


    encode-1.1(u₀) = u₁
    ───────────────────────────────────────
    encode-1.1(constructors u₀) = [ 13, u₁]


### `Bool`

Encode Boolean literals using CBOR's built-in support for Boolean values:


    ───────────────────────
    encode-1.1(True) = True


    ─────────────────────────
    encode-1.1(False) = False


`if` expressions are encoded with a tag:


    encode-1.1(t₀) = t₁   encode-1.1(l₀) = l₁   encode-1.1(r₀) = r₁
    ───────────────────────────────────────────────────────────────
    encode-1.1(if t₀ then l₀ else r₀) = [ 14, t₁, l₁, r₁ ]


### `Natural`

Encode `Natural` literals using the smallest available numeric representation:


    ─────────────────────────  ; n < 2^64
    encode-1.1(n) = [ 15, n ]


    ──────────────────────────  ; 2^64 <= nn
    encode-1.1(n) = [ 15, nn ]


### `Integer`

Encode `Integer` literals using the smallest available numeric representation:


    ────────────────────────────  ; ±n < -2^64
    encode-1.1(±n) = [ 16, -nn ]


    ───────────────────────────  ; -2^64 <= ±n < 0
    encode-1.1(±n) = [ 16, -n ]


    ──────────────────────────  ; 0 <= ±n < 2^64
    encode-1.1(±n) = [ 16, n ]


    ───────────────────────────  ; 2^64 <= ±n
    encode-1.1(±n) = [ 16, nn ]


### `Double`

Encode `Double`s as CBOR decimal fractions in order to avoid loss of precision
when converting to and from their textual representation:


    ─────────────────────────────
    encode-1.1(n.n) = [ 17, n.n ]


### `Text`

Encode `Text` literals as an alternation between their text chunks and any
interpolated expressions:


    encode-1.1(b₀) = b₁   encode-1.1(d₀) = d₁   …   encode-1.1(y₀) = y₁
    ───────────────────────────────────────────────────────────────────────────────────
    encode-1.1("a${b₀}c${d}e…x${y₀}z") = [ 18, "a", b₁, "c", d₁, "e", …, "x", y₁, "z" ]


Note that the encoding of a text literal always begins and ends with a string,
even if the first or last chunk is the empty string.

### Imports

Encode URL imports in a tokenized form with the following elements in order:

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
    encode-1.1(http://authority/path₀/path₁/…/file?query#fragment) = [ 24, 0, "authority" "path₀", "path₁", …, "file", "query", "fragment" ]


    ────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    encode-1.1(http://authority/path₀/path₁/…/file) = [ 24, 0, "authority" "path₀", "path₁", …, "file", null, null ]



    ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    encode-1.1(https://authority/path₀/path₁/…/file?query#fragment) = [ 24, 1, "authority" "path₀", "path₁", …, "file", "query", "fragment" ]


    ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    encode-1.1(https://authority/path₀/path₁/…/file) = [ 24, 1, "authority" "path₀", "path₁", …, "file", null, null ]


Absolute file paths are tokenized in the same way:


    ────────────────────────────────────────────────────────────────────────
    encode-1.1(/path₀/path₁/…/file) = [ 24, 2, "path₀", "path₁", …, "file" ]


Each path type is treated as another "scheme" (i.e. they are distinguished by
the second tag):


    ─────────────────────────────────────────────────────────────────────────
    encode-1.1(./path₀/path₁/…/file) = [ 24, 3, "path₀", "path₁", …, "file" ]


    ──────────────────────────────────────────────────────────────────────────
    encode-1.1(../path₀/path₁/…/file) = [ 24, 4, "path₀", "path₁", …, "file" ]


    ─────────────────────────────────────────────────────────────────────────
    encode-1.1(~/path₀/path₁/…/file) = [ 24, 5, "path₀", "path₁", …, "file" ]


Environment variables are also treated as another scheme:


    ──────────────────────────────────
    encode-1.1(env:x) = [ 24, 6, "x" ]


The `missing` keyword is also treated as another import type:


    ───────────────────────────────
    encode-1.1(missing) = [ 24, 7 ]


### `let` expressions

`let` expressions differ in their encoding depending on whether or not they have
a type annotation:


    encode-1.1(a₀) = a₁   encode-1.1(b₀) = b₁
    ──────────────────────────────────────────────────
    encode-1.1(let x = a₀ in b₀) = [ 25, "x", a₁, b₁ ]


    encode-1.1(a₀) = a₁   encode-1.1(A₀) = A₁   encode-1.1(b₀) = b₁
    ───────────────────────────────────────────────────────────────
    encode-1.1(let x : A₀ = a₀ in b₀) = [ 25, "x", A₁, a₁, b₁ ]


### Type annotations


    encode-1.1(t₀) = t₁   encode-1.1(T₀) = T₁
    ─────────────────────────────────────────
    encode-1.1(t₀ : T₀) = [ 26, t₁, T₁ ]


## Decoding judgment


You can decode a Dhall expression using the following judgment:

    decode-*(cbor) = dhall

... where:

* `cbor` (the output) is a CBOR expression
* `dhall` (the input) is a Dhall expression

... and replacing `*` with the protocol version string of the expression to
encode.

### Built-in constants

A naked CBOR string could have been produced by encoding either a built-in
identifier or a variable.  First, attempt to decode the string as a built-in
identifier if it matches any of the following strings:


    ───────────────────────────────────────────
    decode-1.1("Natural/build") = Natural/build


    ─────────────────────────────────────────
    decode-1.1("Natural/fold") = Natural/fold


    ─────────────────────────────────────────────
    decode-1.1("Natural/isZero") = Natural/isZero


    ─────────────────────────────────────────
    decode-1.1("Natural/even") = Natural/even


    ───────────────────────────────────────
    decode-1.1("Natural/odd") = Natural/odd


    ───────────────────────────────────────────────────
    decode-1.1("Natural/toInteger") = Natural/toInteger


    ─────────────────────────────────────────
    decode-1.1("Natural/show") = Natural/show


    ─────────────────────────────────────────────────
    decode-1.1("Integer/toDouble") = Integer/toDouble


    ─────────────────────────────────────────
    decode-1.1("Integer/show") = Integer/show


    ───────────────────────────────────────
    decode-1.1("Double/show") = Double/show


    ─────────────────────────────────────
    decode-1.1("List/build") = List/build


    ───────────────────────────────────
    decode-1.1("List/fold") = List/fold


    ───────────────────────────────────────
    decode-1.1("List/length") = List/length


    ───────────────────────────────────
    decode-1.1("List/head") = List/head


    ───────────────────────────────────
    decode-1.1("List/last") = List/last


    ─────────────────────────────────────────
    decode-1.1("List/indexed") = List/indexed


    ─────────────────────────────────────────
    decode-1.1("List/reverse") = List/reverse


    ───────────────────────────────────────────
    decode-1.1("Optional/fold") = Optional/fold


    ─────────────────────────────────────────────
    decode-1.1("Optional/build") = Optional/build


    ─────────────────────────
    decode-1.1("Bool") = Bool


    ─────────────────────────────────
    decode-1.1("Optional") = Optional


    ─────────────────────────
    decode-1.1("None") = None


    ───────────────────────────────
    decode-1.1("Natural") = Natural


    ───────────────────────────────
    decode-1.1("Integer") = Integer


    ─────────────────────────────
    decode-1.1("Double") = Double


    ─────────────────────────
    decode-1.1("Text") = Text


    ─────────────────────────
    decode-1.1("List") = List


    ─────────────────────────
    decode-1.1("Type") = Type


    ─────────────────────────
    decode-1.1("Kind") = Kind


Otherwise, decode the CBOR string as an ordinary variable as outlined in the
next section.

### Variables

Decode a naked CBOR string to a variable with an index of 0 if the string does
not match a built-in identifier:


    ─────────────────────  ; x ∉ reservedIdentifiers
    decode-1.1("x") = x@0


Only variables named `_` encode to a naked CBOR integer, so decoding turns a
naked CBOR integer back into a variable named `_`:


    ───────────────────
    decode-1.1(n) = _@n


    ────────────────────
    decode-1.1(nn) = _@n


A decoder MUST accept an integer that is not encoded using the most compact
representation.  For example, a naked unsiged bignum storing `0` is valid, even
though the `0` could have been stored in a single byte as as a compact unsigned
integer instead.

A CBOR array beginning with a string indicates a variable.  The array should
only have two elements, the first of which is a string and the second of which
is the variable index, which can be either a compact integer or a bignum:


    ────────────────────────────
    decode-1.1([ "x", n ]) = x@n


    ─────────────────────────────
    decode-1.1([ "x", nn ]) = x@n


As before, the encoded integer need not be stored in the most compact
representation.

### Function application

Decode a CBOR array beginning with `0` as function application, where the second
element is the function and the remaining elements are the function arguments:


    decode-1.1(f₁) = f₀   decode-1.1(a₁) = a₀   decode-1.1(b₁) = b₀   …
    ───────────────────────────────────────────────────────────────────
    decode-1.1([ 0, f₁, a₁, b₁, … ]) = f₀ a₀ b₀ …


A decoder MUST require at least 1 function argument.  In other words, a decode
MUST reject a CBOR array of of the form `[ 0, f₁ ]`.

### Functions

Decode a CBOR array beginning with a `1` as a λ-expression.  If the array has
three elements then the bound variable is named `_`:


    decode-1.1(A₁) = A₀   decode-1.1(b₁) = b₀
    ──────────────────────────────────────────
    decode-1.1([ 1, A₁, b₁ ]) = λ(_ : A₀) → b₀


... otherwise if the array has four elements then the name of the bound variable
is included in the array:


    decode-1.1(A₁) = A₀   decode-1.1(b₁) = b₀
    ───────────────────────────────────────────────  ; x ≠ "_"
    decode-1.1([ 1, "x", A₁, b₁ ]) = λ(x : A₀) → b₀


A decoder MUST reject the latter form if the bound variable is named `_`

Decode a CBOR array beginning with a `2` as a ∀-expression.  If the array has
three elements then the bound variable is named `_`:


    decode-1.1(A₁) = A₀   decode-1.1(B₁) = B₀
    ──────────────────────────────────────────
    decode-1.1([ 2, A₁, B₁ ]) = ∀(_ : A₀) → B₀


... otherwise if the array has four elements then the name of the bound variable
is included in the array:


    decode-1.1(A₁) = A₀   decode-1.1(B₁) = B₀
    ───────────────────────────────────────────────  ; x ≠ "_"
    decode-1.1([ 2, "x", A₁, B₁ ]) = ∀(x : A₀) → B₀


A decoder MUST reject the latter form if the bound variable is named `_`

### Operators

Decode a CBOR array beginning with a `3` as an operator expression:


    decode-1.1(l₁) = l₀   decode-1.1(r₁) = r₀
    ─────────────────────────────────────────
    decode-1.1([ 3, 0, l₁, r₁ ]) = l₀ || r₀


    decode-1.1(l₁) = l₀   decode-1.1(r₁) = r₀
    ─────────────────────────────────────────
    decode-1.1([ 3, 1, l₁, r₁ ]) = l₀ && r₀


    decode-1.1(l₁) = l₀   decode-1.1(r₁) = r₀
    ─────────────────────────────────────────
    decode-1.1([ 3, 2, l₁, r₁ ]) = l₀ == r₀


    decode-1.1(l₁) = l₀   decode-1.1(r₁) = r₀
    ─────────────────────────────────────────
    decode-1.1([ 3, 3, l₁, r₁ ]) = l₀ != r₀


    decode-1.1(l₁) = l₀   decode-1.1(r₁) = r₀
    ─────────────────────────────────────────
    decode-1.1([ 3, 4, l₁, r₁ ]) = l₀ + r₀


    decode-1.1(l₁) = l₀   decode-1.1(r₁) = r₀
    ─────────────────────────────────────────
    decode-1.1([ 3, 5, l₁, r₁ ]) = l₀ * r₀


    decode-1.1(l₁) = l₀   decode-1.1(r₁) = r₀
    ─────────────────────────────────────────
    decode-1.1([ 3, 6, l₁, r₁ ]) = l₀ ++ r₀


    decode-1.1(l₁) = l₀   decode-1.1(r₁) = r₀
    ─────────────────────────────────────────
    decode-1.1([ 3, 7, l₁, r₁ ]) = l₀ # r₀


    decode-1.1(l₁) = l₀   decode-1.1(r₁) = r₀
    ─────────────────────────────────────────
    decode-1.1([ 3, 8, l₁, r₁ ]) = l₀ ∧ r₀


    decode-1.1(l₁) = l₀   decode-1.1(r₁) = r₀
    ─────────────────────────────────────────
    decode-1.1([ 3, 9, l₁, r₁ ]) = l₀ ⫽ r₀


    decode-1.1(l₁) = l₀   decode-1.1(r₁) = r₀
    ─────────────────────────────────────────
    decode-1.1([ 3, 10, l₁, r₁ ]) = l₀ ⩓ r₀


    decode-1.1(l₁) = l₀   decode-1.1(r₁) = r₀
    ─────────────────────────────────────────
    decode-1.1([ 3, 11, l₁, r₁ ]) = l₀ ? r₀


### `List`

Decode a CBOR array beginning with a `4` as a `List` literal

If the list is empty, then the type MUST be non-`null`:


    decode-1.1(T₁) = T₀
    ────────────────────────────────────
    decode-1.1([ 4, T₁ ]) = [] : List T₀


If the list is non-empty then the type MUST be `null`:


    decode-1.1(a₁) = a₀   decode-1.1(b₁) = b₀
    ──────────────────────────────────────────────────
    decode-1.1([ 4, null, a₁, b₁, … ]) = [ a₀, b₀, … ]


### `Optional`

Decode a CBOR array beginning with a `5` as an `Optional` literal


    decode-1.1(T₁) = T₀
    ────────────────────────────────────────
    decode-1.1([ 5, T₁ ]) = [] : Optional T₀


    decode-1.1(t₁) = t₀   decode-1.1(T₁) = T₀
    ────────────────────────────────────────────────
    decode-1.1([ 5, T₁, t₁ ]) = [ t₀ ] : Optional T₀


    decode-1.1(t₁) = t₀
    ─────────────────────────────────────
    decode-1.1([ 5, null, t₁ ]) = Some t₀


### `merge` expressions

Decode a CBOR array beginning with a `6` as a `merge` expression:


    decode-1.1(t₁) = t₀   decode-1.1(u₁) = u₀
    ─────────────────────────────────────────
    decode-1.1([ 6, t₁, u₁ ]) = merge t₀ u₀


    decode-1.1(t₁) = t₀   decode-1.1(u₁) = u₀   decode-1.1(T₁) = T₀
    ───────────────────────────────────────────────────────────────
    decode-1.1([ 6, t₁, u₁, T₁ ]) = merge t₀ u₀ : T₀


### Records

Decode a CBOR array beginning with a `7` as a record type:


    decode-1.1(T₁) = T₀   …
    ──────────────────────────────────────────────────
    decode-1.1([ 7, { "x" = T₁, … } ]) = { x : T₀, … }


Decode a CBOR array beginning with a `8` as a record literal:


    decode-1.1(t₁) = t₀   …
    ──────────────────────────────────────────────────
    decode-1.1([ 8, { "x" = t₁, … } ]) = { x = t₀, … }


Decode a CBOR array beginning with a `9` as a field access:


    decode-1.1(t₁) = t₀   …
    ─────────────────────────────────
    decode-1.1([ 9, t₁, "x" ]) = t₀.x


Decode a CBOR array beginning with a `10` as a record projection:


    decode-1.1(t₁) = t₀   …
    ────────────────────────────────────────────────────
    decode-1.1([ 10, t₁, "x", "y", … ]) = t₀.{ x, y, … }


A decoder MUST NOT attempt to enforce uniqueness of keys.  That is the
responsibility of the type-checking phase.

### Unions

Decode a CBOR array beginning with a `11` as a union type:


    decode-1.1(T₁) = T₀   …
    ────────────────────────────────────────────────────
    decode-1.1([ 11, { "x" = T₁, … } ]) = < x : T₀ | … >


Decode a CBOR array beginning with a `12` as a union type:


    decode-1.1(t₁) = t₀   decode-1.1(T₁) = T₀   …
    ──────────────────────────────────────────────────────────────────────
    decode-1.1([ 12, "x", t₁, { "y" = T₁, … } ]) = < x = t₀ | y : T₀ | … >


A decoder MUST NOT attempt to enforce uniqueness of keys.  That is the
responsibility of the type-checking phase.

Decode a CBOR array beginning with a `13` as a `constructors` application:


    decode-1.1(u₁) = u₀
    ───────────────────────────────────────
    decode-1.1([ 13, u₁]) = constructors u₀


### `Bool`

Decode CBOR boolean values to Dhall boolean values:


    ───────────────────────
    decode-1.1(True) = True


    ─────────────────────────
    decode-1.1(False) = False


Decode a CBOR array beginning with a `14` as an `if` expression:


    decode-1.1(t₁) = t₀   decode-1.1(l₁) = l₀   decode-1.1(r₁) = r₀
    ───────────────────────────────────────────────────────────────
    decode-1.1([ 14, t₁, l₁, r₁ ]) = if t₀ then l₀ else r₀


### `Natural`

Decode a CBOR array beginning with a `15` as a `Natural` literal:


    ─────────────────────────
    decode-1.1([ 15, n ]) = n


    ──────────────────────────
    decode-1.1([ 15, nn ]) = n


A decoder MUST accept an integer that is not encoded using the most compact
representation.

### `Integer`

Decode a CBOR array beginning with a `16` as an `Integer` literal:


    ────────────────────────────
    decode-1.1([ 16, -nn ]) = ±n


    ───────────────────────────
    decode-1.1([ 16, -n ]) = ±n


    ──────────────────────────
    decode-1.1([ 16, n ]) = ±n


    ───────────────────────────
    decode-1.1([ 16, nn ]) = ±n


A decoder MUST accept an integer that is not encoded using the most compact
representation.

### `Double`

Decode a CBOR array beginning with a `17` as an `Double` literal:


    ─────────────────────────────
    decode-1.1([ 17, n.n ]) = n.n


A decoder MUST accept a decimal fraction that is not encoded using the most
compact representation.

### `Text`

Decode a CBOR array beginning with a `18` as a `Text` literal:


    decode-1.1(b₁) = b₀   decode-1.1(d₁) = d₀   …   decode-1.1(y₁) = y₀
    ───────────────────────────────────────────────────────────────────────────────────
    decode-1.1([ 18, "a", b₁, "c", d₁, "e", …, "x", y₁, "z" ]) = "a${b₀}c${d}e…x${y₀}z"


### Imports

Decode a CBOR array beginning with a `24` as an import

The decoding rules are the exact opposite of the encoding rules:


    ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    decode-1.1([ 24, 0, "authority" "path₀", "path₁", …, "file", "query", "fragment" ]) = http://authority/path₀/path₁/…/file?query#fragment


    ────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    decode-1.1([ 24, 0, "authority" "path₀", "path₁", …, "file", null, null ]) = http://authority/path₀/path₁/…/file



    ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    decode-1.1([ 24, 1, "authority" "path₀", "path₁", …, "file", "query", "fragment" ]) = https://authority/path₀/path₁/…/file?query#fragment


    ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    decode-1.1([ 24, 1, "authority" "path₀", "path₁", …, "file", null, null ]) = https://authority/path₀/path₁/…/file


    ────────────────────────────────────────────────────────────────────────
    decode-1.1([ 24, 2, "path₀", "path₁", …, "file" ]) = /path₀/path₁/…/file


    ─────────────────────────────────────────────────────────────────────────
    decode-1.1([ 24, 3, "path₀", "path₁", …, "file" ]) = ./path₀/path₁/…/file


    ──────────────────────────────────────────────────────────────────────────
    decode-1.1([ 24, 4, "path₀", "path₁", …, "file" ]) = ../path₀/path₁/…/file


    ─────────────────────────────────────────────────────────────────────────
    decode-1.1([ 24, 5, "path₀", "path₁", …, "file" ]) = ~/path₀/path₁/…/file


    ──────────────────────────────────
    decode-1.1([ 24, 6, "x" ]) = env:x


    ───────────────────────────────
    decode-1.1([ 24, 7 ]) = missing


### `let` expressions

Decode a CBOR array beginning with a `25` as a `let` expression:


    decode-1.1(a₁) = a₀   decode-1.1(b₁) = b₀
    ──────────────────────────────────────────────────
    decode-1.1([ 25, "x", a₁, b₁ ]) = let x = a₀ in b₀


    decode-1.1(a₁) = a₀   decode-1.1(A₁) = A₀   decode-1.1(b₁) = b₀
    ───────────────────────────────────────────────────────────────
    decode-1.1([ 25, "x", A₁, a₁, b₁ ]) = let x : A₀ = a₀ in b₀


### Type annotations


    decode-1.1(t₁) = t₀   decode-1.1(T₁) = T₀
    ─────────────────────────────────────────
    decode-1.1([ 26, t₁, T₁ ]) = t₀ : T₀


## Versioning judgments

You can encode a Dhall expression tagged with a protocol version string using
the following judgment:

    encodeWithVersion-*(dhall) = cbor

... where:

* `dhall` (the input) is a Dhall expression
* `cbor` (the output) is a CBOR expression

... and replacing `*` with the protocol version string of the expression to
encode.

You can decode a Dhall expression tagged with a protocol version string using
the following judgment:

    decodeWithVersion-*(cbor) = dhall

... where:

* `dhall` (the input) is a Dhall expression
* `cbor` (the output) is a CBOR expression

... and replacing `*` with the protocol version string of the expression to
decode.


    encode-1.1(e₀) = e₁
    ─────────────────────────────────────────
    encodeWithVersion-1.1(e₀) = [ "1.1", e₁ ]


    decode-1.1(e₀) = e₁
    ─────────────────────────────────────────
    decodeWithVersion-1.1([ "1.1", e₁ ]) = e₀


Version "1.1" is backwards compatible with Version "1.0":


    encode-1.1(e₀) = e₁
    ─────────────────────────────────────────
    encodeWithVersion-1.0(e₀) = [ "1.0", e₁ ]


    decode-1.1(e₀) = e₁
    ─────────────────────────────────────────
    decodeWithVersion-1.0([ "1.0", e₁ ]) = e₀


## Protocol evolution

The standardization process is fallible and this section addresses how to
mitigate the following types of protocol specification errors:

*   A new protocol version string misassigns the major or minor number

    In theory, no action is required.  Standards compliant decoders are
    unaffected if they match on the exact protocol version string and do not
    attempt to interpret the protocol version string.  The only harm is that the
    incorrect protocol version number is misleading to humans by incorrectly
    suggesting the presence or absence of a breaking change.

    In practice, you SHOULD publish a new release of the standard correcting the
    version number and permanently removing standardized support for encoding or
    decoding the incorrect version number (i.e. "blacklisting" the number).

*   There is a specification bug in the encoding logic

    A protocol version that incorrectly specifies how to encode expressions
    SHOULD be blacklisted and the fixed encoding logic SHOULD be released under
    a new protocol version.  This is because there is no way to fix all possible
    encoded data once the encoding bug has been published.

*   New encoding logic is published without changing the version number

    Treat this the same as a specification bug in the encoding logic.  Blacklist
    the old protocol version string and release the same logic under a new
    protocol version string.

*   There is a specification bug in the decoding logic

    Decoding bugs SHOULD be fixed by publishing a new verson of the standard
    fixing the error.  However, the corresponding protocol version string does
    not need to be blacklisted nor changed since decoding logic does not
    persist any data.

*   The decoding logic is changed without changing the version number at all

    Treat this the same as a specification bug in the decoding logic.  Publish
    a new release of the standard fixing the version number that the logic
    decodes, but there is no need to blacklist the old protocol version string
    since no invalid data was persisted.
