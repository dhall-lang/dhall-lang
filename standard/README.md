# Semantics

This document formalizes the semantics for resolving, type-checking and
normalizing Dhall expressions.

Note that this document *does not* specify how a language binding marshals an
normalized Dhall expression into a matching expression in the host language.
The details of how to do so are left open to each implementation, including
supported integer ranges or how to idiomatically encode unions.

Some of these standard documents are literate Haskell code showing how to
translate the natural deduction notation to executable code.  You can build the
Haskell code by running (in this directory):

```bash
$ nix-shell  # Optional, if you want to use the exact same environment as CI
$ cabal build
```

Note that the literate Haskell code is written in such a way as to match the
natural deduction notation as closely as possible, even if this makes the code
more verbose.  For example, the following standard judgment:


    t₀ ↦ t₁   T₀ ↦ T₁
    ─────────────────
    t₀ : T₀ ↦ t₁ : T₁


… is translated to the following Haskell code:

```haskell
alphaNormalize (Annotation t₀ _T₀) = Annotation t₁ _T₁
  where
    t₁ = alphaNormalize t₀

    _T₁ = alphaNormalize _T₀
```

… even if that Haskell code could have been written more directly as:

```haskell
alphaNormalize (Annotation t₀ _T₀) =
    Annotation (alphaNormalize t₀) (alphaNormalize _T₀)
```

However, you can freely simplify things when actually implementing Dhall.

## Table of contents

* [Summary](#summary)
* [Expressions](#expressions)
* [Notation for induction](#notation-for-induction)
* [Shift](#shift)
* [Contexts](#contexts)
* [Shift context](#shift-context)
* [Substitution](#substitution)
* [α-normalization](#α-normalization)
* [β-normalization](#β-normalization)
* [Equivalence](#equivalence)
* [Function check](#function-check)
* [Type inference](#type-inference)
* [Binary encoding and decoding](#binary-encoding-and-decoding)
* [Import resolution](#import-resolution)

## Summary

Dhall's type system is a variation on [CCω][ccw], implemented using a pure type
system (see the ["Function check"](#function-check) section for more details).
Type abstraction and type application are explicit and not inferred.  Dhall also
supports additional built-in functions, operators, and constants for efficiency.

Dhall also supports referencing shadowed variables through the use of De Bruijn
indices.  This document spells out in detail how to implement these
De Bruijn-like variable references.

## Expressions

The syntax of allowed Dhall expressions is specified in
[`./dhall.abnf`](./dhall.abnf), and you can find the correspondence between the
grammar and the semantics in the following separate document:

* [Syntax](./syntax.md)

## Notation for induction

This document uses a non-standard `…` notation for distinguishing list elements
or key-value pairs in records/unions from other expression types,
records, and unions:

```
t          : Naked label which could be any type of expression.

[ ts…  ]   : A list with 0 or more values.
[ t, ts… ] : A list with 1 or more values.  The first value is `t`.

{ xs… }        : A record type or record value with 0 or more fields.
{ x : T, xs… } : A record type with 1 or more field-type pairs.  At least one
                 field is named `x` with a type of `T`.
{ x = t, xs… } : A record value with 1 or more field-value pairs.  At least one
                 field is named `x` with a value of `t`.

< xs… >                    : A union type with 0 or more alternative-type pairs.
< x : T | xs… >            : A union type with 1 or more alternative-type pairs.
                             At least one alternative is named `x` with a type
                             of `T`.
< x | xs… >                : A union type with 1 or more empty alternatives
                             At least one alternative is named `x`, which is an
                             empty alternative

"s"           : A `Text` literal without any interpolated expressions
"s${t}ss…"    : A `Text` literal with at least one interpolated expression

''
s''           : A multi-line `Text` literal with only one line

''
ss
s''           : A multi-ine `Text` literal with more than one line

0x"0123456789abcdef" : A base16-encoded `Bytes` literal (See RFC4648 - Section 8)
```

You will see this notation in judgments that perform induction on lists,
records, or unions.  For example, the following judgment for normalizing a
non-empty list says that to normalize a list you normalize the head of the list
and then normalize the tail:


    t₀ ⇥ t₁   [ ts₀… ] ⇥ [ ts₁… ]
    ─────────────────────────────
    [ t₀, ts₀… ] ⇥ [ t₁, ts₁… ]


Note that this notation does not imply that implementations must use induction
or inductive data structures (like linked lists) to implement lists, records, or
unions.  Implementations may freely use more efficient data structures like
arrays or dictionaries, so long as they behave the same.

## Record keys

We use the `keys` operator to define rules based on the keys of record types or
literals, for example:

    keys({ x : Bool, y : Natural }) = x, y

    keys({=}) = ε

where `ε` represents an empty sequence.

More formally, we define


    s ⇥ {}
    ───────────
    keys(s) = ε


    s ⇥ { x : T, ss… }
    keys(ss…) = ss₁…
    ──────────────────
    keys(s) = x, ss₁…


for record types and analogously for record literals.

## Multi-line string literals

Dhall's grammar supports multi-line string literals, such as:

    ''
    foo
    bar
    ''

These multi-line string literals are syntactic sugar for ordinary double-quoted
string literals and the conversion from multi-line string literals double-quoted
string literals occurs at parse time.

For example, the above multi-line string literal is parsed as:

    "foo\nbar\n"

Because this conversion occurs at parse-time all of the following judgments
only deal with double-quoted string literals.  Consequently, there are no
separate rules for type-checking or normalizing multi-line string literals.

The logic for desugaring multi-line string literals to
double-quoted string literals is implemented in a judgment:

    to-double-quotes(s₀) = s₁

You can find the details of this judgment in the following separate document:

* [Multi-line literal semantics](./multiline.md)

## Record syntactic sugar

Dhall supports syntactic sugar for records that also occurs at parse time.

For example, a record literal of the form:

    { x.y = 1, x.z = 1 } }

... first desugars dotted fields to nested records:

    { x = { y = 1 }, x = { z = 1 } }

... and then desugars duplicate fields by merging them using `∧`:

    { x = { y = 1 } ∧ { z = 1} }

Because this conversion occurs at parse-time all of the following judgments
only deal with records that have no dotted fields and that have unique keys.

Desugaring record literals with dotted fields is implemented by the following
judgment:

    desugar-dotted-fields(r₀) = r₁

... and desugaring record literals with duplicate fields is handled by the
following judgment:

    desugar-duplicates(r₀) = r₁

You can find the details of these judgments in the following separate document:

* [Record syntactic sugar](./record.md)

## Shift

Dhall allows variables to reference shadowed variables of the same name using De
Bruijn indices.  For example:


                                  ┌──refers to──┐
                                  │             │
                                  ↓             │
    λ(x : Type) → λ(y : Type) → λ(x : Type) → x@0


      ┌────────────────refers to────────────────┐
      │                                         │
      ↓                                         │
    λ(x : Type) → λ(y : Type) → λ(x : Type) → x@1


`x@n` refers to the "nth" bound variable named `x` counting outwards from where
the variable is referenced.

If a variable does not specify the De Bruijn index (i.e. just `x`) then the De
Bruijn index defaults to 0 (i.e. `x@0`), like this:


                                  ┌─refers to─┐
                                  │           │
                                  ↓           │
    λ(x : Type) → λ(y : Type) → λ(x : Type) → x


Dhall uses a shift function internally to avoid variable capture in the
implementation of De Bruijn indices.

This shift function has the form:

    ↑(d, x, m, e₀) = e₁

You can find the details of this judgment in the following separate document:

* [Shift](./shift.md)


## Contexts

The syntax of contexts is:

    Γ = ε         ; The empty context
      / Γ, x : T  ; A context extended with a type annotation for a variable

Contexts are ordered and there can be multiple type annotations in the context
for the same variable.  The De Bruijn index associated with each variable
disambiguates which variable to refer to in the context.

## Shift context

You can also shift a context by shifting each expression in that context:


    ─────────────────
    ↑(d, x, m, ε) = ε


    ↑(d, x, m, Γ₀) = Γ₁   ↑(d, x, m, T₀) = T₁
    ─────────────────────────────────────────
    ↑(d, x, m, (Γ₀, y : T₀)) = Γ₁, y : T₁


## Substitution

β-reduction requires support for substitution, which consists in replacing
the uses of a given variable in an expression by another expression.

Substitution has the form:

    e₀[x@n ≔ a] = e₁

You can find the details of this judgment in the following separate document:

* [Substitution](./substitution.md)


## α-normalization

α-normalization renames all bound variables within an expression to use De
Bruijn indices.

α-normalization has the form:

    t₀ ↦ t₁

You can find the details of this judgment in the following separate document:

* [α-normalization](./alpha-normalization.md)


## β-normalization

β-normalization transforms a Dhall expression to an expression called its
normal form. This is similar to "executing" the program represented by the
given Dhall expression.

β-normalization has the form:

    t₀ ⇥ t₁

You can find the details of this judgment in the following separate document:

* [β-normalization](./beta-normalization.md)


## Equivalence

Equivalence captures what it means for two Dhall expressions to be "the same".
It is used for type inference and β-normalization.

Equivalence is a relationship between two expression of the form:


    l ≡ r


You can find the details of this judgment in the following separate document:

* [Equivalence](./equivalence.md)

## Function check

The function check governs the types of functions that our pure type system
permits.

The function check is a judgment of the form:

    c₀ ↝ c₁ : c₂

You can find the details of this judgment in the following separate document:

* [Function check](./function-check.md)


## Type inference

Type inference is a judgment of the form:

    Γ ⊢ t : T

You can find the details of this judgment in the following separate document:

* [Type inference](./type-inference.md)


## Binary encoding and decoding

Dhall supports encoding and decoding expressions to and from a binary format.

Binary encoding and decoding is captured by two judgments:

    encode(dhall) = cbor
    decode(cbor) = dhall

You can find the details of these judgments in the following separate document:

* [Binary semantics](./binary.md)


## Import resolution

Import resolution is captured by a judgment:

    (Δ, here) × Γ₀ ⊢ e₀ ⇒ e₁ ⊢ Γ₁

You can find the details of this judgment in the following separate document:

* [Import resolution semantics](./imports.md)

[ccw]: https://hal.inria.fr/hal-01445835
