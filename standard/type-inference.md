# Type inference

Type inference is a judgment of the form:

    Γ ⊢ t : T

... where:

* `Γ` (an input context) is the type inference context which relates
  identifiers to their types
* `t` (an input expression) is the term to infer the type of
* `T` (the output expression) is the inferred type

Type inference also type-checks the input expression, too, to ensure that it is
well-typed.

To infer the type of a closed expression, supply an empty context:

    ε ⊢ t : T

This judgment guarantees the invariant that the inferred type is safe to
normalize.


## Table of contents

* [Reduction](#reduction)
* [Constants](#constants)
* [Variables](#variables)
* [`Bool`](#bool)
* [`Natural`](#natural)
* [`Text`](#text)
* [`List`](#list)
* [`Optional`](#optional)
* [Records](#records)
* [Unions](#unions)
* [`Integer`](#integer)
* [`Double`](#double)
* [Functions](#functions)
* [`let` expressions](#let-expressions)
* [Type annotations](#type-annotations)
* [Imports](#imports)

## Reduction

Additionally, there is a separate helper judgment for inferring a type reduced
to normal form:


    Γ ⊢ a : A₀   A₀ ⇥ A₁
    ────────────────────
    Γ ⊢ a :⇥ A₁


This judgment is identical to the judgment for type inference except that this
judgment returns the inferred type in normal form.

## Constants

The first rules are that the inferred type of `Type` is `Kind` and the inferred
type of `Kind` is `Sort`:


    ───────────────
    Γ ⊢ Type : Kind


    ───────────────
    Γ ⊢ Kind : Sort


In other words, `Kind` is the "type of types" and `Sort` serves as the
foundation of the type system.

Note that you cannot infer the type of `Sort` as there is nothing above `Sort`
in the type system's hierarchy.  Inferring the type of `Sort` is a type error.

## Variables

Infer the type of a variable by looking up the variable's type in the context:


    Γ ⊢ T : k
    ──────────────────
    Γ, x : T ⊢ x@0 : T


Since `x` is a synonym for `x@0`, you can shorten this rule to:


    Γ ⊢ T : k
    ──────────────────
    Γ, x : T ⊢ x : T


The order of types in the context matters because there can be multiple type
annotations in the context for the same variable.  The DeBruijn index associated
with each variable disambiguates which type annotation in the context to use:


    Γ ⊢ x@n : T
    ────────────────────────  ; 0 < n
    Γ, x : A ⊢ x@(1 + n) : T


    Γ ⊢ x@n : T
    ───────────────────────  ; x ≠ y
    Γ, y : A ⊢ x@n : T


If the natural number associated with the variable is greater than or equal to
the number of type annotations in the context matching the variable then that is
a type error.

## `Bool`

`Bool` is a `Type`:


    ───────────────
    Γ ⊢ Bool : Type


`True` and `False` have type `Bool`:


    ───────────────
    Γ ⊢ True : Bool


    ────────────────
    Γ ⊢ False : Bool


An `if` expression takes a predicate of type `Bool` and returns either the
`then` or `else` branch of the expression, both of which must be the same type:


    Γ ⊢ t :⇥ Bool
    Γ ⊢ l : L
    Γ ⊢ r : R
    Γ ⊢ L :⇥ Type
    Γ ⊢ R :⇥ Type
    L ≡ R
    ──────────────────────────
    Γ ⊢ if t then l else r : L


Note that an `if` expression can only return a term.  More generally, if the
`if` expression returns a value whose type is not a `Type` then that is a type
error.

If the predicate is not a `Bool` then that is a type error.

If the two branches of the `if` expression do not have the same type then that
is a type error.

All of the logical operators take arguments of type `Bool` and return a result
of type `Bool`:


    Γ ⊢ l :⇥ Bool   Γ ⊢ r :⇥ Bool
    ───────────────────────────
    Γ ⊢ l || r : Bool


    Γ ⊢ l :⇥ Bool   Γ ⊢ r :⇥ Bool
    ───────────────────────────
    Γ ⊢ l && r : Bool


    Γ ⊢ l :⇥ Bool   Γ ⊢ r :⇥ Bool
    ───────────────────────────
    Γ ⊢ l == r : Bool


    Γ ⊢ l :⇥ Bool   Γ ⊢ r :⇥ Bool
    ───────────────────────────
    Γ ⊢ l != r : Bool


If the operator arguments do not have type `Bool` then that is a type error.

## `Natural`

`Natural` is a type:


    ──────────────────
    Γ ⊢ Natural : Type


`Natural` number literals have type `Natural`:


    ───────────────
    Γ ⊢ n : Natural


The arithmetic operators take arguments of type `Natural` and return a result of
type `Natural`:


    Γ ⊢ x :⇥ Natural   Γ ⊢ y :⇥ Natural
    ─────────────────────────────────
    Γ ⊢ x + y : Natural


    Γ ⊢ x :⇥ Natural   Γ ⊢ y :⇥ Natural
    ─────────────────────────────────
    Γ ⊢ x * y : Natural


If the operator arguments do not have type `Natural` then that is a type error.

The built-in functions on `Natural` numbers have the following types:


    ─────────────────────────────────────────────────────────────────────────────────────────────────────────────
    Γ ⊢ Natural/build : (∀(natural : Type) → ∀(succ : natural → natural) → ∀(zero : natural) → natural) → Natural


    ──────────────────────────────────────────────────────────────────────────────────────────────────────────
    Γ ⊢ Natural/fold : Natural → ∀(natural : Type) → ∀(succ : natural → natural) → ∀(zero : natural) → natural


    ───────────────────────────────────
    Γ ⊢ Natural/isZero : Natural → Bool


    ─────────────────────────────────
    Γ ⊢ Natural/even : Natural → Bool


    ────────────────────────────────
    Γ ⊢ Natural/odd : Natural → Bool


    ─────────────────────────────────────────
    Γ ⊢ Natural/toInteger : Natural → Integer


    ─────────────────────────────────
    Γ ⊢ Natural/show : Natural → Text


## `Text`


`Text` is a type:


    ───────────────
    Γ ⊢ Text : Type


`Text` literals have type `Text`:


    ──────────────
    Γ ⊢ "s" : Text


    Γ ⊢ t : Text   Γ ⊢ "ss…" : Text
    ───────────────────────────────
    Γ ⊢ "s${t}ss…" : Text


The `Text` show function has the following type:


    ───────────────────────────
    Γ ⊢ Text/show : Text → Text


The `Text` concatenation operator takes arguments of type `Text` and returns a
result of type `Text`:


    Γ ⊢ x :⇥ Text   Γ ⊢ y :⇥ Text
    ─────────────────────────────
    Γ ⊢ x ++ y : Text


If the operator arguments do not have type `Text`, then that is a type error.

## `List`

`List` is a function from a `Type` to another `Type`:


    ──────────────────────
    Γ ⊢ List : Type → Type


A `List` literal's type is inferred either from the type of the elements (if
non-empty) or from the type annotation (if empty):


    Γ ⊢ T :⇥ Type
    ──────────────────────────
    Γ ⊢ ([] : List T) : List T


    Γ ⊢ t : T₀   T₀ :⇥ Type   Γ ⊢ [ ts… ] :⇥ List T₁   T₀ ≡ T₁
    ──────────────────────────────────────────────────────────
    Γ ⊢ [ t, ts… ] : List T₀


Note that the above rules forbid `List` elements that are `Type`s.  More
generally, if the element type is not a `Type` then that is a type error.

If the list elements do not all have the same type then that is a type error.

If an empty list does not have a type annotation then that is a type error.

The `List` concatenation operator takes arguments that are both `List`s of the
same type and returns a `List` of the same type:


    Γ ⊢ x :⇥ List A₀
    Γ ⊢ y :⇥ List A₁
    A₀ ≡ A₁
    ───────────────────
    Γ ⊢ x # y : List A₀


If the operator arguments are not `List`s, then that is a type error.

If the arguments have different element types, then that is a type error.

The built-in functions on `List`s have the following types:


    ───────────────────────────────────────────────────────────────────────────────────────────────────────────
    Γ ⊢ List/build : ∀(a : Type) → (∀(list : Type) → ∀(cons : a → list → list) → ∀(nil : list) → list) → List a


    ────────────────────────────────────────────────────────────────────────────────────────────────────────
    Γ ⊢ List/fold : ∀(a : Type) → List a → ∀(list : Type) → ∀(cons : a → list → list) → ∀(nil : list) → list


    ────────────────────────────────────────────────
    Γ ⊢ List/length : ∀(a : Type) → List a → Natural


    ─────────────────────────────────────────────────
    Γ ⊢ List/head : ∀(a : Type) → List a → Optional a


    ─────────────────────────────────────────────────
    Γ ⊢ List/last : ∀(a : Type) → List a → Optional a


    ─────────────────────────────────────────────────────────────────────────────
    Γ ⊢ List/indexed : ∀(a : Type) → List a → List { index : Natural, value : a }


    ────────────────────────────────────────────────
    Γ ⊢ List/reverse : ∀(a : Type) → List a → List a


## `Optional`

`Optional` is a function from a `Type` to another `Type`:


    ──────────────────────────
    Γ ⊢ Optional : Type → Type


The `Some` constructor infers the type from the provided argument:


    Γ ⊢ a : A   Γ ⊢ A : Type
    ────────────────────────
    Γ ⊢ Some a : Optional A


... and the `None` constructor is an ordinary function that is typeable in
isolation:


    ───────────────────────────────────
    Γ ⊢ None : ∀(A : Type) → Optional A


Note that the above rules forbid an `Optional` element that is a `Type`.  More
generally, if the element type is not a `Type` then that is a type error.

The built-in functions on `Optional` values have the following types:


    ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    Γ ⊢ Optional/fold : ∀(a : Type) → Optional a → ∀(optional : Type) → ∀(just : a → optional) → ∀(nothing : optional) → optional


    ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    Γ ⊢ Optional/build : ∀(a : Type) → (∀(optional : Type) → ∀(just : a → optional) → ∀(nothing : optional) → optional) → Optional a


## Records

Record types are "anonymous", meaning that they are uniquely defined by the
names and types of their fields.

A record can either store term-level values and functions:


    ─────────────
    Γ ⊢ {} : Type


    Γ ⊢ T :⇥ Type   Γ ⊢ { xs… } :⇥ Type
    ───────────────────────────────────  ; x ∉ { xs… }
    Γ ⊢ { x : T, xs… } : Type


... or store types (if it is non-empty):


    Γ ⊢ T :⇥ Kind
    ────────────────────
    Γ ⊢ { x : T } : Kind


    Γ ⊢ T :⇥ Kind   Γ ⊢ { xs… } :⇥ Kind
    ───────────────────────────────────  ; x ∉ { xs… }
    Γ ⊢ { x : T, xs… } : Kind


... or store kinds (if it is non-empty):


    Γ ⊢ T :⇥ Sort
    ────────────────────
    Γ ⊢ { x : T } : Sort


    Γ ⊢ T :⇥ Sort   Γ ⊢ { xs… } :⇥ Sort
    ───────────────────────────────────  ; x ∉ { xs… }
    Γ ⊢ { x : T, xs… } : Sort


... but they can not be mixed.  If one field is a term-level value or function
and another field is a type-level value or function then that is a type error.

If the type of a field is not `Type`, `Kind`, or `Sort` then that is a type
error.

If there are duplicated fields (that is, if two fields have the same name),
then that is a type error.

Record values are also anonymous:


    ────────────
    Γ ⊢ {=} : {}


    Γ ⊢ t : T   Γ ⊢ { xs… } :⇥ { ts… }   Γ ⊢ { x : T, ts… } : i
    ───────────────────────────────────────────────────────────  ; x ∉ { xs… }
    Γ ⊢ { x = t, xs… } : { x : T, ts… }


You can only select field(s) from the record if they are present:


    Γ ⊢ e :⇥ { x : T, xs… }
    ───────────────────────
    Γ ⊢ e.x : T


    Γ ⊢ e :⇥ { ts… }   Γ ⊢ { ts… } :⇥ Type
    ──────────────────────────────────────
    Γ ⊢ e.{} : {}


    Γ ⊢ e :⇥ { x : T, ts… }   Γ ⊢ { x : T, ts… } :⇥ Kind
    ────────────────────────────────────────────────────
    Γ ⊢ e.{ x } : { x : T }


    Γ ⊢ e :⇥ { x : T, ts… }   Γ ⊢ { x : T, ts… } :⇥ Sort
    ────────────────────────────────────────────────────
    Γ ⊢ e.{ x } : { x : T }


    Γ ⊢ e :⇥ { x : T, ts₀… }   Γ ⊢ e.{ xs… } :⇥ { ts₁… }
    ────────────────────────────────────────────────────  ; x ∉ { xs… }
    Γ ⊢ e.{ x, xs… } : { x : T, ts₁… }


Record projection can also be done by specifying the target record type.
For instance, provided that `s` is a record type and `e` is the source record,
`e.(s)` produces another record of type `s` whose values are taken from the
respective fields from `e`.


    Γ ⊢ e :⇥ { ts… }
    Γ ⊢ s : c
    s ⇥ {}
    ────────────────
    Γ ⊢ e.(s) : {}


    Γ ⊢ e :⇥ { x : T₀, ts… }
    Γ ⊢ s : c
    s ⇥ { x : T₁, ss… }
    T₀ ≡ T₁
    Γ ⊢ e.({ ss… }) : U
    ───────────────────────────
    Γ ⊢ e.(s) : { x : T₁, ss… }


If you select a field from a value that is not a record, then that is a type
error.

If the field is absent from the record then that is a type error.

Recursive record merge requires that both arguments are records of terms,
records of types, or records of kinds:


    Γ ⊢ l :⇥ { ls… }
    { ls… } :⇥ Type
    Γ ⊢ r :⇥ {}
    ───────────────────
    Γ ⊢ l ∧ r : { ls… }


    Γ ⊢ l :⇥ { ls… }
    { ls… } :⇥ Type
    Γ ⊢ r :⇥ { a : A, rs… }
    Γ ⊢ { a : A, rs… } :⇥ Type
    Γ ⊢ { ls… } ∧ { rs… } :⇥ { ts… }
    ────────────────────────────────  ; a ∉ ls
    Γ ⊢ l ∧ r : { a : A, ts… }


    Γ ⊢ l :⇥ { a : A₀, ls… }
    Γ ⊢ { a : A₀, ls… } :⇥ Type
    Γ ⊢ r :⇥ { a : A₁, rs… }
    Γ ⊢ { a : A₁, rs… } :⇥ Type
    Γ ⊢ l.a ∧ r.a : A₂
    Γ ⊢ { ls… } ∧ { rs… } :⇥ { ts… }
    ────────────────────────────────
    Γ ⊢ l ∧ r : { a : A₂, ts… }


    Γ ⊢ l :⇥ { ls… }
    { ls… } :⇥ Kind
    Γ ⊢ r :⇥ { a : A, rs… }
    Γ ⊢ { a : A, rs… } :⇥ Kind
    Γ ⊢ { ls… } ∧ { rs… } :⇥ { ts… }
    ────────────────────────────────  ; a ∉ ls
    Γ ⊢ l ∧ r : { a : A, ts… }


    Γ ⊢ l :⇥ { a : A₀, ls… }
    Γ ⊢ { a : A₀, ls… } :⇥ Kind
    Γ ⊢ r :⇥ { a : A₁, rs… }
    Γ ⊢ { a : A₁, rs… } :⇥ Kind
    Γ ⊢ l.a ∧ r.a : A₂
    Γ ⊢ { ls… } ∧ { rs… } :⇥ { ts… }
    ────────────────────────────────
    Γ ⊢ l ∧ r : { a : A₂, ts… }


    Γ ⊢ l :⇥ { ls… }
    { ls… } :⇥ Sort
    Γ ⊢ r :⇥ { a : A, rs… }
    Γ ⊢ { a : A, rs… } :⇥ Sort
    Γ ⊢ { ls… } ∧ { rs… } :⇥ { ts… }
    ────────────────────────────────  ; a ∉ ls
    Γ ⊢ l ∧ r : { a : A, ts… }


    Γ ⊢ l :⇥ { a : A₀, ls… }
    Γ ⊢ { a : A₀, ls… } :⇥ Sort
    Γ ⊢ r :⇥ { a : A₁, rs… }
    Γ ⊢ { a : A₁, rs… } :⇥ Sort
    Γ ⊢ l.a ∧ r.a : A₂
    Γ ⊢ { ls… } ∧ { rs… } :⇥ { ts… }
    ────────────────────────────────
    Γ ⊢ l ∧ r : { a : A₂, ts… }


If the operator arguments are not records then that is a type error.

If they share a field in common that is not a record then that is a type error.

If one argument is a record of terms and the other argument is a record of types
then that is a type error.

Non-recursive right-biased merge also requires that both arguments are both
records of terms or records of types:


    Γ ⊢ l :⇥ { ls… }
    Γ ⊢ { ls… } :⇥ Type
    Γ ⊢ r :⇥ {}
    ───────────────────
    Γ ⊢ l ⫽ r : { ls… }


    Γ ⊢ l :⇥ { ls… }
    Γ ⊢ { ls… } :⇥ Type
    Γ ⊢ r :⇥ { a : A, rs… }
    Γ ⊢ { a : A, rs… } :⇥ Type
    Γ ⊢ { ls… } ⫽ { rs… } :⇥ { ts… }
    ────────────────────────────────  ; a ∉ ls
    Γ ⊢ l ⫽ r : { a : A, ts… }


    Γ ⊢ l :⇥ { a : A₀, ls… }
    Γ ⊢ { a : A₀, ls… } :⇥ Type
    Γ ⊢ r :⇥ { a : A₁, rs… }
    Γ ⊢ { a : A₁, rs… } :⇥ Type
    Γ ⊢ { ls… } ⫽ { rs… } :⇥ { ts… }
    ───────────────────────────────
    Γ ⊢ l ⫽ r : { a : A₁, ts… }


    Γ ⊢ l :⇥ { ls… }
    Γ ⊢ { ls… } :⇥ Kind
    Γ ⊢ r :⇥ { a : A, rs… }
    Γ ⊢ { a : A, rs… } :⇥ Kind
    Γ ⊢ { ls… } ⫽ { rs… } :⇥ { ts… }
    ────────────────────────────────  ; a ∉ ls
    Γ ⊢ l ⫽ r : { a : A, ts… }


    Γ ⊢ l :⇥ { a : A₀, ls… }
    Γ ⊢ { a : A₀, ls… } :⇥ Kind
    Γ ⊢ r :⇥ { a : A₁, rs… }
    Γ ⊢ { a : A₁, rs… } :⇥ Kind
    Γ ⊢ { ls… } ⫽ { rs… } :⇥ { ts… }
    ───────────────────────────────
    Γ ⊢ l ⫽ r : { a : A₁, ts… }


    Γ ⊢ l :⇥ { ls… }
    Γ ⊢ { ls… } :⇥ Sort
    Γ ⊢ r :⇥ { a : A, rs… }
    Γ ⊢ { a : A, rs… } :⇥ Sort
    Γ ⊢ { ls… } ⫽ { rs… } :⇥ { ts… }
    ────────────────────────────────  ; a ∉ ls
    Γ ⊢ l ⫽ r : { a : A, ts… }


    Γ ⊢ l :⇥ { a : A₀, ls… }
    Γ ⊢ { a : A₀, ls… } :⇥ Sort
    Γ ⊢ r :⇥ { a : A₁, rs… }
    Γ ⊢ { a : A₁, rs… } :⇥ Sort
    Γ ⊢ { ls… } ⫽ { rs… } :⇥ { ts… }
    ───────────────────────────────
    Γ ⊢ l ⫽ r : { a : A₁, ts… }


If the operator arguments are not records then that is a type error.

If one argument is a record of terms and the other argument is a record of types
then that is a type error.

Recursive record type merge requires that both arguments are record type
literals.  Any conflicting fields must be safe to recursively merge:


    Γ ⊢ l :⇥ Type
    l ⇥ { ls… }
    Γ ⊢ r :⇥ Type
    r ⇥ {}
    ────────────────
    Γ ⊢ l ⩓ r : Type


    Γ ⊢ l :⇥ Type
    l ⇥ { ls… }
    Γ ⊢ r :⇥ Type
    r ⇥ { a : A, rs… }
    Γ ⊢ { ls… } ⩓ { rs… } : T
    ─────────────────────────────  ; a ∉ ls
    Γ ⊢ l ⩓ r : Type


    Γ ⊢ l :⇥ Type
    l ⇥ { a : A₀, ls… }
    Γ ⊢ r :⇥ Type
    r ⇥ { a : A₁, rs… }
    Γ ⊢ l.a ⩓ r.a : T₀
    Γ ⊢ { ls… } ⩓ { rs… } : T₁
    ─────────────────────────────
    Γ ⊢ l ⩓ r : Type


    Γ ⊢ l :⇥ Kind
    l ⇥ { ls… }
    Γ ⊢ r :⇥ Kind
    r ⇥ { a : A }
    ────────────────
    Γ ⊢ l ⩓ r : Kind


    Γ ⊢ l :⇥ Kind
    l ⇥ { ls… }
    Γ ⊢ r :⇥ Kind
    r ⇥ { a : A, rs… }
    Γ ⊢ { ls… } ⩓ { rs… } : T
    ─────────────────────────────  ; a ∉ ls
    Γ ⊢ l ⩓ r : Kind


    Γ ⊢ l :⇥ Kind
    l ⇥ { a : A₀, ls… }
    Γ ⊢ r :⇥ Kind
    r ⇥ { a : A₁, rs… }
    Γ ⊢ l.a ⩓ r.a : T₀
    Γ ⊢ { ls… } ⩓ { rs… } : T₁
    ─────────────────────────────
    Γ ⊢ l ⩓ r : Kind


    Γ ⊢ l :⇥ Sort
    l ⇥ { ls… }
    Γ ⊢ r :⇥ Sort
    r ⇥ { a : A }
    ────────────────
    Γ ⊢ l ⩓ r : Sort


    Γ ⊢ l :⇥ Sort
    l ⇥ { ls… }
    Γ ⊢ r :⇥ Sort
    r ⇥ { a : A, rs… }
    Γ ⊢ { ls… } ⩓ { rs… } : T
    ─────────────────────────────  ; a ∉ ls
    Γ ⊢ l ⩓ r : Sort


    Γ ⊢ l :⇥ Sort
    l ⇥ { a : A₀, ls… }
    Γ ⊢ r :⇥ Sort
    r ⇥ { a : A₁, rs… }
    Γ ⊢ l.a ⩓ r.a : T₀
    Γ ⊢ { ls… } ⩓ { rs… } : T₁
    ─────────────────────────────
    Γ ⊢ l ⩓ r : Sort


If the operator arguments are not record types then that is a type error.

If they share a field in common that is not a record type then that is a type
error.

## Unions

Union types are "anonymous", meaning that they are uniquely defined by the names
and types of their alternatives.

A union can have alternatives of term-level values and functions:


    ─────────────
    Γ ⊢ <> : Type


    Γ ⊢ T :⇥ Type   Γ ⊢ < ts… > :⇥ Type
    ───────────────────────────────────  ; x ∉ < ts… >
    Γ ⊢ < x : T | ts… > : Type


... or alternatives of types (if it is non-empty):


    Γ ⊢ T :⇥ Kind
    ────────────────────
    Γ ⊢ < x : T > : Kind


    Γ ⊢ T :⇥ Kind   Γ ⊢ < ts… > :⇥ Kind
    ───────────────────────────────────  ; x ∉ < ts… >
    Γ ⊢ < x : T | ts… > : Kind


... or alternatives of kinds (if it is non-empty):


    Γ ⊢ T :⇥ Sort
    ────────────────────
    Γ ⊢ < x : T > : Sort


    Γ ⊢ T :⇥ Sort   Γ ⊢ < ts… > :⇥ Sort
    ───────────────────────────────────  ; x ∉ < ts… >
    Γ ⊢ < x : T | ts… > : Sort


A union type may contain alternatives without an explicit type label:


    Γ ⊢ < ts… > :⇥ c
    ───────────────────  ; x ∉ < ts… >
    Γ ⊢ < x | ts… > : c


Note that the above rule allows storing values, types, and kinds in
unions.  However, if the type of the alternative is not `Type`,
`Kind`, or `Sort` then that is a type error.

If two alternatives share the same name then that is a type error.

Union literals are also anonymous:


    Γ ⊢ t : T   Γ ⊢ < x : T | ts… > :⇥ i
    ─────────────────────────────────────
    Γ ⊢ < x = t | ts… > : < x : T | ts… >


However, union literals are deprecated in favor of union constructors.

If a union alternative is non-empty, then the corresponding constructor is a
function that wraps a value of the appropriate type:


    Γ ⊢ u : c   u ⇥ < x : T | ts… >
    ────────────────────────────────────
    Γ ⊢ u.x : ∀(x : T) → < x : T | ts… >


If a union alternative is empty, then the corresponding constructor's type is
the same as the original union type:


    Γ ⊢ u : c   u ⇥ < x | ts… >
    ───────────────────────────
    Γ ⊢ u.x : < x | ts… >


A `merge` expression is well-typed if there is a one-to-one correspondence
between the fields of the handler record and the alternatives of the union:


    Γ ⊢ t :⇥ {}   Γ ⊢ u :⇥ <>   Γ ⊢ T :⇥ Type
    ─────────────────────────────────────────
    Γ ⊢ (merge t u : T) : T


    Γ ⊢ t :⇥ { ts… }   Γ ⊢ merge t u : T
    ────────────────────────────────────  ; `ts` non-empty
    Γ ⊢ (merge t u : T) : T


    Γ ⊢ t :⇥ { y : ∀(x : A₀) → T₀, ts… }
    Γ ⊢ u :⇥ < y : A₁ | us… >
    Γ ⊢ (merge { ts… } < us… > : T₁) : T₂
    A₀ ≡ A₁
    ↑(-1, x, 0, T₀) = T₁
    ────────────────────────────────────  ; `x` not free in `T₀`
    Γ ⊢ merge t u : T₀


    Γ ⊢ t :⇥ { y : T₀, ts… }
    Γ ⊢ u :⇥ < y | us… >
    Γ ⊢ (merge { ts… } < us… > : T₀) : T₁
    ─────────────────────────────────────
    Γ ⊢ merge t u : T₀


If the first argument of a `merge` expression is not a record then that is a
type error.

If the second argument of a `merge` expression is not a union then that is a
type error.

If you `merge` an empty union without a type annotation then that is a type
error.

If the `merge` expression has a type annotation that is not a `Type` then that
is a type error.

If there is a handler without a matching alternative then that is a type error.

If there is an alternative without a matching handler then that is a type error.

If a handler is not a function and the corresponding union alternative is
non-empty, then that is a type error.

If the handler's input type does not match the corresponding alternative's type
then that is a type error.

If there are two handlers with different output types then that is a type error.

If a `merge` expression has a type annotation that doesn't match every handler's
output type then that is a type error.

## `Integer`

`Integer` is a type:


    ──────────────────
    Γ ⊢ Integer : Type


`Integer` literals have type `Integer`:


    ────────────────
    Γ ⊢ ±n : Integer


The built-in functions on `Integer` have the following types:


    ─────────────────────────────────
    Γ ⊢ Integer/show : Integer → Text


    ───────────────────────────────────────
    Γ ⊢ Integer/toDouble : Integer → Double


## `Double`

`Double` is a type:


    ──────────────────
    Γ ⊢ Double : Type


`Double` literals have type `Double`:


    ────────────────
    Γ ⊢ n.n : Double


The built-in `Double/show` function has the following type:


    ───────────────────────────────
    Γ ⊢ Double/show : Double → Text


## Functions

A function type is only well-typed if the input and output type are well-typed
and if the inferred input and output type are allowed by the function check:


    Γ₀ ⊢ A :⇥ i   ↑(1, x, 0, (Γ₀, x : A)) = Γ₁   Γ₁ ⊢ B :⇥ o   i ↝ o : c
    ────────────────────────────────────────────────────────────────────
    Γ₀ ⊢ ∀(x : A) → B : c


If the input or output type is neither a `Type`, a `Kind`, nor a `Sort` then
that is a type error.

The function check disallows dependent function types but allows all other
function types.  If the function type is a dependent function type then that is
a type error.

An unquantified function type `A → B` is a short-hand for `∀(_ : A) → B`.  Note
that the `_` does *not* denote some unused type variable but rather denotes the
specific variable named `_` (which is a valid variable name and this variable
named `_` may in fact be present within `B`).  For example, this is a well-typed
judgment:

    ε ⊢ Type → ∀(x : _) → _ : Type

... because it is equivalent to:

    ε ⊢ ∀(_ : Type) → ∀(x : _) → _ : Type

The type of a λ-expression is a function type whose input type (`A`) is the same
as the type of the bound variable and whose output type (`B`) is the same as the
inferred type of the body of the λ-expression (`b`).


    ↑(1, x, 0, (Γ₀, x : A)) = Γ₁   Γ₁ ⊢ b : B   Γ₀ ⊢ ∀(x : A) → B : c
    ─────────────────────────────────────────────────────────────────
    Γ₀ ⊢ λ(x : A) → b : ∀(x : A) → B


Note that the above rule requires that the inferred function type must be
well-typed.  The type-checking step for the function type triggers a function
check which disallows dependent function types.

The type system ensures that function application is well-typed, meaning that
the input type that a function expects matches the inferred type of the
function's argument:


    Γ ⊢ f :⇥ ∀(x : A₀) → B₀
    Γ ⊢ a₀ : A₁
    A₀ ≡ A₁
    ↑(1, x, 0, a₀) = a₁
    B₀[x ≔ a₁] = B₁
    ↑(-1, x, 0, B₁) = B₂
    ───────────────────────
    Γ ⊢ f a₀ : B₂


If the function does not have a function type, then that is a type error.

If the inferred input type of the function does not match the inferred type of
the function argument then that is a type error.

## `let` expressions

For the purposes of type-checking, an expression of the form:

    let x : A = a₀ in b₀

... is **not** semantically identical to:

    (λ(x : A) → b₀) a₀

`let` differs in behavior in order to support "type synonyms", such as:

    let t : Type = Natural in 1 : t

If you were to desugar that to:

    (λ(t : Type) → 1 : t) Natural

... then that would not be a well-typed expression, even though the `let`
expression would be well-typed.


    Γ ⊢ a₀ : A₁
    Γ ⊢ A₀ : i
    A₀ ≡ A₁
    a₀ ⇥ a₁
    ↑(1, x, 0, a₁) = a₂
    b₀[x ≔ a₂] = b₁
    ↑(-1, x, 0, b₁) = b₂
    Γ ⊢ b₂ : B
    ─────────────────────────────
    Γ ⊢ let x : A₀ = a₀ in b₀ : B


    Γ ⊢ a₀ : A
    a₀ ⇥ a₁
    ↑(1, x, 0, a₁) = a₂
    b₀[x ≔ a₂] = b₁
    ↑(-1, x, 0, b₁) = b₂
    Γ ⊢ b₂ : B
    ────────────────────────
    Γ ⊢ let x = a₀ in b₀ : B


If the `let` expression has a type annotation that doesn't match the type of
the right-hand side of the assignment then that is a type error.


## Type annotations

The inferred type of a type annotation is the annotation.  Type-checking also
verifies that the annotation matches the inferred type of the annotated
expression:


    Γ ⊢ T₀ : i   Γ ⊢ t : T₁   T₀ ≡ T₁
    ─────────────────────────────────
    Γ ⊢ (t : T₀) : T₀


Note that the above rule permits kind annotations, such as `List : Type → Type`.

If the inferred type of the annotated expression does not match the type
annotation then that is a type error.

Even though `Sort` is not a type-valid expression by itself, it is valid
as a type annotation:


    Γ ⊢ t : Sort
    ─────────────────────
    Γ ⊢ (t : Sort) : Sort


## Imports

An expression with unresolved imports cannot be type-checked

