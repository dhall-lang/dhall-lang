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

* [Normalization](#normalization)
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
* [Assertions](#assertions)
* [Imports](#imports)

## Normalization

Types inferred according to the following rules will be in β-normal form, provided that
all types in the input context `Γ` are in β-normal form.
The advantage of β-normalizing all inferred types is that repeated normalization of the
same types can largely be avoided.

However implementations MAY choose to infer types that are not β-normalized as long as
they are equivalent to the types specified here.

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
    ────────────────
    Γ, x : T ⊢ x : T


The order of types in the context matters because there can be multiple type
annotations in the context for the same variable.  The DeBruijn index associated
with each variable disambiguates which type annotation in the context to use:


    Γ ⊢ x@n : T
    ────────────────────────  ; 0 < n
    Γ, x : A ⊢ x@(1 + n) : T


    Γ ⊢ x@n : T
    ──────────────────  ; x ≠ y
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


    Γ ⊢ t : Bool
    Γ ⊢ l : L
    Γ ⊢ r : R
    Γ ⊢ L : Type
    Γ ⊢ R : Type
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


    Γ ⊢ l : Bool   Γ ⊢ r : Bool
    ───────────────────────────
    Γ ⊢ l || r : Bool


    Γ ⊢ l : Bool   Γ ⊢ r : Bool
    ───────────────────────────
    Γ ⊢ l && r : Bool


    Γ ⊢ l : Bool   Γ ⊢ r : Bool
    ───────────────────────────
    Γ ⊢ l == r : Bool


    Γ ⊢ l : Bool   Γ ⊢ r : Bool
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


    Γ ⊢ x : Natural   Γ ⊢ y : Natural
    ─────────────────────────────────
    Γ ⊢ x + y : Natural


    Γ ⊢ x : Natural   Γ ⊢ y : Natural
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


    ──────────────────────────────────────────────────
    Γ ⊢ Natural/subtract : Natural → Natural → Natural


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


    Γ ⊢ x : Text   Γ ⊢ y : Text
    ───────────────────────────
    Γ ⊢ x ++ y : Text


If the operator arguments do not have type `Text`, then that is a type error.

## `List`

`List` is a function from a `Type` to another `Type`:


    ──────────────────────
    Γ ⊢ List : Type → Type


A `List` literal's type is inferred either from the type of the elements (if
non-empty) or from the type annotation (if empty):


    Γ ⊢ T₀ : c   T₀ ⇥ List T₁   Γ ⊢ T₁ : Type
    ─────────────────────────────────────────
    Γ ⊢ ([] : T₀) : List T₁


    Γ ⊢ t : T₀   Γ ⊢ T₀ : Type   Γ ⊢ [ ts… ] : List T₁   T₀ ≡ T₁
    ────────────────────────────────────────────────────────────
    Γ ⊢ [ t, ts… ] : List T₀


Note that the above rules forbid `List` elements that are `Type`s.  More
generally, if the element type is not a `Type` then that is a type error.

If the list elements do not all have the same type then that is a type error.

If an empty list does not have a type annotation then that is a type error.

The `List` concatenation operator takes arguments that are both `List`s of the
same type and returns a `List` of the same type:


    Γ ⊢ x : List A₀
    Γ ⊢ y : List A₁
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


## Records

Record types are "anonymous", meaning that they are uniquely defined by the
names and types of their fields.

An empty record is a `Type`:


    ─────────────
    Γ ⊢ {} : Type


A non-empty record can store terms, types and kinds:


    Γ ⊢ T : t₀   Γ ⊢ { xs… } : t₁   t₀ ⋁ t₁ = t₂
    ────────────────────────────────────────────  ; x ∉ { xs… }
    Γ ⊢ { x : T, xs… } : t₂


If the type of a field is not `Type`, `Kind`, or `Sort` then that is a type
error.

Carefully note that there should be no need to handle duplicate fields by this
point because the [desugaring rules for record literals](./record.md) merge
duplicate fields into unique fields.

Record values are also anonymous. The inferred record type has sorted fields
and normalized field types.


    ────────────
    Γ ⊢ {=} : {}


    Γ ⊢ t : T   Γ ⊢ { xs… } : { ts… }   Γ ⊢ { x : T, ts… } : i
    ──────────────────────────────────────────────────────────  ; x ∉ { xs… }
    Γ ⊢ { x = t, xs… } : { x : T, ts… }


You can only select field(s) from the record if they are present:


    Γ ⊢ e : { x : T, xs… }
    ──────────────────────
    Γ ⊢ e.x : T


    Γ ⊢ e : { ts… }
    ───────────────
    Γ ⊢ e.{} : {}


    Γ ⊢ e : { x : T, ts₀… }   Γ ⊢ e.{ xs… } : { ts₁… }
    ──────────────────────────────────────────────────  ; x ∉ { xs… }
    Γ ⊢ e.{ x, xs… } : { x : T, ts₁… }


Record projection can also be done by specifying the target record type.
For instance, provided that `s` is a record type and `e` is the source record,
`e.(s)` produces another record of type `s` whose values are taken from the
respective fields from `e`.


    Γ ⊢ e : { ts… }
    Γ ⊢ s : c
    s ⇥ {}
    ───────────────
    Γ ⊢ e.(s) : {}


    Γ ⊢ e : { x : T₀, ts… }
    Γ ⊢ s : c
    s ⇥ { x : T₁, ss… }
    T₀ ≡ T₁
    Γ ⊢ e.({ ss… }) : U
    ───────────────────────────
    Γ ⊢ e.(s) : { x : T₁, ss… }


If you select a field from a value that is not a record, then that is a type
error.

If the field is absent from the record then that is a type error.

Non-recursive right-biased merge also requires that both arguments are records:


    Γ ⊢ l : { ls… }
    Γ ⊢ r : {}
    ───────────────────
    Γ ⊢ l ⫽ r : { ls… }


    Γ ⊢ l : { ls… }
    Γ ⊢ r : { a : A, rs… }
    Γ ⊢ { ls… } ⫽ { rs… } : { ts… }
    ───────────────────────────────  ; a ∉ ls
    Γ ⊢ l ⫽ r : { a : A, ts… }


    Γ ⊢ l : { a : A₀, ls… }
    Γ ⊢ r : { a : A₁, rs… }
    Γ ⊢ { ls… } ⫽ { rs… } : { ts… }
    ───────────────────────────────
    Γ ⊢ l ⫽ r : { a : A₁, ts… }


If the operator arguments are not records then that is a type error.

Recursive record type merge requires that both arguments are record type
literals.  Any conflicting fields must be safe to recursively merge:


    Γ ⊢ l : t
    Γ ⊢ r : Type
    l ⇥ { ls… }
    r ⇥ {}
    ─────────────
    Γ ⊢ l ⩓ r : t


    Γ ⊢ l : t₀
    Γ ⊢ r : t₁
    l ⇥ { ls… }
    r ⇥ { a : A, rs… }
    Γ ⊢ { ls… } ⩓ { rs… } : t
    t₀ ⋁ t₁ = t₂
    ─────────────────────────  ; a ∉ ls
    Γ ⊢ l ⩓ r : t₂


    Γ ⊢ l : t₀
    Γ ⊢ r : t₁
    l ⇥ { a : A₀, ls… }
    r ⇥ { a : A₁, rs… }
    Γ ⊢ A₀ ⩓ A₁ : T₀
    Γ ⊢ { ls… } ⩓ { rs… } : T₁
    t₀ ⋁ t₁ = t₂
    ──────────────────────────
    Γ ⊢ l ⩓ r : t₂


If the operator arguments are not record types then that is a type error.

If they share a field in common that is not a record type then that is a type
error.

Recursive record merge requires that the types of both arguments can be
combined with recursive record merge:


    Γ ⊢ l : T₀
    Γ ⊢ r : T₁
    Γ ⊢ T₀ ⩓ T₁ : i
    T₀ ⩓ T₁ ⇥ T₂
    ───────────────
    Γ ⊢ l ∧ r : T₂


The `toMap` operator can be applied only to a record value, and every field
of the record must have the same type, which in turn must be a `Type`.


    Γ ⊢ e : { x : T, xs… }
    Γ ⊢ ( toMap { xs… } : List { mapKey : Text, mapValue : T } ) : List { mapKey : Text, mapValue : T }
    ───────────────────────────────────────────────────────────────────────────────────────────────────
    Γ ⊢ toMap e : List { mapKey : Text, mapValue : T }


    Γ ⊢ e : {}   Γ ⊢ T₀ : Type   T₀ ⇥ List { mapKey : Text, mapValue : T₁ }
    ───────────────────────────────────────────────────────────────────────
    Γ ⊢ ( toMap e : T₀ ) : List { mapKey : Text, mapValue : T₁ }


    Γ ⊢ toMap e : T₀   T₀ ≡ T₁
    ──────────────────────────
    Γ ⊢ ( toMap e : T₁ ) : T₀


You can complete a record literal using the record completion operator (`T::r`),
which is syntactic sugar for `(T.default ⫽ r) : T.Type`.  The motivation for
this operator is to easily create records without having to explicitly specify
default-valued fields.

In other words, given a a record `T` containing the following fields:

* A `Type` field with a record type
* A `default` field with default fields for the record type

... then `T::r` creates a record of type `T.Type` by extending the default
fields from `T.default` with the fields provided by `r`, overriding if
necessary.

To type-check a record completion, desugar the operator and type-check the
desugared form:


    Γ ⊢ ((T.default ⫽ r) : T.Type) : U
    ──────────────────────────────────
    Γ ⊢ T::r : U


## Unions

Union types are "anonymous", meaning that they are uniquely defined by the names
and types of their alternatives.

An empty union is a `Type`:


    ─────────────
    Γ ⊢ <> : Type


A non-empty union can have alternatives of terms, types and kinds:


    Γ ⊢ T : t₀   Γ ⊢ < xs… > : t₁  t₀ ⋁ t₁ = t₂
    ───────────────────────────────────────────  ; x ∉ { xs… }
    Γ ⊢ < x : T | xs… > : t₂


A union type may contain alternatives without an explicit type label:


    Γ ⊢ < ts… > : c
    ───────────────────  ; x ∉ < ts… >
    Γ ⊢ < x | ts… > : c


Note that the above rule allows storing values, types, and kinds in
unions.  However, if the type of the alternative is not `Type`,
`Kind`, or `Sort` then that is a type error.

If two alternatives share the same name then that is a type error.

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


    Γ ⊢ t : {}   Γ ⊢ u : <>   Γ ⊢ T : Type
    ──────────────────────────────────────
    Γ ⊢ (merge t u : T) : T


    Γ ⊢ merge t u : T₀   T₀ ≡ T₁
    ────────────────────────────
    Γ ⊢ (merge t u : T₁) : T₀


We use a trick to recursively check that all handlers have the same output type:
Based on the types of the remaining fields and alternatives, we "invent" values
(`t₁` and `u₁`) from which we can create the smaller `merge` expression.

An implementation could simply loop over the inferred record type.


    Γ ⊢ t₀ : { y : ∀(x : A₀) → T₀, ts… }
    Γ ⊢ u₀ : < y : A₁ | us… >
    A₀ ≡ A₁
    Γ ⊢ t₁ : { ts… }
    Γ ⊢ u₁ : < us… >
    ↑(-1, x, 0, T₀) = T₁
    Γ ⊢ (merge t₁ u₁ : T₁) : T₂
    ────────────────────────────────────  ; `x` not free in `T₀`
    Γ ⊢ merge t₀ u₀ : T₁


    Γ ⊢ t₀ : { y : T₀, ts… }
    Γ ⊢ u₀ : < y | us… >
    Γ ⊢ t₁ : { ts… }
    Γ ⊢ u₁ : < us… >
    Γ ⊢ (merge t₁ u₁ : T₀) : T₁
    ─────────────────────────────────────
    Γ ⊢ merge t₀ u₀ : T₀


`Optional`s can also be `merge`d as if they had type `< None | Some : A >`:


    Γ₀ ⊢ o : Optional A
    ↑(1, x, 0, (Γ₀, x : < None | Some : A >)) = Γ₁
    Γ₁ ⊢ merge t x : T
    ──────────────────────────────────
    Γ ⊢ merge t o : T


If the first argument of a `merge` expression is not a record then that is a
type error.

If the second argument of a `merge` expression is not a union or an `Optional`
then that is a type error.

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


    ──────────────────────────────────────
    Γ ⊢ Integer/negate : Integer → Integer


    ─────────────────────────────────────
    Γ ⊢ Integer/clamp : Integer → Natural


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


    Γ₀ ⊢ A : i   ↑(1, x, 0, (Γ₀, x : A)) = Γ₁   Γ₁ ⊢ B : o   i ↝ o : c
    ──────────────────────────────────────────────────────────────────
    Γ₀ ⊢ ∀(x : A) → B : c


If the input or output type is neither a `Type`, a `Kind`, nor a `Sort` then
that is a type error.

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


    Γ₀ ⊢ A₀ : c₀
    A₀ ⇥ A₁
    ↑(1, x, 0, (Γ₀, x : A₁)) = Γ₁
    Γ₁ ⊢ b : B
    Γ₀ ⊢ ∀(x : A₁) → B : c₁
    ──────────────────────────────────
    Γ₀ ⊢ λ(x : A₀) → b : ∀(x : A₁) → B


Note that the above rule requires that the inferred function type must be
well-typed.

The type system ensures that function application is well-typed, meaning that
the input type that a function expects matches the inferred type of the
function's argument:


    Γ ⊢ f : ∀(x : A₀) → B₀
    Γ ⊢ a₀ : A₁
    A₀ ≡ A₁
    ↑(1, x, 0, a₀) = a₁
    B₀[x ≔ a₁] = B₁
    ↑(-1, x, 0, B₁) = B₂
    B₂ ⇥ B₃
    ──────────────────────
    Γ ⊢ f a₀ : B₃


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

Type-checking an annotated expression verifies that the annotation
matches the inferred type of the annotated expression, and returns the
inferred type as the type of the whole expression:


    Γ ⊢ T₀ : i   Γ ⊢ t : T₁   T₀ ≡ T₁
    ─────────────────────────────────
    Γ ⊢ (t : T₀) : T₁


Note that the above rule permits kind annotations, such as `List : Type → Type`.

If the inferred type of the annotated expression does not match the type
annotation then that is a type error.

Even though `Sort` is not a type-valid expression by itself, it is valid
as a type annotation:


    Γ ⊢ t : Sort
    ─────────────────────
    Γ ⊢ (t : Sort) : Sort


## Assertions

An assertion is equivalent to built-in language support for checking two
expressions for judgmental equality, commonly invoked like this:

    let example = assert : (2 + 2) === 4

    in  …

An assertion checks that:

* The type annotation is an equivalence
* The two sides of the equivalence are in fact equivalent

... or in other words:


    Γ ⊢ T : Type
    T ⇥ x === y
    x ≡ y
    ──────────────────────────
    Γ ⊢ (assert : T) : x === y


The inferred type of an assertion is the same as the provided annotation.

If the annotation is not an equivalence then that is a type error.

If the two sides of the equivalence are not equivalent then that is a type error.

To type-check an equivalence, verify that the two sides are terms:


    Γ ⊢ x : A₀
    Γ ⊢ y : A₁
    Γ ⊢ A₀ : Type
    Γ ⊢ A₁ : Type
    A₀ ≡ A₁
    ──────────────────
    Γ ⊢ x === y : Type


If either side of the equivalence is not a term, then that is a type error.

If the inferred types do not match, then that is also a type error.

## Imports

An expression with unresolved imports cannot be type-checked
