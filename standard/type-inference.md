# Type inference

```haskell
module TypeInference
    ( -- * Type inference
      inferType
    , freeVars
    ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Set (Set)
import FunctionCheck (functionCheck)
import Prelude hiding (Bool(..))
import Shift (shift)
import Substitution (substitute)
import Syntax

import qualified BetaNormalization
import qualified Data.List          as List
import qualified Data.Map           as Map
import qualified Data.Ord           as Ord
import qualified Data.Set           as Set
import qualified Equivalence
import qualified Prelude
```

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
* [`Date` / `Time` / `TimeZone`](#date--time--timezone)
* [`List`](#list)
* [`Optional`](#optional)
* [Records](#records)
* [Unions](#unions)
* [`with` expressions](#with-expressions)
* [`Integer`](#integer)
* [`Double`](#double)
* [Functions](#functions)
* [`let` expressions](#let-expressions)
* [Type annotations](#type-annotations)
* [Assertions](#assertions)
* [Nested record updates](#assertions)
* [Imports](#imports)
* [Detecting free variables](#free-variables)

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
annotations in the context for the same variable.  The De Bruijn index associated
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
For example, if the context has a single type annotation (say, `x : T`)
then `x@0` is valid but `x@1` is a type error.

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
    Γ ⊢ L : c₀
    Γ ⊢ R : c₁
    L ≡ R
    ──────────────────────────
    Γ ⊢ if t then l else r : L


If either branch of the if expression is not a term, type, or kind, then that is a type error.

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


The `Text/replace` function has the following type:


    ───────────────────────────────────────────────────────────────────────────────────────
    Γ ⊢ Text/replace : ∀(needle : Text) → ∀(replacement : Text) → ∀(haystack : Text) → Text


The `Text` concatenation operator takes arguments of type `Text` and returns a
result of type `Text`:


    Γ ⊢ x : Text   Γ ⊢ y : Text
    ───────────────────────────
    Γ ⊢ x ++ y : Text


If the operator arguments do not have type `Text`, then that is a type error.

## `Date` / `Time` / `TimeZone`

`Date`, `Time`, and `TimeZone` are all `Type`s:


    ───────────────
    Γ ⊢ Date : Type


    ───────────────
    Γ ⊢ Time : Type


    ───────────────────
    Γ ⊢ TimeZone : Type


`Date` literals have type `Date`:


    ─────────────────────
    Γ ⊢ YYYY-MM-DD : Date


`Time` literals have type `Time`:


    ───────────────────
    Γ ⊢ hh:mm:ss : Time


… and `TimeZone` literals have type `TimeZone`:


    ─────────────────────
    Γ ⊢ ±HH:MM : TimeZone


The `{Date,Time,TimeZone}/show` functions have the following type:


    ───────────────────────────
    Γ ⊢ Date/show : Date → Text


    ───────────────────────────
    Γ ⊢ Time/show : Time → Text


    ───────────────────────────────────
    Γ ⊢ TimeZone/show : TimeZone → Text


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

An empty record type is a `Type`:


    ─────────────
    Γ ⊢ {} : Type


A non-empty record type can store terms, types and kinds:


    Γ ⊢ T : t₀   Γ ⊢ { xs… } : t₁   t₀ ⋁ t₁ = t₂
    ────────────────────────────────────────────  ; x ∉ { xs… }
    Γ ⊢ { x : T, xs… } : t₂


If the type of a field is not `Type`, `Kind`, or `Sort` then that is a type
error.

If a record type has duplicate fields then it is a type error.

Record values are also anonymous. The inferred record type has sorted fields
and normalized field types.


    ────────────
    Γ ⊢ {=} : {}


    Γ ⊢ t : T   Γ ⊢ { xs… } : { ts… }   Γ ⊢ { x : T, ts… } : i
    ──────────────────────────────────────────────────────────  ; x ∉ { xs… }
    Γ ⊢ { x = t, xs… } : { x : T, ts… }


Carefully note that there should be no need to handle duplicate fields by this
point because the [desugaring rules for record literals](./record.md) merge
duplicate fields into unique fields.

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


    Γ ⊢ u : c   u ⇥ < x : T | ts… >   ↑(1, x, 0, < x : T | ts… >) = U
    ─────────────────────────────────────────────────────────────────
    Γ ⊢ u.x : ∀(x : T) → U


If a union alternative is empty, then the corresponding constructor's type is
the same as the original union type:


    Γ ⊢ u : c   u ⇥ < x | ts… >
    ───────────────────────────
    Γ ⊢ u.x : < x | ts… >

### `merge` expressions

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
    x ∉ freeVars(T₀)
    ↑(-1, x, 0, T₀) = T₁
    Γ ⊢ (merge t₁ u₁ : T₁) : T₂
    ────────────────────────────────────
    Γ ⊢ merge t₀ u₀ : T₁


    Γ ⊢ t₀ : { y : T₀, ts… }
    Γ ⊢ u₀ : < y | us… >
    Γ ⊢ t₁ : { ts… }
    Γ ⊢ u₁ : < us… >
    Γ ⊢ (merge t₁ u₁ : T₀) : T₁
    ─────────────────────────────────────
    Γ ⊢ merge t₀ u₀ : T₀

If `x` is free in `T₀` then it is a type error.
See [Detecting free variables](#free-variables) for details about the `freeVars` judgment.

`Optional`s can also be `merge`d as if they had type `< None | Some : A >`.
To achieve that, we type-check a new `merge` expression with a second argument (`x`) of that type:


    Γ₀ ⊢ o : Optional A
    ↑(1, x, 0, (Γ₀, x : < None | Some : A >)) = Γ₁
    ↑(1, x, 0, t₀) = t₁
    Γ₁ ⊢ merge t₁ x : T
    ──────────────────────────────────
    Γ₀ ⊢ merge t₀ o : T


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

### `showConstructor` expressions

A `showConstructor` expression with a union or an `Optional` as its first
argument has type `Text`.


    Γ ⊢ e : < ts… >
    ──────────────────────────────
    Γ ⊢ showConstructor e : Text


    Γ ⊢ o : Optional A
    ────────────────────────────
    Γ ⊢ showConstructor o : Text


If the first argument of a `showConstructor` expression is not a union or an
`Optional` then that is a type error.

## `with` expressions

A record update using the `with` keyword replaces a field:


    Γ ⊢ e : { k : T₁, ts… }
    Γ ⊢ v : T₂
    ──────────────────────────────────
    Γ ⊢ e with k = v : { k : T₂, ts… }


    Γ ⊢ e : { ts… }
    Γ ⊢ v : T
    ─────────────────────────────────  ; k ∉ ts
    Γ ⊢ e with k = v : { k : T, ts… }


... and record updates can be nested, creating intermediate records if
necessary:


    Γ ⊢ e : { k₀ : T₀, ts… }
    Γ ⊢ e.k₀ with k₁.ks… = v : T₁
    ───────────────────────────────────────────
    Γ ⊢ e with k₀.k₁.ks… = v : { k₀ : T₁, ts… }


    Γ ⊢ e : { ts… }
    Γ ⊢ {=} with k₁.ks… = v : T₁
    ───────────────────────────────────────────  ; k₀ ∉ ts
    Γ ⊢ e with k₀.k₁.ks… = v : { k₀ : T₁, ts… }


An `Optional` update using the `with` keyword must preserve the type of the "inner" value.
(This restriction is due to the fact that Dhall performs beta-reduction with no type information available.)


    Γ ⊢ e : Optional T₀
    Γ ⊢ v : T₁
    T₀ ≡ T₁
    ──────────────────────────────────
    Γ ⊢ e with ? = v : Optional T₀
    
    
    Γ ⊢ e : Optional T₀
    Γ, x : T₀ ⊢ x with k.ks… = v : T₁
    T₀ ≡ T₁
    ────────────────────────────────── x ∉ FV(v)
    Γ ⊢ e with ?.k.ks… = v : Optional T₀


If the expression being updated (i.e., the `e` in `e with ks… = v`) is not a
record or an `Optional` then that is a type error.

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

An expression with unresolved imports cannot be type-checked.

## Free variables

For `merge` expressions, type-checking requires determining the set of free variables in a term.
This is done via the `freeVars` judgment.
The `freeVars` judgment is a function that gives the set of variables that are free (with the de Bruijn index equal to 0) in a term.

The input of a `freeVars` judgment is a term.
The output is a set of variable names (without de Bruijn indices).

For example, free variables of the term `x + y` is the set of variables `x` and `y`. Free variables of the term `λ(x : Natural) → x + y` is the set containing only `y`, as `x` is not free in that term.

For these examples, we write the judgments as:

    freeVars(x + y) = { x, y }

    freeVars(λ(x : Natural) → x + y) = { y }


The rules of the `freeVars` judgment are standard for lambda-calculus (see, for example, [this tutorial](https://opendsa.cs.vt.edu/ODSA/Books/PL/html/FreeBoundVariables.html)) except for the special handling of de Bruijn indices.

The set of free variables is empty for:

- all literals that contain no other terms (numerical, date or time, bytes, etc. literals, but not text literals with interpolations and not record literals or record types)
- for built-in terms such as `List` or `Natural/show`
- for built-in constant symbols such as `Type`, `True` or `False`
- and for imports without a `using headers` option, as imported expressions may not contain free variables


    ───────────────── ; n is a Natural literal
    freeVars(n) = { }



    ───────────────── ; t is a time literal
    freeVars(t) = { }


    ...


    ──────────────────────────── ; Natural/show is a built-in term
    freeVars(Natural/show) = { }


    ...


    ───────────────────── ; Type is a built-in constant
    freeVars(Type) = { }


    ...


    ────────────────────────----------- ; import without a headers term
    freeVars(https://example.com) = { }


Special rules apply to variables, function types, function expressions, and `let` expressions.

For variables:


    ─────────────────────── ; Variable with zero de Bruijn index
    freeVars(x @ 0) = { x }



    ─────────────────────────── ; Variable with a non-zero de Bruijn index
    freeVars(x @ (1 + n)) = { }


For function expressions, function types, and `let` expressions, the body of the substitution must be shifted before computing `freeVars` and the name of the new bound variable must be removed from the free variable set:


    freeVars(T) = V₀
    ↑(1, x, 0, a₀) = a₁
    freeVars(a₁) = V₁
    V₂ = V₁ \ {x}
    V₃ = V₀ ∪ V₂
    ────────────────────────────
    freeVars(λ(x : T) → a₀) = V₃

The rules for function types are the same:


    freeVars(T) = V₀
    ↑(1, x, 0, a₀) = a₁
    freeVars(a₁) = V₁
    V₂ = V₁ \ {x}
    V₃ = V₀ ∪ V₂
    ───────────────────────────
    freeVars(∀(x : T) → a₀) = V₃


The rules for `let` expressions are the same except that there may be free variables in the body expression and the type annotation may be missing:

When type annotations are present:


    freeVars(T) = V₀
    ↑(1, x, 0, a₀) = a₁
    freeVars(a₁) = V₁
    V₂ = V₁ \ {x}
    freeVars(b₀) = V₃
    V₄ = V₀ ∪ V₂ ∪ V₃
    ───────────────────────────────────
    freeVars(let x : T = b₀ in a₀) = V₄


When type annotations are missing:


    ↑(1, x, 0, a₀) = a₁
    freeVars(a₁) = V₁
    V₂ = V₁ \ {x}
    freeVars(b₀) = V₃
    V₄ = V₂ ∪ V₃
    ───────────────────────────────
    freeVars(let x = b₀ in a₀) = V₃


For terms of all other forms, `freeVars` is computed as the union of the sets of free variables computed recursively for all subterms. 

    
    freeVars(f) = V₁
    freeVars(b) = V₂
    V₃ = V₁ ∪ V₂
    ──────────────────
    freeVars(f b) = V₃

    
    freeVars(a) = V₁
    freeVars(b) = V₂
    freeVars(c) = V₃
    V₄ = V₁ ∪ V₂ ∪ V₃
    ─────────────────────────────────
    freeVars(if a then b else c) = V₄


    ...

    
    freeVars(T) = V₁
    ────────────────────────
    freeVars(assert: T) = V₁

```haskell
type Context = [(Text, Expression)]

inferType
    :: Context     -- ^ @Γ@
    -> Expression  -- ^ @t@
    -> Maybe Expression  -- ^ @T@

nf :: Expression -> Expression
nf = BetaNormalization.betaNormalize

eq :: Expression -> Expression -> Prelude.Bool
eq = Equivalence.equivalent

arr :: Expression -> Expression -> Expression
arr a b = Forall "_" a b

pi_ :: Text -> Expression -> Expression -> Expression
pi_ = Forall

extend :: Text -> Expression -> Context -> Context
extend x t ctx = shiftContext 1 x 0 ((x, t) : ctx)

shiftContext :: Integer -> Text -> Natural -> Context -> Context
shiftContext _d _x _m [] = []
shiftContext d x m ((y, t) : ctx) =
    (y, shift d x m t) : shiftContext d x m ctx

lookupVariable :: Text -> Natural -> Context -> Maybe Expression
lookupVariable _x _n [] =
    Nothing
lookupVariable x n ((y, t) : ctx)
    | x == y && n == 0 =
        -- @T@ was checked when the binder was added; re-checking the
        -- already-shifted type in the remaining context is wrong under
        -- intervening same-name binders (see @mergeEquivalence@).
        Just t
    | x == y =
        lookupVariable x (n - 1) ctx
    | Prelude.otherwise =
        lookupVariable x n ctx

inferUniverse :: Context -> Expression -> Maybe Constant
inferUniverse ctx t = do
    k <- inferType ctx t
    case k of
        Constant c -> Just c
        _          -> Nothing

vee :: Constant -> Constant -> Constant
vee = max

sortedRecordType :: [(Text, Expression)] -> Expression
sortedRecordType fields =
    RecordType (List.sortBy (Ord.comparing fst) fields)

uniqueKeys :: [(Text, a)] -> Prelude.Bool
uniqueKeys kvs =
    List.sort (map fst kvs) == List.nub (List.sort (map fst kvs))

asRecordType :: Expression -> Maybe [(Text, Expression)]
asRecordType t =
    case nf t of
        RecordType fields -> Just fields
        _                 -> Nothing

asUnionType :: Expression -> Maybe [(Text, Maybe Expression)]
asUnionType t =
    case nf t of
        UnionType alts -> Just alts
        _              -> Nothing

textMapType :: Expression -> Expression
textMapType t =
    Application (Builtin List)
        (RecordType [("mapKey", Builtin Text), ("mapValue", t)])

inferType _ctx (Constant Type) = Just (Constant Kind)
inferType _ctx (Constant Kind) = Just (Constant Sort)
inferType _   (Constant Sort) = Nothing

inferType ctx (Variable x n) =
    lookupVariable x n ctx

inferType _ctx (Builtin Bool)     = Just (Constant Type)
inferType _ctx (Builtin True)     = Just (Builtin Bool)
inferType _ctx (Builtin False)    = Just (Builtin Bool)

inferType ctx (If t l r) = do
    tT <- inferType ctx t
    guard (eq tT (Builtin Bool))
    lT <- inferType ctx l
    rT <- inferType ctx r
    _  <- inferUniverse ctx lT
    _  <- inferUniverse ctx rT
    guard (eq lT rT)
    return (nf lT)

inferType ctx (Operator l Or r) = inferBoolOp ctx l r
inferType ctx (Operator l And r) = inferBoolOp ctx l r
inferType ctx (Operator l Equal r) = inferBoolOp ctx l r
inferType ctx (Operator l NotEqual r) = inferBoolOp ctx l r

inferType _ctx (Builtin Natural) = Just (Constant Type)
inferType _ctx (NaturalLiteral _) = Just (Builtin Natural)
inferType ctx (Operator l Plus r) = inferNaturalOp ctx l r
inferType ctx (Operator l Times r) = inferNaturalOp ctx l r

inferType _ctx (Builtin NaturalBuild) = Just $
    arr (pi_ "natural" (Constant Type)
            (pi_ "succ" (arr (Variable "natural" 0) (Variable "natural" 0))
                (pi_ "zero" (Variable "natural" 0) (Variable "natural" 0))))
        (Builtin Natural)

inferType _ctx (Builtin NaturalFold) = Just $
    arr (Builtin Natural)
        (pi_ "natural" (Constant Type)
            (pi_ "succ" (arr (Variable "natural" 0) (Variable "natural" 0))
                (pi_ "zero" (Variable "natural" 0) (Variable "natural" 0))))

inferType _ctx (Builtin NaturalIsZero)    = Just (arr (Builtin Natural) (Builtin Bool))
inferType _ctx (Builtin NaturalEven)      = Just (arr (Builtin Natural) (Builtin Bool))
inferType _ctx (Builtin NaturalOdd)       = Just (arr (Builtin Natural) (Builtin Bool))
inferType _ctx (Builtin NaturalToInteger) = Just (arr (Builtin Natural) (Builtin Integer))
inferType _ctx (Builtin NaturalShow)      = Just (arr (Builtin Natural) (Builtin Text))
inferType _ctx (Builtin NaturalSubtract)  =
    Just (arr (Builtin Natural) (arr (Builtin Natural) (Builtin Natural)))

inferType _ctx (Builtin Text) = Just (Constant Type)
inferType ctx (TextLiteral (Chunks chunks _z)) = do
    mapM_ (\(_, t) -> do
        tT <- inferType ctx t
        guard (eq tT (Builtin Text))) chunks
    return (Builtin Text)
inferType _ctx (Builtin TextShow) = Just (arr (Builtin Text) (Builtin Text))
inferType _ctx (Builtin TextReplace) = Just $
    pi_ "needle" (Builtin Text)
        (pi_ "replacement" (Builtin Text)
            (pi_ "haystack" (Builtin Text) (Builtin Text)))
inferType ctx (Operator l TextAppend r) = do
    lT <- inferType ctx l
    rT <- inferType ctx r
    guard (eq lT (Builtin Text) && eq rT (Builtin Text))
    return (Builtin Text)

inferType _ctx (Builtin Date)     = Just (Constant Type)
inferType _ctx (Builtin Time)     = Just (Constant Type)
inferType _ctx (Builtin TimeZone) = Just (Constant Type)
inferType _ctx (DateLiteral _)     = Just (Builtin Date)
inferType _ctx (TimeLiteral _ _)   = Just (Builtin Time)
inferType _ctx (TimeZoneLiteral _) = Just (Builtin TimeZone)
inferType _ctx (Builtin DateShow)     = Just (arr (Builtin Date) (Builtin Text))
inferType _ctx (Builtin TimeShow)     = Just (arr (Builtin Time) (Builtin Text))
inferType _ctx (Builtin TimeZoneShow) = Just (arr (Builtin TimeZone) (Builtin Text))

inferType _ctx (Builtin Bytes) = Just (Constant Type)
inferType _ctx (BytesLiteral _) = Just (Builtin Bytes)

inferType _ctx (Builtin List) = Just (arr (Constant Type) (Constant Type))
inferType ctx (EmptyList t0) = do
    _  <- inferUniverse ctx t0
    let t1 = nf t0
    case t1 of
        Application (Builtin List) tElem -> do
            k <- inferType ctx tElem
            guard (eq k (Constant Type))
            return (Application (Builtin List) (nf tElem))
        _ -> Nothing

inferType ctx (NonEmptyList (t :| ts)) = do
    tT <- inferType ctx t
    k  <- inferType ctx tT
    guard (eq k (Constant Type))
    mapM_ (\u -> do
        uT <- inferType ctx u
        guard (eq tT uT)) ts
    return (Application (Builtin List) (nf tT))

inferType ctx (Operator l ListAppend r) = do
    lT <- inferType ctx l
    rT <- inferType ctx r
    case (nf lT, nf rT) of
        (Application (Builtin List) a0, Application (Builtin List) a1) -> do
            guard (eq a0 a1)
            return (Application (Builtin List) (nf a0))
        _ -> Nothing

inferType _ctx (Builtin ListBuild) = Just $
    pi_ "a" (Constant Type)
        (arr (pi_ "list" (Constant Type)
                (pi_ "cons" (arr (Variable "a" 0) (arr (Variable "list" 0) (Variable "list" 0)))
                    (pi_ "nil" (Variable "list" 0) (Variable "list" 0))))
             (Application (Builtin List) (Variable "a" 0)))

inferType _ctx (Builtin ListFold) = Just $
    pi_ "a" (Constant Type)
        (arr (Application (Builtin List) (Variable "a" 0))
             (pi_ "list" (Constant Type)
                 (pi_ "cons" (arr (Variable "a" 0) (arr (Variable "list" 0) (Variable "list" 0)))
                     (pi_ "nil" (Variable "list" 0) (Variable "list" 0)))))

inferType _ctx (Builtin ListLength) = Just $
    pi_ "a" (Constant Type)
        (arr (Application (Builtin List) (Variable "a" 0)) (Builtin Natural))

inferType _ctx (Builtin ListHead) = Just $
    pi_ "a" (Constant Type)
        (arr (Application (Builtin List) (Variable "a" 0))
             (Application (Builtin Optional) (Variable "a" 0)))

inferType _ctx (Builtin ListLast) = Just $
    pi_ "a" (Constant Type)
        (arr (Application (Builtin List) (Variable "a" 0))
             (Application (Builtin Optional) (Variable "a" 0)))

inferType _ctx (Builtin ListIndexed) = Just $
    pi_ "a" (Constant Type)
        (arr (Application (Builtin List) (Variable "a" 0))
             (Application (Builtin List)
                 (RecordType [("index", Builtin Natural), ("value", Variable "a" 0)])))

inferType _ctx (Builtin ListReverse) = Just $
    pi_ "a" (Constant Type)
        (arr (Application (Builtin List) (Variable "a" 0))
             (Application (Builtin List) (Variable "a" 0)))

inferType _ctx (Builtin Optional) = Just (arr (Constant Type) (Constant Type))
inferType ctx (Some a) = do
    aT <- inferType ctx a
    k  <- inferType ctx aT
    guard (eq k (Constant Type))
    return (Application (Builtin Optional) (nf aT))
inferType _ctx (Builtin None) = Just $
    pi_ "A" (Constant Type) (Application (Builtin Optional) (Variable "A" 0))

inferType _ctx (RecordType []) = Just (Constant Type)
inferType ctx (RecordType fields) = do
    guard (uniqueKeys fields)
    universes <- mapM (\(_, t) -> inferUniverse ctx t) fields
    return (Constant (List.foldl1' vee universes))

inferType _ctx (RecordLiteral []) = Just (RecordType [])
inferType ctx (RecordLiteral fields) = do
    guard (uniqueKeys fields)
    typed <- mapM (\(x, t) -> do
        tT <- inferType ctx t
        return (x, tT)) fields
    let recordType = sortedRecordType typed
    _ <- inferType ctx recordType
    return recordType

inferType ctx (Field e x) = do
    eT <- inferType ctx e
    case asRecordType eT of
        Just fields -> do
            t <- lookup x fields
            return (nf t)
        Nothing ->
            case asUnionType e of
                Just alts | Just (Just t) <- lookup x alts -> do
                    let u = nf e
                    let u1 = shift 1 x 0 u
                    return (pi_ x t u1)
                Just alts | Just Nothing <- lookup x alts ->
                    return (nf e)
                _ -> Nothing

inferType ctx (ProjectByLabels e []) = do
    eT <- inferType ctx e
    guard (case asRecordType eT of
        Just _  -> Prelude.True
        Nothing -> Prelude.False)
    return (RecordType [])
inferType ctx (ProjectByLabels e (x:xs)) = do
    eT <- inferType ctx e
    fields <- asRecordType eT
    t <- lookup x fields
    RecordType rest <- inferType ctx (ProjectByLabels e xs)
    guard (x `notElem` map fst rest)
    return (sortedRecordType ((x, t) : rest))

inferType ctx (ProjectByType e s) = do
    eT <- inferType ctx e
    _  <- inferUniverse ctx s
    srcFields <- asRecordType eT
    case nf s of
        RecordType [] -> return (RecordType [])
        RecordType ((x, t1):ss) -> do
            t0 <- lookup x srcFields
            guard (eq t0 t1)
            RecordType rest <- inferType ctx (ProjectByType e (RecordType ss))
            return (sortedRecordType ((x, t1) : rest))
        _ -> Nothing

inferType ctx (Operator l Prefer r) = do
    lT <- inferType ctx l
    rT <- inferType ctx r
    ls <- asRecordType lT
    rs <- asRecordType rT
    return (sortedRecordType (preferFields ls rs))

inferType ctx (Operator l CombineRecordTypes r) = do
    lT <- inferType ctx l
    rT <- inferType ctx r
    c0 <- case lT of
        Constant c -> Just c
        _          -> Nothing
    c1 <- case rT of
        Constant c -> Just c
        _          -> Nothing
    ls <- asRecordType l
    rs <- asRecordType r
    _  <- checkCombineRecordTypes ctx ls rs
    return (Constant (vee c0 c1))

inferType ctx (Operator l CombineRecordTerms r) = do
    t0 <- inferType ctx l
    t1 <- inferType ctx r
    _  <- inferType ctx (Operator t0 CombineRecordTypes t1)
    return (nf (Operator t0 CombineRecordTypes t1))

inferType ctx (ToMap e Nothing) = do
    eT <- inferType ctx e
    fields <- asRecordType eT
    case fields of
        [] -> Nothing
        (_, t):rest -> do
            k <- inferType ctx t
            guard (eq k (Constant Type))
            mapM_ (\(_, t') -> guard (eq t t')) rest
            return (textMapType (nf t))

inferType ctx (ToMap e (Just t0)) = do
    _ <- inferUniverse ctx t0
    inferred <- inferType ctx (ToMap e Nothing) <|> emptyAnnotatedToMap ctx t0
    guard (eq inferred t0)
    return (nf inferred)

inferType ctx (Completion t r) =
    inferType ctx
        (Annotation
            (Operator (Field t "default") Prefer r)
            (Field t "Type"))

inferType _ctx (UnionType []) = Just (Constant Type)
inferType ctx (UnionType alts) = do
    guard (uniqueKeys alts)
    universes <- mapM inferAltUniverse alts
    return (Constant (List.foldl1' vee universes))
  where
    inferAltUniverse (_, Nothing) = Just Type
    inferAltUniverse (_, Just t)  = inferUniverse ctx t

inferType ctx (Merge t u Nothing) =
    inferMerge ctx t u Nothing
inferType ctx (Merge t u (Just annotation)) = do
    _ <- inferType ctx annotation
    result <- inferMerge ctx t u (Just annotation)
    guard (eq result annotation)
    return (nf result)

inferType ctx (ShowConstructor e) = do
    eT <- inferType ctx e
    case asUnionType eT of
        Just _ -> return (Builtin Text)
        Nothing ->
            case nf eT of
                Application (Builtin Optional) _ -> return (Builtin Text)
                _ -> Nothing

inferType ctx (With e (k :| ks) v) =
    inferWith ctx e (k :| ks) v

inferType _ctx (Builtin Integer) = Just (Constant Type)
inferType _ctx (IntegerLiteral _) = Just (Builtin Integer)
inferType _ctx (Builtin IntegerShow)     = Just (arr (Builtin Integer) (Builtin Text))
inferType _ctx (Builtin IntegerToDouble) = Just (arr (Builtin Integer) (Builtin Double))
inferType _ctx (Builtin IntegerNegate)   = Just (arr (Builtin Integer) (Builtin Integer))
inferType _ctx (Builtin IntegerClamp)    = Just (arr (Builtin Integer) (Builtin Natural))

inferType _ctx (Builtin Double) = Just (Constant Type)
inferType _ctx (DoubleLiteral _) = Just (Builtin Double)
inferType _ctx (Builtin DoubleShow) = Just (arr (Builtin Double) (Builtin Text))

inferType ctx (Forall x a b) = do
    i <- inferUniverse ctx a
    let ctx1 = extend x (nf a) ctx
    o <- inferUniverse ctx1 b
    return (Constant (functionCheck i o))

inferType ctx (Lambda x a0 b) = do
    _  <- inferUniverse ctx a0
    let a1 = nf a0
    let ctx1 = extend x a1 ctx
    bodyT <- inferType ctx1 b
    let functionType = pi_ x a1 bodyT
    _ <- inferType ctx functionType
    return functionType

inferType ctx (Application f a0) = do
    fT <- inferType ctx f
    case nf fT of
        Forall x aExpected b0 -> do
            aActual <- inferType ctx a0
            guard (eq aExpected aActual)
            let a2 = shift 1 x 0 a0
            let b1 = substitute b0 x 0 a2
            let b2 = shift (-1) x 0 b1
            return (nf b2)
        _ -> Nothing

inferType ctx (Let x (Just a0) aBound b0) = do
    a1 <- inferType ctx aBound
    _  <- inferUniverse ctx a0
    guard (eq a0 a1)
    let aN = nf aBound
    let a2 = shift 1 x 0 aN
    let b1 = substitute b0 x 0 a2
    let b2 = shift (-1) x 0 b1
    inferType ctx b2

inferType ctx (Let x Nothing aBound b0) = do
    _  <- inferType ctx aBound
    let aN = nf aBound
    let a2 = shift 1 x 0 aN
    let b1 = substitute b0 x 0 a2
    let b2 = shift (-1) x 0 b1
    inferType ctx b2

inferType ctx (Annotation t (Constant Sort)) = do
    tT <- inferType ctx t
    guard (eq tT (Constant Sort))
    return (Constant Sort)
inferType ctx (Annotation t t0) = do
    _  <- inferUniverse ctx t0
    t1 <- inferType ctx t
    guard (eq t0 t1)
    return t1

inferType ctx (Assert t) = do
    k <- inferType ctx t
    guard (eq k (Constant Type))
    case nf t of
        Operator x Equivalent y -> do
            guard (eq x y)
            return (nf t)
        _ -> Nothing

inferType ctx (Operator x Equivalent y) = do
    a0 <- inferType ctx x
    a1 <- inferType ctx y
    k0 <- inferType ctx a0
    k1 <- inferType ctx a1
    guard (eq k0 (Constant Type) && eq k1 (Constant Type))
    guard (eq a0 a1)
    return (Constant Type)

inferType ctx (Operator l Alternative r) = do
    _ <- inferType ctx l
    inferType ctx r

inferType _ Import{} = Nothing

inferBoolOp :: Context -> Expression -> Expression -> Maybe Expression
inferBoolOp ctx l r = do
    lT <- inferType ctx l
    rT <- inferType ctx r
    guard (eq lT (Builtin Bool) && eq rT (Builtin Bool))
    return (Builtin Bool)

inferNaturalOp :: Context -> Expression -> Expression -> Maybe Expression
inferNaturalOp ctx l r = do
    lT <- inferType ctx l
    rT <- inferType ctx r
    guard (eq lT (Builtin Natural) && eq rT (Builtin Natural))
    return (Builtin Natural)

preferFields :: [(Text, Expression)] -> [(Text, Expression)] -> [(Text, Expression)]
preferFields ls rs =
    let rMap = Map.fromList rs
        kept = [ kv | kv@(k, _) <- ls, not (Map.member k rMap) ]
    in kept <> rs

checkCombineRecordTypes
    :: Context
    -> [(Text, Expression)]
    -> [(Text, Expression)]
    -> Maybe ()
checkCombineRecordTypes ctx ls rs = do
    let rMap = Map.fromList rs
    mapM_
        (\(k, a) -> case Map.lookup k rMap of
            Nothing -> return ()
            Just b  -> do
                _ <- inferType ctx (Operator a CombineRecordTypes b)
                return ())
        ls

emptyAnnotatedToMap :: Context -> Expression -> Maybe Expression
emptyAnnotatedToMap ctx t0 = do
    eT <- inferType ctx (RecordLiteral [])
    guard (eq eT (RecordType []))
    case nf t0 of
        Application (Builtin List) (RecordType kvs)
            | Just t1 <- lookup "mapValue" kvs
            , maybe Prelude.False (`eq` Builtin Text) (lookup "mapKey" kvs) -> do
                k <- inferType ctx t1
                guard (eq k (Constant Type))
                return (nf t0)
        _ -> Nothing

inferMerge
    :: Context
    -> Expression
    -> Expression
    -> Maybe Expression
    -> Maybe Expression
inferMerge ctx t u maybeAnn = do
    tT <- inferType ctx t
    uT <- inferType ctx u
    handlers <- asRecordType tT
    case asUnionType uT of
        Just alts ->
            mergeHandlers ctx handlers alts maybeAnn
        Nothing ->
            case nf uT of
                Application (Builtin Optional) a ->
                    mergeHandlers ctx handlers
                        [("None", Nothing), ("Some", Just a)] maybeAnn
                _ -> Nothing

mergeHandlers
    :: Context
    -> [(Text, Expression)]
    -> [(Text, Maybe Expression)]
    -> Maybe Expression
    -> Maybe Expression
mergeHandlers ctx handlers alts maybeAnn = do
    guard (List.sort (map fst handlers) == List.sort (map fst alts))
    outputs <- mapM (handlerOutput ctx) alts
    case outputs of
        [] ->
            case maybeAnn of
                Just ann -> do
                    k <- inferType ctx ann
                    guard (eq k (Constant Type))
                    return (nf ann)
                Nothing -> Nothing
        o:os -> do
            mapM_ (\o' -> guard (eq o o')) os
            case maybeAnn of
                Just ann -> do
                    guard (eq o ann)
                    return (nf o)
                Nothing -> return (nf o)
  where
    handlerMap = Map.fromList handlers
    handlerOutput _ctx (y, Just a1) = do
        handler <- Map.lookup y handlerMap
        case nf handler of
            Forall x a0 t0 -> do
                guard (eq a0 a1)
                guard (not (Set.member x (freeVars t0)))
                return (shift (-1) x 0 t0)
            _ -> Nothing
    handlerOutput _ctx (y, Nothing) =
        Map.lookup y handlerMap

inferWith
    :: Context
    -> Expression
    -> NonEmpty PathComponent
    -> Expression
    -> Maybe Expression
inferWith ctx e (Label k :| []) v = do
    eT <- inferType ctx e
    vT <- inferType ctx v
    fields <- asRecordType eT
    let fields' = Map.insert k vT (Map.fromList fields)
    return (sortedRecordType (Map.toList fields'))
inferWith ctx e (Label k0 :| (k1:ks)) v = do
    eT <- inferType ctx e
    fields <- asRecordType eT
    nested <- case lookup k0 fields of
        Just _ ->
            inferWith ctx (Field e k0) (k1 :| ks) v
        Nothing ->
            inferWith ctx (RecordLiteral []) (k1 :| ks) v
    let fields' = Map.insert k0 nested (Map.fromList fields)
    return (sortedRecordType (Map.toList fields'))
inferWith ctx e (DescendOptional :| []) v = do
    eT <- inferType ctx e
    vT <- inferType ctx v
    case nf eT of
        Application (Builtin Optional) t0 -> do
            guard (eq t0 vT)
            return (nf eT)
        _ -> Nothing
inferWith ctx e (DescendOptional :| (k:ks)) v = do
    eT <- inferType ctx e
    case nf eT of
        Application (Builtin Optional) t0 -> do
            let ctx1 = extend "x" (nf t0) ctx
            inner <- inferWith ctx1 (Variable "x" 0) (k :| ks) (shift 1 "x" 0 v)
            guard (eq t0 inner)
            guard (not (Set.member "x" (freeVars v)))
            return (nf eT)
        _ -> Nothing

freeVars :: Expression -> Set Text
freeVars (Variable x 0) = Set.singleton x
freeVars (Variable _ _) = Set.empty
freeVars (Lambda x t a0) =
    let v0 = freeVars t
        a1 = shift 1 x 0 a0
        v1 = Set.delete x (freeVars a1)
    in  Set.union v0 v1
freeVars (Forall x t a0) =
    let v0 = freeVars t
        a1 = shift 1 x 0 a0
        v1 = Set.delete x (freeVars a1)
    in  Set.union v0 v1
freeVars (Let x (Just t) b0 a0) =
    let v0 = freeVars t
        a1 = shift 1 x 0 a0
        v1 = Set.delete x (freeVars a1)
        v3 = freeVars b0
    in  v0 `Set.union` v1 `Set.union` v3
freeVars (Let x Nothing b0 a0) =
    let a1 = shift 1 x 0 a0
        v1 = Set.delete x (freeVars a1)
        v3 = freeVars b0
    in  v1 `Set.union` v3
freeVars e =
    Set.unions (map freeVars (subterms e))

subterms :: Expression -> [Expression]
subterms expression =
    case expression of
        Variable{} -> []
        Lambda _ a b -> [a, b]
        Forall _ a b -> [a, b]
        Let _ maybeType a b -> maybe [] pure maybeType ++ [a, b]
        If a b c -> [a, b, c]
        Merge a b maybeType -> a : b : maybe [] pure maybeType
        ToMap a maybeType -> a : maybe [] pure maybeType
        EmptyList a -> [a]
        NonEmptyList (t :| ts) -> t : ts
        Annotation a b -> [a, b]
        Operator a _ b -> [a, b]
        Application a b -> [a, b]
        Field a _ -> [a]
        ProjectByLabels a _ -> [a]
        ProjectByType a b -> [a, b]
        Completion a b -> [a, b]
        Assert a -> [a]
        With a _ b -> [a, b]
        DoubleLiteral{} -> []
        NaturalLiteral{} -> []
        IntegerLiteral{} -> []
        TextLiteral (Chunks chunks _) -> map snd chunks
        BytesLiteral{} -> []
        DateLiteral{} -> []
        TimeLiteral{} -> []
        TimeZoneLiteral{} -> []
        RecordType fields -> map snd fields
        RecordLiteral fields -> map snd fields
        UnionType alts -> [ t | (_, Just t) <- alts ]
        ShowConstructor a -> [a]
        Import (Remote _ (Just headers)) _ _ -> [headers]
        Import{} -> []
        Some a -> [a]
        Builtin{} -> []
        Constant{} -> []
```
