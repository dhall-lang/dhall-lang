# How to translate recursive code to Dhall

> Embed recursive data types and functions in a non-recursive language

The Dhall configuration language provides built-in support only for one
recursive data structure: `List`. The language does not provide built-in
support for user-defined recursive types, recursive values, or recursive
functions.

For example, a definition of a list with integer values in Haskell could look like this:

```haskell
data ListInt = Nil | Cons Int ListInt
```

This code is not supported in Dhall because it is a recursive definition:
the type `ListInt` is being defined using the type `ListInt` itself. A corresponding definition in Dhall syntax is rejected by Dhall:

```dhall
⊢ let ListInt : Type = < Nil | Cons : { head: Integer, tail : ListInt } > in ListInt

Error: Unbound variable: ListInt
```

A simple recursive function can be defined in Haskell like this:

```haskell
step :: Int -> Int
step n = if n == 0 then 0 else step (n - 1)
```

A corresponding definition in Dhall syntax would be:

```dhall
⊢ let step = \(n: Natural) -> if Natural/isZero n then 0 else step (Natural/subtract 1 n) in step

Error: Unbound variable: step
```

This code is rejected by Dhall because `step` may not be used inside its own definition.

## Why is recursion not supported directly?

Prohibiting direct recursion is one of the core design decisions in Dhall.
It ensures that any well-typed Dhall program will always evaluate to a final value (called the "normal form") within a finite time.
Because of that limitation, it is simply not possible to write a Dhall program that type-checks but enters an infinite loop while evaluating.
This is a valuable property for a configuration language.

Nevertheless, one can still work with a wide range of recursive types and functions in Dhall.
Recursion must be encoded in a special way, ensuring _statically_ (before evaluation) that all recursive data structures are finite and all recursive functions
terminate.
This guide explains how to do encode recursion in Dhall, walking through examples of progressively increasing difficulty.

## How to implement recursive types: a general recipe

The main idea is to replace a recursive type definition by a more detailed description (called a "recursion scheme") showing how a value of the recursive type
may be constructed from simpler parts or from primitive values.
That description will be _itself_ non-recursive, and so it will be accepted by Dhall.
Then one needs to use a trick (known as the "Church encoding") that involves universally quantified types. That trick replaces a recursive type definition with
an equivalent but more complicated type whose definition is _not_ recursive.

We will now explain the Church encoding technique step by step.

### Step 1: from a recursive type definition to a recursion scheme

First, we rewrite a recursive type definition in the form `data T = F T` where `F` is a new type constructor called a "recursion scheme". We will need
to define `F` appropriately each time.

For example, the integer list type has this Haskell definition:

```haskell
data ListInt = Nil | Cons Int ListInt
```

We need to rewrite this definition as `data ListInt = F ListInt`, where `F` must be a suitable new type constructor. In this case, it is clear what `F` should
be:

```haskell
data F r = Nil | Cons Int r
```

We find `F r` by replacing all recursive instances of `ListInt` by the type parameter `r`. (Here the letter `r` reminds us of "recursion".)

If we substitute `ListInt` for `r` in `F r`, we obtain `Nil | Cons Int ListInt`, which is exactly the right-hand side of the original recursive definition. In
this way, `data ListInt = F ListInt` is the same as `data ListInt = Nil | Cons Int ListInt`. It shows that our choice of `F` is correct for `ListInt`.

The recursion scheme (`F`) describes all the possible ways of constructing a value of a recursive type. In this example, there are only two ways of constructing
a value of type `ListInt`: first, `Nil` is a value of type `ListInt`. Second, if we _somehow_ already have a value `r` of type `ListInt`, we can use `Cons` to
construct new values of type `ListInt`, for instance, `Cons -123 r`.

Because `F` is itself _not_ recursive, Dhall will accept its definition. The Dhall code for `F` is:

```dhall
let F = λ(r : Type) → < Nil | Cons : { head : Integer, tail : r } >
```

As another example, take a binary tree with integer leaf values:

```haskell
data TreeInt = Leaf Int | Branch TreeInt TreeInt
```

The corresponding recursion scheme `F` is defined by:

```haskell
data F r = Leaf Int | Branch r r
```

This `F` is a _non-recursive_ type constructor defined in Dhall by:

```dhall
let F = λ(r : Type) → < Leaf: Integer | Branch : { left : r, right : r } >
```

If a recursive data type has itself some type parameters, those type parameters will have to be added to `F` in addition to the type parameter `r`.

For example, a list with a type parameter can be defined in Haskell as:

```haskell
data List a = Nil | Cons a (List a)
```

The corresponding recursion scheme is:

```haskell
data F a r = Nil | Cons a r
```

The corresponding Dhall code is:

```dhall
let F = λ(a : Type) → λ(r : Type) →
   < Nil | Cons : { head : a, tail : r } >
```

A binary tree with a type parameter in Haskell:

```haskell
data Tree a = Leaf a | Branch (Tree a) (Tree a)
```

The corresponding recursion scheme `F` is:

```haskell
data F a r = Leaf a | Branch r r
```

The Dhall code is:

```dhall
let F = λ(a : Type) → λ(r : Type) →
   < Leaf : a | Branch : { left : r, right : r } >
```

As another example, consider a binary tree with two type parameters:

```haskell
data TreeAB a b = LeafA a | LeafB b | Branch (TreeAB a b) (TreeAB a b)
```

The corresponding recursion scheme is:

```haskell
data F a b r = LeafA a | LeafB b | Branch r r
```

The Dhall code is:

```dhall
let F = λ(a : Type) → λ(b : Type) → λ(r : Type) →
   < LeafA : a | LeafB : b | Branch : { left : r, right : r } >
```

A somewhat more complicated example is a "mutually recursive" type definition where two types are defined in terms of each other.
In Haskell:

```haskell
data Layer = Name String | OneLayer Layer | TwoLayers Layer2 Layer2
data Layer2 = Name2 String | ManyLayers [ Layer ]   
```

The type `Layer` is defined via itself and `Layer2`, while `Layer2` is defined via `Layer`.

We need two recursion schemes (`F` and `F2`) to describe this definition. In terms of the recursion schemes, the type definitions should look like this:

```haskell
data Layer = Layer (F Layer Layer2)
data Layer2 = Layer2 (F2 Layer Layer2)
```

We will achieve this formulation if we define  `F` and `F2` by:

```haskell
data F a b = Name String |  OneLayer a | TwoLayers b b
data F2 a b = Name2 String | ManyLayers [ a ]
```

The recursion schemes `F` and `F2` are non-recursive type constructors with two type parameters each. The Dhall code for this example is:

```dhall
let F = λ(a : Type) → λ(b : Type) → < Name : Text | OneLayer : b | TwoLayers: { left : b, right : b } >
let F2 = λ(a : Type) → λ(b : Type) → < Name2 : Text | ManyLayers : List a >
```

These examples show how to convert recursive type definitions into the corresponding recursion schemes. In this tutorial, we will often denote recursion schemes
by `F`. (For every example, there will be a different `F`.)
The definitions of `F`s are non-recursive and will be accepted by Dhall.

### Step 2: the Church encoding

Now we show the trick known as the "Church encoding". This trick uses a given recursion scheme `F` to define a special type that contains a universal
quantifier. In Dhall, that type is written as:

```dhall
let C = ∀(r : Type) → (F r → r) → r
```

It turns out that `C` is equivalent to a type `T` defined recursively by `T = F T`.
The type `C` may be called a "Church encoding of `T`".
Because the definition of `C` is not recursive, it will be accepted by Dhall.

With this trick, we have achieved the goal of defining the type `T` without recursion.
Let us now discuss this trick in more detail.

#### The meaning of universal quantifiers

Carefully note that the Church encoding uses the universal quantifier (`∀`) and not the `λ` symbol. Let us explain the difference in some detail using an
example.

We will compare the Dhall expressions `λ(r : Type) → Optional r` and `∀(r : Type) → Optional r`.

The expression `λ(r : Type) → Optional r` is equivalent to just the type constructor `Optional`. It is a function operating on types: it needs to be applied to
a particular type in
order to produce a result type. The expression `λ(r : Type) → Optional r` is itself of type `Type → Type`. This is not what we need for the Church encoding.

The expression `∀(r : Type) → Optional r` has type `Type`. It is the type of functions having a type parameter `r` and returning a value of type `Optional r`.
So, a value of type `∀(r : Type) → P r` must be a function expression of the form `λ(r : Type) → ...`. The code of such a function must work for all types `r`
and
produce some value of type `Optional r`, no matter what the type `r` might be. The Church encoding uses functions of this kind.

#### Additional type parameters and mutual recursion

If the recursion scheme `F` has additional type parameters `a`, `b`, etc., we need to write the Church encoding with all those parameters next to `F`:

```dhall
let C = λ(a : Type) → λ(b : Type) → ∀(r : Type) → (F a b r → r) → r
```

For mutually recursive definitions with several recursion schemes, we write a Church-encoded type for each mutually recursive type separately. For instance, if
we have two mutually recursive types with recursion schemes `F` and `F2`, we write:

```dhall
let C = ∀(a : Type) → ∀(b : Type) → (F a b → a) → (F2 a b → b) → a
```

for the first type and:

```dhall
let C2 = ∀(a : Type) → ∀(b : Type) → (F a b → a) → (F2 a b → b) → b
```

for the second type.
The result is a pair of types that solve the "system of type equations" `C = F C C2`, `C2 = F2 C C2`.

All these type definitions (`C`, `C2`) are not recursive because they just use previously defined (and also non-recursive) type constructors (`F`, `F2`, etc.)
in some type expressions with
quantifiers. However, it turns out that those definitions are equivalent to recursive data types, with the additional guarantee that the resulting data
structures are always finite.

It is far from obvious why a type of the form `∀(r : Type) → (F r → r) → r` is equivalent to a recursively defined type `T = F T`.
A mathematical proof of that property is given in the
paper ["Recursive types for free"](https://homepages.inf.ed.ac.uk/wadler/papers/free-rectypes/free-rectypes.txt) by P. Wadler.
In this tutorial, we will focus on the practical use of Church encoding.

Let us write the Dhall code for the examples shown in the previous section.

The type `ListInt` (a list with integer values):

```dhall
let F = λ(r : Type) → < Nil | Cons : { head : Integer, tail : r } >
let ListInt = ∀(r : Type) → (F r → r) → r
```

A binary tree with integer leaf values:

```dhall
let F = λ(r : Type) → < Leaf: Integer | Branch : { left : r, right : r } >
let TreeInt = ∀(r : Type) → (F r → r) → r
```

A list with values of type `a`:

```dhall
let F = λ(a : Type) → λ(r : Type) →
   < Nil | Cons : { head : a, tail : r } >
let ListA = λ(a : Type) → ∀(r : Type) → (F r → r) → r
```

This `ListA` is actually equivalent to Dhall's built-in `List`.

A binary tree with leaf values of type `a`:

```dhall
let F = λ(a : Type) → λ(r : Type) →
   < Leaf : a | Branch : { left : r, right : r } >
let TreeA = λ(a : Type) → ∀(r : Type) → (F a r → r) → r
```

A binary tree with two type parameters:

```dhall
let F = λ(a : Type) → λ(b : Type) → λ(r : Type) →
   < LeafA : a | LeafB : b | Branch : { left : r, right : r } >
let TreeAB = λ(a : Type) → λ(b : Type) → ∀(r : Type) → (F a b r → r) → r
```

Two mutually recursive types `Layer` and `Layer2`:

```dhall
let F = λ(a : Type) → λ(b : Type) → < Name : Text | OneLayer : b | TwoLayers: { left : b, right : b } >
let F2 = λ(a : Type) → λ(b : Type) → < Name2 : Text | ManyLayers : List a >
let Layer = ∀(a : Type) → ∀(b : Type) → (F a b → a) → (F2 a b → b) → a
let Layer2 = ∀(a : Type) → ∀(b : Type) → (F a b → a) → (F2 a b → b) → b
```

### Step 3: Working with recursive types

We have shown a recipe for converting any recursive type definition into a recursion scheme and finally into non-recursive (but more complicated) Church-encoded
type.

It takes some work to figure out how to write values of those types and to develop techniques for programming with Church encodings more conveniently.

#### Example: `ListInt`

To learn those techniques, we begin by looking at the Church-encoded type `ListInt` whose Dhall code is:

```dhall
let F = λ(r : Type) → < Nil | Cons : { head : Integer, tail : r } >
let ListInt = ∀(r : Type) → (F r → r) → r
```

How can we implement a value `x` of this type? We need to write a function that takes an arbitrary type `r` and an arbitrary function `frr` of type `F r → r`
and then returns a value of type `r`.

```dhall
let x : ListInt = λ(r : Type) → λ(frr : F r → r) → ... -- Some code here.
```

The code of this function must work for any type `r` whatsoever. How can we produce a value of type `r` if we do not even know what type it is?

We can do that only by applying the function `frr` to some argument of type `F r`. So, now we need to see how we could produce a value of type `F r`.

We notice that `F r` is a union type with one of the possibilities being just `Nil`.
So, we can give that value `Nil` as an argument to `frr`. This will give us a value of type `r` that we can return. So, one possibility of implementing a value
of type `ListInt` is `x0` defined like this:

```dhall
let x0 : ListInt = λ(r : Type) → λ(frr : F r → r) →
    let fr0 = < Nil | Cons : { head : Integer, tail : r } >.Nil
    let r0 = frr fr0
    in r0
  in x0
```

The other variant of the union type `F r` is `Cons` containing a record with an `Integer` and a previously known value of type `r`. But where will we get that
previous value? So far, we could only get the value `x0` defined just above. Let us build upon that code. Choosing the integer value arbitrarily as `-123`, we
write:

```dhall
let x1 : ListInt = λ(r : Type) → λ(frr : F r → r) →
    let fr0 = < Nil | Cons : { head : Integer, tail : r } >.Nil
    let r0 = frr fr0
    let fr1 = < Nil | Cons : { head : Integer, tail : r } >.Cons {head = -123, tail = r0 }
    let r1 = frr fr1
    in r1 
  in x1
```

We have written the code in a verbose way in order to show the pattern of how to build up more values of type `r` (denoted `r0`, `r1`, ...). We can continue in
the same way to encode more complicated values of type `ListInt`:

```dhall
let x2 : ListInt = λ(r : Type) → λ(frr : F r → r) →
    let fr0 = < Nil | Cons : { head : Integer, tail : r } >.Nil
    let r0 = frr fr0
    let fr1 = < Nil | Cons : { head : Integer, tail : r } >.Cons {head = -123, tail = r0 }
    let r1 = frr fr1
    let fr2 = < Nil | Cons : { head : Integer, tail : r } >.Cons {head = +456, tail = r1 }
    let r2 = frr fr2
    in r2 
  in x2
```

We find that we can implement values of type `ListInt` if we just choose a list of zero or more integers (`-123`, `+456`, etc.) and write code as shown above
with zero or more similar-looking steps. Each step contains an arbitrary integer value and computes a new value of type `r` out of a previous value. In this
way, the type `ListInt` represents (possibly empty) lists of integer values. There is no other way of constructing a value of type `ListInt`.

Could we hide the verbose boilerplate and make working with `ListInt` easier? Let us introduce "constructors" `nil` and `cons` so that the code for `x2` will be
just `cons +456 (cons -123 nil)`.

The `nil` constructor is the same as `x0` shown above. The `cons` constructor encapsulates one step of the boilerplate code:

```dhall
let F = λ(r : Type) → < Nil | Cons : { head : Integer, tail : r } >

let ListInt = ∀(r : Type) → (F r → r) → r

let nil : ListInt = λ(r : Type) → λ(frr : F r → r) →
    frr (F r).Nil

let cons: Integer → ListInt → ListInt = λ(head : Integer) → λ(tail : ListInt) →
    λ(r : Type) → λ(frr : F r → r) →
        let fr = (F r).Cons { head = head, tail = tail r frr }
            in frr fr

    in cons +456 (cons -123 nil)
```

In this code, it is important that we are allowed to write `tail r frr` while computing `fr`. The value `tail : ListInt` is a function whose first argument is a
type. We are using that function with the type `r` that we have received in the body of `cons`. We are allowed to do this because `tail`, being a value of
type `ListInt`, is a function that can work with arbitrary types `r`.

Let us also implement a `foldRight` function for `ListInt`. That function serves as a general "aggregation" algorithm, converting a list of integers into an
aggregated value of some type. The type signature of `foldRight` is:

```dhall
foldRight : ∀(r : Type) → ∀(init : r) → ∀(update : Integer → r → r) → ListInt → r
```

The arguments of `foldRight` are an arbitrary result type `r`, an initial value of type `r`, and an "updater" function of type `r → Integer → r`. The arguments
of the updater function are the currently aggregated value of type `r` and a next integer from the list. The result is the next aggregated value (of type `r`).

The function `foldRight` should iterate over all integers in the list and keep updating the current result value (of type `r`) until the list is finished and a
final value is obtained.

It is perhaps surprising that the code of `foldRight` is _non-recursive_:

```dhall
let foldRight : ∀(r : Type) → ∀(init : r) → ∀(update : Integer → r → r) → ListInt → r =
  λ(r : Type) → λ(init : r) → λ(update : Integer → r → r) → λ(list : ListInt) →
    let consR : { head : Integer, tail : r } → r = λ(fr : { head : Integer, tail : r }) → update fr.head fr.tail
    let frr : F r → r = λ(fr : F r) → merge { Nil = init, Cons = consR } fr
        in list r frr
    in foldRight
```

This code merely calls the given value `list : ListInt` on a certain function `fr : F r → r`. That function is constructed out of the given arguments `init`
and `update`.

Because `foldRight` is non-recursive, Dhall accepts that function.

In this way, Dhall is able to construct integer lists and also to run loops over them, computing an aggregated value using `foldRight`.

If we take, for example, the list `l = cons +456 (cons -123 nil)`,
which corresponds to the ordinary list `[+456, -123]`, then `foldRight r init update l` starts
from the initial value `init : r`. Then it takes the right-most element of the list (`-123`) and computes `update -123 init`. Then it
computes `update +456 (update -123)`. This fits with the name "fold right": the computation starts from the right-most element of the list and iterates to the
left.

As an example, we use `foldRight` to implement a function that converts a `ListInt` value into the built-in list type `List Integer`:

```dhall
let toList : ListInt → List Integer = λ(list : ListInt) →
    foldRight (List Integer) ([]: List Integer) (λ(x: Integer) → λ(r : List Integer) → r # [ x ]) list
    in toList (cons +456 (cons -123 nil))
```

The result is computed as `[ +456, -123 ]`.

## Where did the recursion go?

The technique of Church encoding may be perplexing. If we are actually working with recursive types and recursive functions, why do we no longer
see any recursion in the code? In `foldRight`, why is there no code that iterates over a list of integers in a loop?

An answer is found by comparing the codes for the values `x0`, `x1`, and `x2` shown in the previous section when working with `ListInt`. The values `x0`, `x1`,
and `x2` are functions whose second argument is a function `frr : F r → r`. The code for `x0` calls that function only once; the code for `x1` calls that
function twice; and the code for `x2` calls that function three times.

This explains why `foldRight` is non-recursive. The code of `foldRight` merely prepares a function `frr` and passes it to the given value of type `ListInt`. If
we
run `foldRight` on `x2`, it is the code of `x2` that will call the function `frr` three times. But there is actually no loop in `x2`. It is just hard-coded in
the
function `x2` to apply `frr` three times in a row.

A list of 1000 integers will be represented by a function that takes an argument `frr : F r → r` and applies `frr` a thousand times
to some arguments.
This is because the only way of creating a list of 1000 integers is to create an expression such as `cons 1 (cons 2 (cons 3 (... (cons 1000 nil)))...)`.
It is _hard-coded_ in that expression to call the `cons` function 1000 times.

In this way, the Church encoding avoids loops and allows us to represent iterative computations without recursion.

Data structures that contain 1000 integers are replaced by functions that are hard-coded to call their argument 1000 times. In this way, the Church encoding
guarantees that all recursive structures will be finite and all operations on those structures will terminate. It is for
that reason that Dhall is able to accept Church encodings of recursive types without compromising any safety guarantees.

## Church encoding and `fold` types are equivalent

How to generalize `foldRight` from `ListInt` to arbitrary recursive types? That is done via an equivalence relationship between a Church-encoded type, such
as `ListInt`, and the type of the corresponding `foldRight` function.

Looking at the type of `foldRight` for `ListInt`, we note that the type of functions `F r → r` is equivalent to a pair of a value of type `r` and a function
with the type signature `Integer → r → r`.

To see why, consider how we can implement any function of type `F r → r` (equivalently, `< Nil | Cons : { head : Integer, tail : r } > → r`). That function must
handle two cases: when the argument is `Nil` and when the argument is `Cons`. When the argument is `Nil`, the function must return a value of type `r`. That
value must be _hard-coded_ within the function body (because `Nil` contains no data from which we could create a value of type `r`). When the argument
is `Cons { head = h, tail = t }` then the function must somehow create a value of type `r` out of that data. This ability is equivalent to having a function of
type `Integer → r → r`.

So, the data required to implement a function of type `F r → r` is a pair of a value of type `r` and a function of type `Integer → r → r`.

This is the same as the data contained in the arguments of `foldRight` (that is, `init` and `update`).

So, the type of `foldRight` can be rewritten equivalently as:

```dhall
∀(r : Type) → (F r → r) → ListInt → r
```

We can then swap the order of curried arguments to obtain another equivalent type expression:

```dhall
ListInt → ∀(r : Type) → (F r → r) → r
```

This is exactly the same as the type `ListInt → ListInt`. The code of `foldRight` is equivalent to just an _identity function_ of that type. No wonder it is
non-recursive!

Keeping this in mind, we may say that the Church encoding trick consists of encoding recursive types via the types of their `fold` functions.

The same argument will hold for any recursive types, including recursive types with extra type parameters. Given a recursion scheme `F` and the corresponding
Church-encoded type `C = ∀(r : Type) → (F r → r)  → r`, a `fold` function can be implemented in general as an identity function of type `C → C` adapted to a
typical type signature of `fold`-like functions:

```dhall
∀(r : Type) → ∀(frr : F r → r) → C → r
```

In practice, it is easier to use a value of type `C` directly as a `fold` function. We will see an example of that usage shortly.

## The `build` function

In the previous section we showed the constructors `nil` and `cons` for the `ListInt` type. What is the corresponding technique for an arbitrary Church-encoded
type with a given recursion scheme `F`?

Both the constructors `nil` and `cons` have types of the form "something that returns a `ListInt` value":

```dhall
let nil : ListInt = ...
let cons: Integer → ListInt → ListInt = ...
```

To make this pattern more apparent, we can rewrite those types equivalently as functions from some record types to `ListInt`:

```dhall
let nil1 : {} -> ListInt = ...
let cons1: { x : Integer, l : ListInt } → ListInt = ...
```

The two constructors `nil1` and `cons1` are equivalent to a single function `nil1cons1 : < n : { } | c : { x : Integer, l : ListInt } > → ListInt`.

The argument of that function has type `< n : { } | c : { x : Integer, l : ListInt } >`, which is equivalent to `F ListInt` because `F ListInt`
means `< Nil | Cons : { head : Integer, tail : ListInt } >`.

So, we find that the two constructors `nil` and `cons` may be replaced by a single value of type `F ListInt → ListInt`.

Of course, practical programming is more convenient with the pair of constructors `nil` and `cons`.
But the equivalence of that pair (`nil` and `cons`) to a single value of type `F ListInt → ListInt` shows us how to generalize this technique from `ListInt` to
an arbitrary recursive type.

Namely, the full set of constructors for an arbitrary recursion scheme `F` and the corresponding Church-encoded type `C = ∀(r : Type) → (F r → r) → r` is just a
specific value of type `F C → C`.

It turns out that there is a unique value of type `F C → C` that satisfies certain required properties (which are beyond the scope of this tutorial). We will
denote that value by `build: F C → C`. It is that `build` function that encapsulates all the basic constructors that are used for building values the type `C`.

A general implementation of `build` depends on having the standard `fmapF` function for the type constructor `F`. So, this technique only works when `F` is a
covariant type constructor. But this is always true in all practical cases.

The Dhall code for `build` is:

```dhall
let F = ∀(r : Type) → ... -- Define it here. This is non-recursive.
let fmapF : ∀(a : Type) → ∀(b : Type) → (a → b) → F a → F b = ... -- Define it here. This is non-recursive.
let C = ∀(r : Type) → (F r → r) → r
let build : F C → C = λ(fc : F C) → λ(r : Type) → λ(frr : F r → r) →
    let c2r : C → r = λ(c : C) → c r frr
    let fr : F r = fmapF C r c2r fc
    in frr fr
  in build
```

In Dhall, the only built-in recursive data type is `List`. The corresponding `fold` and `build` functions are the built-in symbols `List/fold` and `List/build`.

### Example of using `fold` and `build`: pretty-printing a binary tree

We have shown how to define the `fold` and `build` functions for any Church-encoded type, given the recursion scheme `F` and the corresponding `fmapF` function.
When working with a specific type, such as a binary tree, it is often convenient to implement specific constructors for that type instead of using the
generic `build` function.

As an example, we will now implement a function that converts a `TreeInt` value into a string in a LISP-like format, such as `"(+1 (+2 +3))"`.

As we have seen before, `TreeInt` is Church-encoded via the recursion scheme `F` defined by:

```dhall
let F = λ(r : Type) → < Leaf: Integer | Branch : { left : r, right : r } >

let TreeInt = ∀(r : Type) → (F r → r) → r
```

The (non-recursive!) `fmapF` function for `F` is:

```dhall
let fmapF : ∀(a : Type) → ∀(b : Type) → (a → b) → F a → F b =
    λ(a : Type) → λ(b : Type) → λ(f : a → b) → λ(fa : F a) → merge {
      Leaf = (F b).Leaf,
      Branch = λ(branch : { left : a, right : a }) → (F b).Branch { left = f branch.left, right = f branch.right }
    } fa
```

To create `TreeInt` values, we need a `build` function. The general code for `build` is reduced to:

```dhall
let build : F TreeInt → TreeInt =
  λ(fc : F TreeInt) → λ(r : Type) → λ(frr : F r → r) →
     frr (fmapF TreeInt r (λ(c : TreeInt) → c r frr) fc)
```

or equivalently:

```dhall
let build : F TreeInt → TreeInt =
  λ(fc : F TreeInt) → λ(r : Type) → λ(frr : F r → r) →
    frr (merge {
        Leaf = (F r).Leaf,
        Branch = λ(branch : { left : TreeInt, right : TreeInt }) → (F r).Branch { left = branch.left r frr, right = branch.right r frr }
    } fc)
```

A function of type `F TreeInt → TreeInt` is equivalent to a pair of functions of types `Integer → TreeInt` and `TreeInt → TreeInt → TreeInt` respectively. Let
us denote these two functions by `leaf` and `branch`. The code of those functions can be read off the above code of `build` if we apply `build` to arguments of
type `(F TreeInt).Leaf` and `(F TreeInt).Branch`:

```dhall
let leaf : Integer → TreeInt = λ(x : Integer) → λ(r : Type) → λ(frr : F r → r) → frr ((F r).Leaf x)
let branch : TreeInt → TreeInt → TreeInt = λ(left : TreeInt) → λ(right : TreeInt) → λ(r : Type) → λ(frr : F r → r) → frr ((F r).Branch { left = left r frr, right = right r frr })
```

Now we can construct values of type `TreeInt`:

```dhall
let example1 = branch (leaf +1) (branch (leaf +2) (leaf +3))
```

To implement a pretty-printer for `TreeInt`, we use values of type `TreeInt` as `fold` functions. The type signature of the Church encoding takes care of
recursion for us. We only need to implement a non-recursive function `frr : F Text → Text` that describes how to create the text representation for a larger
tree — either from a leaf or from the text representations of the two subtrees. We enclose values in parentheses and add a space between two subtrees:

```dhall
let print : TreeInt → Text = λ(tree: ∀(r : Type) → (F r → r) → r) →
  let frr : F Text → Text = λ(fr : F Text) →
    merge {
        Leaf = λ(t : Integer) → Integer/show t,
        Branch = λ(b : { left : Text, right : Text }) → "(${b.left} ${b.right})" } fr
    in tree Text frr
    
let test = assert : print example1 === "(+1 (+2 +3))"
```

Similarly to this example, one can use `fold` to implement a wide range of recursive functions on trees.

## Pattern matching on Church-encoded values

When working with recursive types in ordinary functional languages, one often uses pattern matching. For example, here is a simple function that detects whether
a given tree is a single leaf:

```haskell
data TreeInt = Leaf Int | Branch TreeInt TreeInt

isSingleLeaf: TreeInt -> Bool
isSingleLeaf t = case t of
    Leaf _ -> true
    Branch _ _ -> false
```

Another example is a function that checks whether the first element of a list exists:

```haskell
headMaybe :: [a] -> Maybe a
headMaybe []     = Nothing
headMaybe (x:xs) = Just x
```

The Dhall translation of `TreeInt` and `ListInt` are Church-encoded types:

```dhall
let F = λ(r : Type) → < Leaf: Integer | Branch : { left : r, right : r } >
let TreeInt = ∀(r : Type) → (F r → r) → r
```

and

```dhall
let F = λ(r : Type) → < Nil | Cons : { head : Integer, tail : r } >
let ListInt = ∀(r : Type) → (F r → r) → r
```

Values of type `TreeInt` and `ListInt` are functions, so we cannot perform pattern matching on such values. How can we implement functions like `isSingleLeaf`
and `headMaybe` in Dhall?

The general method for translating pattern matching into Church-encoded types `C` consists of two steps. The first step is to define a function we will
call `unroll`, of type `C → F C`. This function is the inverse of the function `build : F C → C` from the previous subsection.

The Dhall code for `unroll` is:

```dhall
let F = λ(r : Type) → ... -- Define it here. This is non-recursive.
let fmapF : ∀(a : Type) → ∀(b : Type) → (a → b) → F a → F b = ... -- Define it here. This is non-recursive.
let C = ∀(r : Type) → (F r → r) → r
let unroll : C → F C =
  let fmapBuild : F (F C) → F C = fmapF (F C) C build -- Use the definition of `build` above.
    in λ(c : C) → c (F C) fmapBuild
  in unroll
```

A rigorous proof that `unroll` and `build` are inverse functions is shown in the
paper ["Recursive types for free"](https://homepages.inf.ed.ac.uk/wadler/papers/free-rectypes/free-rectypes.txt), which is beyond the scope of this tutorial.

Because of those functions, the types `C` and `F C` are equivalent (isomorphic) to each other. Any data of type `C` can be mapped
into data of type `F C` and back, without loss of information. In that sense, the type `C` satisfies the "type equation" `C = F C`. This is one way of defining
rigorously the meaning of recursive types written in Haskell as `data T = F T`.

The second step is to apply `unroll` to the value on which we need to use pattern matching. The result will be a value of type `F C`, which will be typically a
union type. With values of that type, we can just use the ordinary pattern matching (`merge` in Dhall).

This technique allows us to translate `isSingleLeaf` and `headMaybe` to Dhall. Let us look at some examples.

For `C = TreeInt`, the type `F C` is the union type `< Leaf: Integer | Branch : { left : TreeInt, right : TreeInt } >`. The function `isSingleLeaf` is
implemented via pattern matching on that type:

```dhall
let F = λ(r : Type) → < Leaf: Integer | Branch : { left : r, right : r } >

let TreeInt = ∀(r : Type) → (F r → r) → r

let fmapF : ∀(a : Type) → ∀(b : Type) → (a → b) → F a → F b =
    λ(a : Type) → λ(b : Type) → λ(f : a → b) → λ(fa : F a) → merge {
      Leaf = (F b).Leaf,
      Branch = λ(branch : { left : a, right : a }) → (F b).Branch { left = f branch.left, right = f branch.right }
    } fa

-- Assume the definition of `unroll` as shown above.

let isSingleLeaf : TreeInt → Bool = λ(c : TreeInt) →
    merge {
      Leaf = λ(_ : Integer) → true,
      Branch = λ(_ : { left : TreeInt, right : TreeInt }) → false
    } (unroll c)
  in isSingleLeaf
```

For `C = ListInt`, the type `F C` is the union type `< Nil | Cons : { head : Integer, tail : ListInt } >`. The function `headOptional` that replaces
Haskell's `headMaybe` is written in Dhall like this:

```dhall
let F = λ(r : Type) → < Nil | Cons : { head : Integer, tail : r } >

let ListInt = ∀(r : Type) → (F r → r) → r

let fmapF : ∀(a : Type) → ∀(b : Type) → (a → b) → F a → F b =
    λ(a : Type) → λ(b : Type) → λ(f : a → b) → λ(fa : F a) → merge {
      Nil = (F b).Nil,
      Cons = λ(pair : { head : Integer, tail : a }) → (F b).Cons (pair // { tail = f pair.tail })
    } fa

-- Assume the definition of `unroll` as shown above.

let headOptional : ListInt → Optional Integer = λ(c : ListInt) →
    merge {
      Cons = λ(list : { head : Integer, tail : ListInt }) → Some (list.head),
      Nil = None Integer
    } (unroll c)
  in headOptional (cons -456 (cons +123 nil))
```

The result is computed as `Some -456`.

## Other operations on Church-encoded data types

Recursive data types such as lists and trees support certain useful operations such as `concat`, `filter`, or `traverse`. Normally, those operations are
implemented via recursive code. To use those operations in Dhall, we need to avoid using recursion and instead use the Church-encoded data (that is, a fold-like
function) as the provider of iteration. Let us show some examples of how this can be done.

#### Concatenating and reversing non-empty lists

We will implement `concat` and `reverse` functions for _non-empty_ lists using a Church encoding.

Non-empty lists (`NEL: Type → Type`) can be defined recursively as:

```haskell
data NEL a = One a | Cons a (NEL a)
```

The recursion scheme corresponding to this definition is:

```haskell
data F a r = One a | Cons a r
```

Convert this definition to Dhall and write the corresponding Church encoding:

```dhall
let F = ∀(a : Type) → ∀(r : Type) → < One : a |  Cons : { head : a, tail: r } >
let NEL = ∀(a : Type) → ∀(r : Type) → (F a r → r) → r
```

It will be more convenient to rewrite the type `NEL` without using union or record types. An equivalent definition is:

```dhall
let NEL = λ(a : Type) → ∀(r : Type) → (a → r) → (a → r → r) → r
```

The standard constructors for `NEL` are:

- a function (`one`) that creates a list of one element
- a function (`cons`) that prepends a given value of type `a` to a list of type `NEL a`

Non-empty list values can be now built as `cons Natural 1 (cons Natural 2 (one Natural 3))` and so on.

```dhall
let one : ∀(a : Type) → a → NEL a =
    λ(a : Type) → λ(x : a) → λ(r : Type) → λ(ar : a → r) → λ(_ : a → r → r) → ar x
let cons : ∀(a : Type) → a → NEL a → NEL a =
    λ(a : Type) → λ(x : a) → λ(prev : NEL a) → λ(r : Type) → λ(ar : a → r) → λ(arr : a → r → r) → arr x (prev r ar arr)
let example1 : NEL Natural = cons Natural 1 (cons Natural 2 (one Natural 3))
let example2 : NEL Natural = cons Natural 3 (cons Natural 2 (one Natural 1))
```

The folding function is just an identity function:

```dhall
let foldNEL : ∀(a : Type) → NEL a → ∀(r : Type) → (a → r) → (a → r → r) → r =
    λ(a : Type) → λ(nel : NEL a) → nel
```

To see that this is a "right fold", apply `foldNEL` to some functions `ar : a → r` and `arr : a → r → r` and a three-element list such as `example1`. The result
will be `arr 1 (arr 2 (ar 3))`; the first function evaluation is at the right-most element of the list.

Folding with `one` and `cons` gives again the initial list:

```dhall
assert : example1 === foldNEL Natural example1 (NEL Natural) (one Natural) (cons Natural)
```

To concatenate two lists, we right-fold the first list and substitute the second list instead of the right-most element:

```dhall
let concatNEL: ∀(a : Type) → NEL a → NEL a → NEL a =
    λ(a : Type) → λ(nel1 : NEL a) → λ(nel2 : NEL a) →
        foldNEL a nel1 (NEL a) (λ(x : a) → cons a x nel2) (cons a)
let test = assert : concatNEL Natural example1 example2 === cons Natural 1 (cons Natural 2 (cons Natural 3 (cons Natural 3 (cons Natural 2 (one Natural 1)))))
```

To reverse a list, we right-fold over it and accumulate a new list by appending elements to it.

So, we will need a new constructor (`snoc`) that appends a given value of type `a` to a list of type `NEL a`, rather than prepending as `cons` does.

```dhall
let snoc : ∀(a : Type) → a → NEL a → NEL a =
    λ(a : Type) → λ(x : a) → λ(prev : NEL a) →
    foldNEL a prev (NEL a) (λ(y : a) → cons a y (one a x)) (cons a)
let test = assert example1 === snoc Natural 3 (snoc Natural 2 (one Natural 1))
```

Now we can write the reversing function:

```dhall
let reverseNEL : ∀(a : Type) → NEL a → NEL a =
    λ(a : Type) → λ(nel : NEL a) → foldNEL a nel (NEL a) (one a) (snoc a)
let test = assert : reverseNEL Natural example1 === example2
let test = assert : reverseNEL Natural example2 === example1
```

### Sizing a Church-encoded type constructor

The functions `concatNEL` and `reverseNEL` shown in the previous section are specific to list-like sequences and cannot be straightforwardly generalized to
other recursive types, such as trees.

However, a number of useful functions (such as `filter`, `join`, `traverse`) can be implemented generally for any Church-encoded data type.
Within this tutorial's limited scope, we just look at some functions that compute the total size and the maximum depth of a data structure.

Suppose we are given an arbitrary recursion scheme `F` with two type parameters. It defines a type constructor `C` via Church encoding as:

```dhall
let F = ∀(a : Type) → ∀(r : Type) → ...
let C = ∀(a : Type) → ∀(r : Type) → (F a r → r) → r
```

We imagine that a value `p : C a` is a data structure that stores zero or more values of type `a`.

The "total size" of `p` is the number of the values of type `a` that it stores. For example, if `p` is a list of 5 elements then the size of `p` is 5. The size
of a `TreeInt` value `branch (branch (leaf +10) (leaf +20)) (leaf +30)` is 3 because it stores three numbers.

The "maximum depth" of `p` is the depth of nested recursion required to obtain that value. For example, if `p` is a `TreeInt`
value `branch (branch (leaf +10) (leaf +20)) (leaf +30)` then the depth of `p` is 2. The depth of a single-leaf tree (such as `leaf +10`) is 0.

The goal is to implement these functions generically, for all Church-encoded data structures at once.

Both of those functions need to traverse the entire data structure and to accumulate a `Natural` value. Let us begin with `size`:

```dhall
let size : ∀(a : Type) → ∀(ca : C a) → Natural =
  λ(a : Type) → λ(ca : C a) →
    let sizeF : F a Natural → Natural = ??? 
    in ca Natural sizeF
```

The function `sizeF` should count the number of data items stored in `F a Natural`. The values of type `Natural` inside `F` represent the sizes of nested
instances of `C a`; those sizes have been already computed.

It is clear that the function `sizeF` will need to be different for each recursion scheme `F`.
For a given value `fa : f a Natural`, the result of `sizeF fa` will be equal to the number of values of type `a` stored in `fa` plus the sum of all natural
numbers stored in `fa`.

For example, non-empty lists are described by `F a r = < One : a | Cons : { head : a, tail: r } >`.
The corresponding `sizeF` function is:

```dhall
let sizeF : < One : a | Cons : { head : a, tail: Natural } > → Natural = λ(fa : < One : a | Cons : { head : a, tail: Natural } >) → merge {
      One = λ(x : a) → 1,
      Cons = λ(x : { head : a, tail: Natural }) → 1 + x.tail,
   } fa
```

Binary trees are described by `F a r = < Leaf : a | Branch : { left : r, right: r } >`.
The corresponding `sizeF` function is:

```dhall
let sizeF : < Leaf : a | Branch : { left : Natural, right: Natural } > → Natural = λ(fa : < Leaf : a | Branch : { left : Natural, right: Natural } >) → merge {
      Leaf = λ(x : a) → 1,
      Branch = λ(x : { left : Natural, right: Natural }) → x.left + x.right,
   } fa
```

Having realized that `sizeF` needs to be supplied for each recursion scheme `F`, we can implement `size` like this:

```dhall
let size : ∀(a : Type) → ∀(sizeF : ∀(b : Type) → F b Natural → Natural) → ∀(ca : C a) → Natural =
  λ(a : Type) → λ(ca : C a) → λ(sizeF : ∀(b : Type) → F b Natural → Natural) →
    ca Natural (sizeF a)
```

Turning now to the `depth` function, we proceed similarly and realize that the only difference is in the `sizeF` function.
Instead of `sizeF` described above, we need `depthF` with the same type signature `∀(b : Type) → F b Natural → Natural`.
For the depth calculation, `depthF` should return 1 plus the maximum of all values of type `Natural` that are present. If no such values are present, it just
returns 1.

For non-empty lists (and also for empty lists), the `depthF` function is the same as `sizeF` (because the recursion depth is the same as the list size).

For binary trees, the corresponding `depthF` function is:

```dhall
let depthF : < Leaf : a | Branch : { left : Natural, right: Natural } > → Natural = λ(fa : < Leaf : a | Branch : { left : Natural, right: Natural } >) → Natural/subtract 1 (merge {
      Leaf = λ(x : a) → 1,
      Branch = λ(x : { left : Natural, right: Natural }) → 1 + Natural/max x.left x.right,
   } fa)
```

Here the functions `Natural/max` and `Natural/subtract` come from the standard Dhall prelude.

## Performance

The performance of Church-encoded values is slower than that of directly implemented data structures. Although `fold` is a non-recursive function, its execution
time is linear in the data size because `fold` needs to traverse the entire data.

Another source of slowness is that a value `c` of a Church-encoded type `C` is a function that may call its parameter `frr : F r → r` many times. For example,
a `ListInt` value representing a list of 1000 integers is a function that calls its parameter `frr` 1000 times. So, evaluating any code that applies `c` to
some `frr` will have to perform 1000 evaluations of some function. The evaluation time is linear in the data size.

In particular, this applies to the code of `unroll`. Since we use `unroll` for pattern matching, any single pattern matching will be linear in the data size.
For instance, evaluating `headOptional` takes time that is linear in the length of the list, even though we may have expected that `headOptional` only needs to
examine the first element of the list. If `headOptional` is called repeatedly, evaluation may become quite slow.

The functions `concatNEL` and `reverseNEL` shown previously are quadratic in the length of the list because they use `foldNEL` twice nested.

## Examples

We will now illustrate the general recipe on more examples, showing how to translate recursive Haskell code into non-recursive Dhall definitions.

### Recursive record

Consider the following recursive Haskell code:

```haskell
-- Example0.hs

data Person = MakePerson
    { name     :: String
    , children :: [Person]
    }

example :: Person
example =
    MakePerson
    { name     = "John"
    , children =
        [ MakePerson { name = "Mary", children = [] }
        , MakePerson { name = "Jane", children = [] }
        ]
    }

everybody :: Person -> [String]
everybody p = name p : concatMap everybody (children p)

result :: [String]
result = everybody example
```

... which evaluates to:

```console
$ ghci Example0.hs
*Main> result
["John","Mary","Jane"]
```

To convert this to non-recursive Dhall code, we use the Church encoding recipe.

The first step is to define the recursion scheme. We name the type parameter `_Person` to avoid confusion, as we will want to define the type `Person` later.

```dhall
let F = λ(_Person : Type) → { name: String, children: List _Person }
```

This definition is non-recursive and will be accepted by Dhall.

We do not need to Church-encode `List` as it is natively supported in Dhall.

The next step is to define the Church-encoded type `Person`:

```dhall
let Person = ∀(_Person : Type) → (F _Person → _Person) → _Person
```

This replaces the Haskell definition of the type `Person`.

Next, we create the example value of type `Person`. We will name the function parameter `MakePerson` for clearer comparison with the Haskell code.

```dhall
let example : Person = λ(_Person : Type) → λ(MakePerson : F _Person → _Person) →
    MakePerson {
        name = "John",
        children =
            [ MakePerson { name = "Mary", children = [] : List _Person ]
            , MakePerson { name = "Jane", children = [] : List _Person ]
            ]
    } 
```

Now we need to translate the recursive function `everybody` to Dhall.
Actually, one cannot mechanically translate arbitrary recursive code to Church-encoded types.
This is possible only for certain classes of recursive functions.
One such class comprises "fold-like" functions: that is, functions whose recursive calls are performed directly on substructures of the recursive data type.
It turns out that `everybody` is a fold-like function. To see why, let us visualize how that function works on a given recursive data structure in Haskell:

```haskell
everybody (MakePerson { name = "John", children = [ child1, child2, ... ] })
  == "John" : concat [(everybody child1), (everybody child2), ...]
```

Here `child1` and `child2` are substructures of the recursive data type `Person`. Each of them is again of type `Person`.
Once the recursive calls `everybody child1`, `everybody child2`, etc., are finished, we obtain a list of values of type `List String`. The remaining computation
is equivalent to a function of type `String → List (List String) → List String`.

That type is equivalent to just `F (List String) → List String` and is exactly the same type as the argument of the Church-encoded type `Person` if we use the
type `List String` instead of the type argument `_Person`.

So, the recursive pattern can be replaced by a "fold" where we just need to supply a "folding function" of type `F (List String) → List String` as its argument.

The Dhall code is:

```dhall
let everybody : Person → List Text =
    let concat = http://prelude.dhall-lang.org/List/concat
    let foldingFunction : F (List Text) → List Text = λ(p : F (List Text)) →
        [ p.name ] # concat Text p.children
    in
    λ(x : Person) → x (List Text) foldingFunction
```

Put together the entire Dhall code and run it:

```dhall
-- example0.dhall
let Person
    : Type
    = ∀(_Person : Type) →
      ∀(MakePerson : { children : List _Person, name : Text } → _Person) →
        _Person

let example
    : Person
    = λ(_Person : Type) →
      λ(MakePerson : { children : List _Person, name : Text } → _Person) →
        MakePerson
          { children =
            [ MakePerson { children = [] : List _Person, name = "Mary" }
            , MakePerson { children = [] : List _Person, name = "Jane" }
            ]
          , name = "John"
          }

let everybody
    : Person → List Text
    = let concat = http://prelude.dhall-lang.org/List/concat

      in  λ(x : Person) →
            x
              (List Text)
              ( λ(p : { children : List (List Text), name : Text }) →
                  [ p.name ] # concat Text p.children
              )

let result
    : List Text
    = everybody example

in  result
```

... which evaluates to the same result as the Haskell code:

```console
$ dhall <<< './example0.dhall'
List Text

[ "John", "Mary", "Jane" ]
```

### Recursive union types

Recursive union types are covered by the same Church encoding recipe, except that their recursion schemes `F` will be union types.
Instead of writing lots of `merge` expressions, it is more convenient to replace the type of functions `F r → r` by a curried function type.

For example, consider the natural number type implemented recursively with two constructors `Zero` and `Succ` via this Haskell code:

```haskell
-- Example1.hs

import Numeric.Natural (Natural)

data Nat = Zero | Succ Nat

example :: Nat
example = Succ (Succ (Succ Zero))

toNatural :: Nat -> Natural
toNatural  Zero    = 0
toNatural (Succ n) = 1 + toNatural n

result :: Natural
result = toNatural example
```

... which produces this `result`:

```console
$ ghci Example1.hs
*Main> result
3
```

The Church encoding can be written without union types as:

```dhall
let Nat : Type = ∀(_Nat : Type) → ∀(Zero : _Nat) → ∀(Succ : _Nat → _Nat) → _Nat
```

So, the Haskell code corresponds to this Dhall code (we rename `_Nat` to just `Nat` for brevity):

```dhall
-- example1.dhall

let Nat
    : Type
    = ∀(Nat : Type) → ∀(Zero : Nat) → ∀(Succ : Nat → Nat) → Nat

let example
    : Nat
    = λ(Nat : Type) →
      λ(Zero : Nat) →
      λ(Succ : Nat → Nat) →
        Succ (Succ (Succ Zero))

let toNatural
    : Nat → Natural
    = λ(x : Nat) → x Natural 0 (λ(n : Natural) → 1 + n)

let result
    : Natural
    = toNatural example

in  result
```

... which produces the same `result`:

```console
$ dhall <<< './example1.dhall'
Natural

3
```

### Mutually recursive types

The above pattern generalizes to mutually recursive types, too. For example,
this Haskell code:

```haskell
-- Example2.hs

import Numeric.Natural

data Even = Zero | SuccEven Odd

data Odd = SuccOdd Even

example :: Odd
example = SuccOdd (SuccEven (SuccOdd Zero))

oddToNatural :: Odd -> Natural
oddToNatural (SuccOdd e) = 1 + evenToNatural e

evenToNatural :: Even -> Natural
evenToNatural  Zero        = 0
evenToNatural (SuccEven o) = 1 + oddToNatural o

result :: Natural
result = oddToNatural example
```

... which produces this `result`:

```console
$ ghci Example2.hs
*Main> result
3
```

... corresponds to this Dhall code:

```dhall
let Odd
    : Type
    = ∀(Even : Type) →
      ∀(Odd : Type) →
      ∀(Zero : Even) →
      ∀(SuccEven : Odd → Even) →
      ∀(SuccOdd : Even → Odd) →
        Odd

let example
    : Odd
    = λ(Even : Type) →
      λ(Odd : Type) →
      λ(Zero : Even) →
      λ(SuccEven : Odd → Even) →
      λ(SuccOdd : Even → Odd) →
        SuccOdd (SuccEven (SuccOdd Zero))

let oddToNatural
    : Odd → Natural
    = λ(o : Odd) →
        o Natural Natural 0 (λ(n : Natural) → 1 + n) (λ(n : Natural) → 1 + n)

let result = oddToNatural example

in  result
```

... which produces the same `result`:

```console
$ dhall <<< './example2.dhall'
Natural

3
```

The trick here is that the Dhall's `Odd` type combines both of the Haskell
`Even` and `Odd` types. Similarly, Dhall's `oddToNatural` function combines
both of the Haskell `evenToNatural` and `oddToNatural` functions. You can
define a separate `Even` and `evenToNatural` in Dhall, too, but they would not
reuse any of the logic from `Odd` or `oddToNatural`.

### Smart constructors

The `build` function shown earlier is, in principle, equivalent to all possible constructors that one may need when building values of a recursive type.
When working with recursive union types, however, using the `build` function requires writing lots of `merge` expressions.

It is more convenient to create several "smart constructors" that remove the need for `merge` expressions.

The following examples implement the same logic as the prior
examples, except defining convenient intermediate constructors along the way.

For example, we can define a `MakePerson` smart constructor and then use that
smart constructor to create the same `example` `Person`:

```dhall
-- example0.dhall

let List/map = https://prelude.dhall-lang.org/v16.0.0/List/map

let Person
    : Type
    = ∀(Person : Type) →
      ∀(MakePerson : { children : List Person, name : Text } → Person) →
        Person

let MakePerson
    : { children : List Person, name : Text } → Person
    = λ(x : { children : List Person, name : Text }) →
      λ(_Person : Type) →
      λ(MakePerson : { children : List _Person, name : Text } → _Person) →
        let adapt
            : Person → _Person
            = λ(y : Person) → y _Person MakePerson

        in  MakePerson
              (x with children = List/map Person _Person adapt x.children)

let example
    : Person
    = MakePerson
        { children =
          [ MakePerson { children = [] : List Person, name = "Mary" }
          , MakePerson { children = [] : List Person, name = "Jane" }
          ]
        , name = "John"
        }

let everybody
    : Person → List Text
    = let concat = http://prelude.dhall-lang.org/List/concat

      in  λ(x : Person) →
            x
              (List Text)
              ( λ(p : { children : List (List Text), name : Text }) →
                  [ p.name ] # concat Text p.children
              )

let result
    : List Text
    = everybody example

in  result
```

Carefully notice the difference in how we create an `example` `Person`, which
changed from this:

```dhall
let example
    : Person
    = λ(Person : Type) →
      λ(MakePerson : { children : List Person, name : Text } → Person) →
        MakePerson
          { children =
            [ MakePerson { children = [] : List Person, name = "Mary" }
            , MakePerson { children = [] : List Person, name = "Jane" }
            ]
          , name = "John"
          }
```

... to this:

```dhall
let example
    : Person
    = MakePerson
        { children =
          [ MakePerson { children = [] : List Person, name = "Mary" }
          , MakePerson { children = [] : List Person, name = "Jane" }
          ]
        , name = "John"
        }
```

They are both the same type and they both have the same normal form, but the
latter is more ergonomic to create from the end user's perspective due to
using the `MakePerson` smart constructor we defined.

We can also rework the `Nat` example in the same way:

```dhall
-- example1.dhall

let Nat
    : Type
    = ∀(Nat : Type) → ∀(Zero : Nat) → ∀(Succ : Nat → Nat) → Nat

let Zero
    : Nat
    = λ(Nat : Type) → λ(Zero : Nat) → λ(Succ : Nat → Nat) → Zero

let Succ
    : Nat → Nat
    = λ(x : Nat) →
      λ(Nat : Type) →
      λ(Zero : Nat) →
      λ(Succ : Nat → Nat) →
        Succ (x Nat Zero Succ)

let example
    : Nat
    = Succ (Succ (Succ Zero))

let toNatural
    : Nat → Natural
    = λ(x : Nat) → x Natural 0 (λ(n : Natural) → 1 + n)

let result
    : Natural
    = toNatural example

in  result
```

... and the `Even`/`Odd` example:

```dhall
let Even
    : Type
    = ∀(Even : Type) →
      ∀(Odd : Type) →
      ∀(Zero : Even) →
      ∀(SuccEven : Odd → Even) →
      ∀(SuccOdd : Even → Odd) →
        Even

let Odd
    : Type
    = ∀(Even : Type) →
      ∀(Odd : Type) →
      ∀(Zero : Even) →
      ∀(SuccEven : Odd → Even) →
      ∀(SuccOdd : Even → Odd) →
        Odd

let Zero
    : Even
    = λ(Even : Type) →
      λ(Odd : Type) →
      λ(Zero : Even) →
      λ(SuccEven : Odd → Even) →
      λ(SuccOdd : Even → Odd) →
        Zero

let SuccEven
    : Odd → Even
    = λ(x : Odd) →
      λ(Even : Type) →
      λ(Odd : Type) →
      λ(Zero : Even) →
      λ(SuccEven : Odd → Even) →
      λ(SuccOdd : Even → Odd) →
        SuccEven (x Even Odd Zero SuccEven SuccOdd)

let SuccOdd
    : Even → Odd
    = λ(x : Even) →
      λ(Even : Type) →
      λ(Odd : Type) →
      λ(Zero : Even) →
      λ(SuccEven : Odd → Even) →
      λ(SuccOdd : Even → Odd) →
        SuccOdd (x Even Odd Zero SuccEven SuccOdd)

let example
    : Odd
    = SuccOdd (SuccEven (SuccOdd Zero))

let oddToNatural
    : Odd → Natural
    = λ(o : Odd) →
        o Natural Natural 0 (λ(n : Natural) → 1 + n) (λ(n : Natural) → 1 + n)

let result = oddToNatural example

in  result
```

In each case, the general pattern for building the "smart constructors" is the
same: any time we reach a recursive occurrence of the type, we apply the
recursive occurrence to all the variables we brought into scope, in the
same order.

For example, when building the smart constructor for `MakePerson`, each
recursive occurrence is bound to a variable named `y`, which is applied to
the two bound variables we brought into scope (`Person` and `MakePerson`):

```dhall
let MakePerson
    : { children : List Person, name : Text } → Person
    = λ(x : { children : List Person, name : Text }) →
      λ(_Person : Type) →
      λ(MakePerson : { children : List _Person, name : Text } → _Person) →
        let adapt
            : Person → _Person
            = λ(y : Person) → y _Person MakePerson
                 -- See here: ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑

        in  MakePerson
              (x with children = List/map Person _Person adapt x.children)
```

... and you see the same pattern here in the `Nat` example:

```dhall
let Succ
    : Nat → Nat
    = λ(x : Nat) →
      λ(Nat : Type) →
      λ(Zero : Nat) →
      λ(Succ : Nat → Nat) →
        Succ (x Nat Zero Succ)
     -- Here: ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑
```

... and in the `Even`/`Odd` example:

```dhall
let SuccEven
    : Odd → Even
    = λ(x : Odd) →
      λ(Even : Type) →
      λ(Odd : Type) →
      λ(Zero : Even) →
      λ(SuccEven : Odd → Even) →
      λ(SuccOdd : Even → Odd) →
        SuccEven (x Even Odd Zero SuccEven SuccOdd)
         -- Here: ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑
```

### JSON

You can see a real example of this pattern in the Prelude's support for
[`JSON`](https://github.com/dhall-lang/dhall-lang/blob/master/Prelude/JSON/Type.dhall)

## Conclusion

The general algorithm for translating recursive code to non-recursive code is
known as the Boehm-Berarducci encoding and is shown in this paper:

* [Automatic synthesis of typed Λ-programs on term algebras](http://www.sciencedirect.com/science/article/pii/0304397585901355)

This guide doesn't explain the full algorithm due to the amount of detail
involved, but if you are interested you can read the above paper.

Also, if the above examples were not sufficient, feel free to open an issue to
request another example to add to this guide.
