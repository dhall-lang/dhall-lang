# How to translate recursive code to Dhall

> Embed recursive data types and functions in a non-recursive language

The Dhall configuration language provides built-in support only for one
recursive data type: `List`. The language does not provide built-in
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

Rejecting direct recursion is one of the core design decisions in Dhall.
It ensures that any well-typed Dhall program will always evaluate to a final value (called the "normal form") within finite time.
Because of that limitation, it is simply not possible to write a Dhall program that type-checks but then enters an infinite loop while evaluating.
This is a valuable property for a configuration language.

However, one can still work with a wide range of recursive types and functions in Dhall. 
This guide explains how to do that, walking through examples of progressively increasing difficulty.

## How to implement recursive types: a general recipe

The main idea is to replace a recursive type definition by a more detailed description showing how a value of the recursive type may be constructed from simpler parts or from primitive values.
That description will be _itself_ non-recursive, and so it will be accepted by Dhall.
Then one needs to use a trick (known as the "Church encoding") that involves universally quantified types. That trick replaces a recursive type definition with an equivalent but more complicated type whose definition is _not_ recursive.

We will now explain the Church encoding technique step by step.

### Step 1: from a recursive type definition to a recursion scheme

First, we rewrite a Haskell recursive type definition in the form `data T = F T` where `F` will be a new type constructor called the "recursion scheme". We will need to define `F` appropriately.

For example, the integer list type has this Haskell definition:

```haskell
data ListInt = Nil | Cons Int ListInt
```

We need to rewrite this definition as `data ListInt = F ListInt`, where `F` needs to be a suitable new type constructor. It is clear what `F` should be:

```haskell
data F r = Nil | Cons Int r
```

We find `F r` by replacing all recursive instances of `ListInt` via the type parameter `r`. (Here the letter `r` reminds us of "recursion".)

If we substitute `ListInt` for `r` in `F r`, we obtain `Nil | Cons Int ListInt`, which is exactly the right-hand side of the original recursive definition. In this way, `data ListInt = F ListInt` is the same as `data ListInt = Nil | Cons Int ListInt`. It shows that our choice of `F` is correct. 

The recursion scheme `F` describes all the possible ways of constructing a value of a recursive type. In this example, there are only two ways of constructing a value of type `ListInt`: first, `Nil` is a value of type `ListInt`. Second, if we _somehow_ already have a value `r` of type `ListInt`, we can use `Cons` to construct new values of type `ListInt`, for instance, `Cons -123 r`. 

Because `F` is itself _not_ recursive, its definition will be accepted by Dhall:

```dhall
let F = λ(r : Type) → < Nil | Cons : { head : Integer, tail : r } > in F
```

As another example, take a binary tree (with just integer leaf values):

```haskell
data TreeInt = Leaf Int | Branch TreeInt TreeInt
```

The corresponding recursion scheme `F` is defined by:

```haskell
data F r = Leaf Int | Branch r r
```

This is a _non-recursive_ type constructor defined in Dhall by:

```dhall
let F = λ(r : Type) → < Leaf: Integer | Branch : { left : r, right : r } > in F
```

If a recursive data type has itself some type parameters, those type parameters will have to be added to `F` in addition to the type parameter `r`.

A list with a type parameter in Haskell:

```haskell
data List a = Nil | Cons a (List a)
```

The corresponding recursion scheme is:

```haskell
data F a r = Nil | Cons a r
```

The Dhall code is:

```dhall
let F = λ(a : Type) → λ(r : Type) →
   < Nil | Cons : { head : a, tail : r } > in F
```

A binary tree with a type parameter in Haskell:

```haskell
data Tree a = Leaf a | Branch (Tree a) (Tree a)
```

The corresponding recursion scheme is:

```haskell
data F a r = Leaf a | Branch r r
```

The Dhall code is:

```dhall
let F = λ(a : Type) → λ(r : Type) →
   < Leaf : a | Branch : { left : r, right : r } > in F
```

As another example, consider a binary tree with more type parameters:

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
   < LeafA : a | LeafB : b | Branch : { left : r, right : r } > in F
```

We see that we can always produce a non-recursive type constructor `F` for any recursive type definition.
The code of `F` will be accepted by Dhall.

### Step 2: the Church encoding

Now we use a trick known as the "Church encoding". This trick converts any recursion scheme `F` into a type that contains a universal quantifier. In Dhall, that type is written as:

```dhall
∀(r : Type) → (F r → r) → r
```

Carefully note that the Church encoding uses the universal quantifier (`∀`) and not the `λ` symbol.

The expression `λ(r : Type) → P r` (with some type constructor `P`) would be a function that needs to be applied to a particular type in order to produce a result type. In other words, it would be a _type constructor_ that itself is not of type `Type` but of type `Type → Type`. This is not what we need for the Church encoding.  

Compare that with what happens if we use the universal quantifier.
A value of type `∀(r : Type) → P r` is a function with a type parameter.
That is, some code that works for all types `r` in the same way and produces a value of type `P r` no matter what type `r` might be.
A value of type `∀(r : Type) → P r` is a function expression of the form `λ(r : Type) → ...`. We will see below how to work with those expressions.

If the recursion scheme `F` has additional type parameters `a`, `b`, etc., we need to write the Church encoding with all those parameters next to `F`:

```dhall
λ(a : Type) → λ(b : Type) → ∀(r : Type) → (F a b r → r) → r
```

Those type definitions are not recursive because they just use a previously defined (and also non-recursive) type constructor `F` in a type expression with quantifiers. However, it turns out that those definitions are equivalent to defining recursive data types with the additional guarantee that the resulting data structures are always finite.

A mathematical proof of this property is given in the paper ["Recursive types for free"](https://homepages.inf.ed.ac.uk/wadler/papers/free-rectypes/free-rectypes.txt) by P. Wadler. In this tutorial we will focus on the practical use of those type constructions.

Let us now write the Dhall code for the examples shown in the previous section.

The type `ListInt` (a list with integer values):

```dhall
let F = λ(r : Type) → < Nil | Cons : { head : Integer, tail : r } >
let ListInt = ∀(r : Type) → (F r → r) → r
    in ListInt
```

A binary tree with integer leaf values:

```dhall
let F = λ(r : Type) → < Leaf: Integer | Branch : { left : r, right : r } >
let TreeInt = ∀(r : Type) → (F r → r) → r
    in TreeInt
```

A list with values of type `a`:

```dhall
let F = λ(a : Type) → λ(r : Type) →
   < Nil | Cons : { head : a, tail : r } >
let ListA = λ(a : Type) → ∀(r : Type) → (F r → r) → r
    in ListA
```

A binary tree with leaf values of type `a`:

```dhall
let F = λ(a : Type) → λ(r : Type) →
   < Leaf : a | Branch : { left : r, right : r } >
let TreeA = λ(a : Type) → ∀(r : Type) → (F a r → r) → r
    in TreeA
```

A binary tree with two type parameters:

```dhall
let F = λ(a : Type) → λ(b : Type) → λ(r : Type) →
   < LeafA : a | LeafB : b | Branch : { left : r, right : r } >
let TreeAB = λ(a : Type) → λ(b : Type) → ∀(r : Type) → (F a b r → r) → r
    in TreeAB
```

### Step 3: Working with recursive types

We have shown a recipe for converting any recursive type definition into a recursion scheme and finally into non-recursive (but complicated) Church-encoded type.

It takes some work to figure out how to write values of those types and to develop techniques for programming with Church encodings more conveniently.

#### Worked example: `ListInt`

To learn those techniques, we study the type `ListInt` whose Dhall code is:

```dhall
let F = λ(r : Type) → < Nil | Cons : { head : Integer, tail : r } >
let ListInt = ∀(r : Type) → (F r → r) → r
    in ListInt
```

How can we implement a value `x` of this type? We need to write a function that takes an arbitrary type `r` and an arbitrary function `frr` of type `F r → r` and then returns a value of type `r`.

```dhall
let x : ListInt = λ(r : Type) → λ(frr : F r → r) → ... -- Some code here.
```

The code of this function must work for any type `r` whatsoever. How can we produce a value of type `r` if we do not even know what type it is?

The only possibility of doing that is by applying the function `frr` to some argument of type `F r`. So, now we need to see how we could produce a value of type `F r`.

We notice that `F r` is a union type with one of the possibilities being just `Nil`.
So, we can give that value `Nil` as an argument to `frr`. This will give us a value of type `r` that we can return. So, one possibility of implementing a value of type `ListInt` is:

```dhall
let x0 : ListInt = λ(r : Type) → λ(frr : F r → r) →
    let fr0 = < Nil | Cons : { head : Integer, tail : r } >.Nil
    let r0 = frr fr0
    in r0
  in x0
```

The other possibility in the union type of `F r` is `Cons` with a record that contains an `Integer` and a previously known value of type `r`. But where will we get that previous value? So far, we could only get that value out of using `Nil` when we defined `x0` just above. Let us build upon that code. Choosing the integer value arbitrarily as `-123`, we write:

```dhall
let x1 : ListInt = λ(r : Type) → λ(frr : F r → r) →
    let fr0 = < Nil | Cons : { head : Integer, tail : r } >.Nil
    let r0 = frr fr0
    let fr1 = < Nil | Cons : { head : Integer, tail : r } >.Cons {head = -123, tail = r0 }
    let r1 = frr fr1
    in r1 
  in x1
```

We have written the code in a verbose way in order to show how we build up values of type `r` from scratch. We can continue in the same way to encode more complicated values of type `ListInt`:

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

We find that we can implement values of type `ListInt` if we just choose zero or more integers and write code as shown above with zero or more similar-looking steps. Each step contains an arbitrary integer value and computes a new value of type `r` out of a previous value. In this way, the type `ListInt` represents (possibly empty) lists of integer values. There is no other way of constructing a value of type `ListInt`.

Could we hide the verbose boilerplate and make working with `ListInt` easier? Let us introduce "constructors" `nil` and `cons` so that the code for `x2` will be just `cons +456 (cons -123 nil)`.

The `nil` constructor is the same as `x0` shown above. The `cons` constructor encapsulates one step of the boilerplate code:

```dhall
let nil : ListInt = λ(r : Type) → λ(frr : F r → r) →
    frr < Nil | Cons : { head : Integer, tail : r } >.Nil
let cons: Integer → ListInt → ListInt = λ(head : Integer) → λ(tail : ListInt) →
    λ(r : Type) → λ(frr : F r → r) →
        let fr = < Nil | Cons : { head : Integer, tail : r } >.Cons {head = head, tail = tail r f }
        in frr fr
    in cons +456 (cons -123 nil)
```

In this code, it is important that we are allowed to write `tail r f` while computing `fr`. The value `tail : ListInt` is a function (since `ListInt` is a function type). We are using that function with the type `r` that we have received in the body of `cons`. We are allowed to do this because `tail`, being a value of type `ListInt`, is a function that can work with arbitrary types `r`.

Let us also implement a `foldLeft` function for `ListInt`. That function serves as a general "aggregation" algorithm, converting a list of integers into an aggregated value of some type. The type signature of `foldLeft` is:

```dhall
foldLeft : ∀(r : Type) → ∀(init : r) → ∀(update : r → Integer → r) → ListInt → r
```

The arguments of `foldLeft` are an arbitrary result type `r`, an initial value of type `r`, and an "updater" function of type `r → Integer → r`. The arguments of the updater function are the currently aggregated value of type `r` and a next integer from the list. The result is the next aggregated value (of type `r`).

It is perhaps surprising that the code of `foldLeft` is _non-recursive_:

```dhall
let foldLeft : ∀(r : Type) → ∀(init : r) → ∀(update : r → Integer → r) → ListInt → r =
  λ(r : Type) → λ(init : r) → λ(update : r → Integer → r) → λ(list : ListInt) →
    let consR : { head : Integer, tail : r } → r = λ(fr : { head : Integer, tail : r }) → update fr.tail fr.head
    let frr : F r → r = λ(fr : F r) → merge { Nil = init, Cons = consR } fr
    in list r frr
  in foldLeft
```

This code merely calls the given value `list : ListInt` on a certain function `fr : F r → r`. That function is constructed out of the given arguments `init` and `update`.

Because `foldLeft` is non-recursive, Dhall accepts that function.

In this way, Dhall is able to construct integer lists and also to run loops over them, computing an aggregated value using `foldLeft`.

### Where did the recursion go?

The technique of Church encoding may be unfamiliar and perplexing. If we are actually working with recursive types and recursive functions, why do we no longer see any recursion in the code? In `foldLeft`, why is there no code that iterates over a list of integers in a loop?

An answer is found by comparing the codes for the values `x0`, `x1`, and `x2` shown in the previous section when working with `ListInt`. The values `x0`, `x1`, and `x2` are functions whose second argument is a function `frr : F r → r`. The code for `x0` calls that function only once; the code for `x1` calls that function twice; and the code for `x2` calls that function three times.

This explains why `foldLeft` is non-recursive. The code of `foldLeft` merely prepares a function `frr` and passes it to the given value of type `ListInt`. If we run `foldLeft` on `x2`, it is the code of `x2` that will call the function `frr` three times. There is also no loop in `x2`; it is just hard-coded in the function `x2` to apply `frr` three times.

A list of 1000 integers will be represented by a function (call it `x1000 : ListInt`) that takes an argument `frr : F r → r` and calls the `frr` function a thousand times.
This is because the only way of creating a list of 1000 integers is to create an expression such as `cons 1 (cons 2 (cons 3 (... (cons 1000 nil)))...)`.

In this way, the Church encoding hides the loops and allows us to represent iterative computations without recursion.

At the same time, the Church encoding guarantees that all recursive structures will be finite and all operations on those structures will terminate. It is for that reason that Dhall is able to accept Church encodings.

### Church encoding and `fold` types are equivalent

How to generalize `foldLeft` from `ListInt` to arbitrary recursive types? That is done via an equivalence relationship between a Church-encoded type, such as `ListInt`, and the type of the corresponding `foldLeft` function.

Looking at the type of `foldLeft` for `ListInt`, we note that the type of functions `F r → r` is equivalent to a pair of functions: one function with zero arguments (returning just `r`) and one with the type signature of `update : r → Integer → r`. For this reason, the data required to create a function of type `F r → r` is the same as the data contained in the arguments of `foldLeft` (that is, `init` and `update`).

Because of that, the type of `foldLeft` can be rewritten equivalently as:

```dhall
∀(r : Type) → ∀(frr : F r → r) → ListInt → r
```

We can then swap the order of curried arguments to obtain another equivalent type expression:

```dhall
ListInt → ∀(r : Type) → ∀(frr : F r → r) → r
```

This is exactly the same as `ListInt → ListInt`, and the function of that type is an identity function. The code of `foldLeft` is just an identity function in disguise! No wonder it is non-recursive.

Keeping this in mind, we may say that the Church encoding method consists of encoding recursive types via the types of their `fold` functions.

The same argument will hold for any recursive types, including recursive types with extra type parameters. Given a recursion scheme `F` and the corresponding Church-encoded type `C = ∀(r : Type) → (F r → r)  → r`, a `fold` function can be implemented in general as an identity function of type `C → C` adapted to the type signature of `fold`:

```dhall
∀(r : Type) → ∀(frr : F r → r) → C → r
```

### The `build` function

In the previous section we showed the constructors `nil` and `cons` for the `ListInt` type. What is the corresponding technique for an arbitrary Church-encoded type with a given recursion scheme `F`?

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

Note that the argument of that function has type `< n : { } | c : { x : Integer, l : ListInt } >`, which is equivalent to `F ListInt` because `F ListInt` is just `< Nil | Cons : { head : Integer, tail : ListInt } >`.

So, we find that the two constructors `nil` and `cons` may be replaced by a single value of type `F ListInt → ListInt`.

This suggests that the set of constructors for an arbitrary recursion scheme `F` and the corresponding Church-encoded type `C = ∀(r : Type) → (F r → r) → r` is just a value of type `F C → C`. 

It turns out that there is a unique value of type `F C → C` that satisfies certain required properties (which are beyond the scope of this tutorial). That value is denoted as the `build` function for the Church-encoded type `C`. The `build` function encapsulates all the basic constructors that are used for building values the type `C`.

A general implementation of `build` depends on having the `fmapF` function for the type constructor `F`. So, this technique only works when `F` is a covariant type constructor. But this is always true in all practical cases.

The Dhall code for `build` is:

```dhall
let F = ∀(r : Type) → ... -- Define it here.
let fmapF : ∀(a : Type) → ∀(b : Type) → (a → b) → F a → F b = ... -- Define it here.
let C = ∀(r : Type) → (F r → r) → r
let build : F C → C = λ(fc : F C) → λ(r : Type) → λ(frr : F r → r) →
    let c2r : C → r = λ(c : C) → c r frr
    let fr : F r = fmapF C r c2r fc
    in frr fr
  in build
```

In Dhall, the only built-in recursive data type is `List`. The corresponding `fold` and `build` functions are the built-in symbols `List/fold` and `List/build`. 

### Pattern matching on Church-encoded values

When working with recursive types in ordinary functional languages, one often uses pattern matching. For example, here is a simple function that detects whether a given tree is a single leaf:

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
    in TreeInt
```

and

```dhall
let F = λ(r : Type) → < Nil | Cons : { head : Integer, tail : r } >
let ListInt = ∀(r : Type) → (F r → r) → r
in ListInt
```

Values of type `TreeInt` and `ListInt` are functions, so we cannot perform pattern matching on such values. How can we implement functions like `isSingleLeaf` and `headMaybe` in Dhall?

The general method for translating pattern matching into Church-encoded types `C` consists of two steps. The first step is to define a function we will call `unroll`, of type `C → F C`. This function is the inverse of the function `build : F C → C` from the previous subsection.

The Dhall code for `unroll` is:

```dhall
let F = λ(r : Type) → ... -- Define it here.
let fmapF : ∀(a : Type) → ∀(b : Type) → (a → b) → F a → F b = ... -- Define it here.
let C = ∀(r : Type) → (F r → r) → r
let unroll : C → F C =
  let fmapBuild : F (F C) → F C = fmapF (F C) C build -- Use the definition of `build` above.
    in λ(c : C) → c (F C) fmapBuild
```

A rigorous proof that `unroll` and `build` are inverse functions is shown in the paper ["Recursive types for free"](https://homepages.inf.ed.ac.uk/wadler/papers/free-rectypes/free-rectypes.txt) and is beyond the scope of this tutorial. Let us just remark that because we have these functions, the types `C` and `F C` are equivalent (isomorphic). Any data of type `C` can be mapped into data of type `F C` and back, without loss of information. In that sense, the type `C` satisfies the "type equation" `C = F C`. This is one way of defining rigorously the meaning of recursive types that one writes in Haskell as `data T = F T`.

The second step is to apply `unroll` to the value on which we need to use pattern matching. The result will be a value of type `F C`, which will be typically a union type. Then we can use ordinary pattern matching on values of that type.

With this technique, `isSingleLeaf` and `headMaybe` are translated to Dhall straightforwardly.

For `C = TreeInt`, the type `F C` is the union type `< Leaf: Integer | Branch : { left : TreeInt, right : TreeInt } >`. The function `isSingleLeaf` is implemented via Pattern-matching on that type:

```dhall
-- Assume definitions of TreeInt and unroll as shown above.
let isSingleLeaf : TreeInt → Bool = λ(c : TreeInt) →
    merge {
      Leaf = λ(_ : Integer) → true,
      Branch = λ(_ : { left : TreeInt, right : TreeInt }) → false
    } (unroll c)
  in isSingleLeaf
```

For `C = ListInt`, the type `F C` is the union type `< Nil | Cons : { head : Integer, tail : ListInt } >`. The function `headOptional`, similar to Haskell's `headMaybe`, is written in Dhall like this:

```dhall
-- Assume definitions of ListInt and unroll as shown above.
let headOptional : ListInt → Optional Integer = λ(c : ListInt) →
    merge {
      Cons = λ(list : { head : Integer, tail : ListInt }) → Some (list.head),
      Nil = None Integer
    } (unroll c)
  in headOptional
```

### Performance

The performance of Church-encoded values is often slow. The main source of slowness is that a value `c` of a Church-encoded type `C` is a function that may call its parameter `frr : F r → r` many times. For example, a `ListInt` value representing a list of 1000 integers is a function that calls its parameter `frr` 1000 times. So, evaluating any code that applies `c` to some `frr` will have to perform 1000 evaluations of some function. The evaluation time is linear in the data size.

In particular, this applies to the code of `unroll`. Since we use `unroll` for pattern matching, any single pattern matching will be linear in the data size. For instance, evaluating `headOptional` takes time that is linear in the length of the list, even though we may have expected that `headOptional` only needs to examine the first element of the list. If `headOptional` is called repeatedly, evaluation may become quite slow.

# Examples

We will now illustrate the general recipe on more examples, showing how to translate recursive Haskell code into non-recursive Dhall definitions.

## Recursive record

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

The equivalent Dhall code would be:

```dhall
-- example0.dhall

let Person
    : Type
    = ∀(Person : Type) →
      ∀(MakePerson : { children : List Person, name : Text } → Person) →
        Person

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

... which evaluates to the same result:

```console
$ dhall <<< './example0.dhall'
List Text

[ "John", "Mary", "Jane" ]
```

Carefully note that there is more than one bound variable named `Person` in the
above example.  We can disambiguate them by prefixing some of them with an
underscore (i.e. `_Person`):

```dhall
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

The way that this works is that a recursive function like `everybody` is
performing substitution.  In this specific case, `everybody` is:

*   replacing each occurrence of the type `Person` with the type `List Text`
*   replacing each occurrence of the `MakePerson` function with the following
    anonymous function:

    ```dhall
    λ(p : { children : List (List Text), name : Text }) →
      [ p.name ] # concat Text p.children
    ```

... which means that our previous example could also have been written like
this:

```dhall
let concat = http://prelude.dhall-lang.org/List/concat

let Person
    : Type
    = List Text

let MakePerson
    : { children : List Person, name : Text } → Person
    = λ(p : { children : List Person, name : Text }) →
        [ p.name ] # concat Text p.children

let result =
      MakePerson
        { children =
          [ MakePerson { children = [] : List Person, name = "Mary" }
          , MakePerson { children = [] : List Person, name = "Jane" }
          ]
        , name = "John"
        }

in  result
```

## Recursive sum type

Sum types work in the same way, except that instead of one constructor (i.e.
`MakePerson`) we now have two constructors: `Succ` and `Zero`.  For example,
this Haskell code:

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

... corresponds to this Dhall code:

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

Like before, our recursive `toNatural` function is performing substitution by:

*   replacing every occurrence of `Nat` with `Natural`
*   replacing every occurrence of `Zero` with `0`
*   replacing every occurrence of `Succ` with an anonymous function

... which means that we could have equivalently written:

```dhall
let Nat = Natural

let Zero
    : Nat
    = 0

let Succ
    : Nat → Nat
    = λ(n : Nat) → 1 + n

let result
    : Nat
    = Succ (Succ (Succ Zero))

in  result
```

## Mutually recursive types

The above pattern generalizes to mutually recursive types, too.  For example,
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
`Even` and `Odd` types.  Similarly, Dhall's `oddToNatural` function combines
both of the Haskell `evenToNatural` and `oddToNatural` functions.  You can
define a separate `Even` and `evenToNatural` in Dhall, too, but they would not
reuse any of the logic from `Odd` or `oddToNatural`.

Like before, our recursive `oddToNatural` function is performing substitution
by:

*   replacing every occurrence of `Even` with `Natural`
*   replacing every occurrence of `Odd` with `Natural`
*   replacing every occurrence of `Zero` with `0`
*   replacing every occurrence of `SuccEven` with an anonymous function
*   replacing every occurrence of `SuccOdd` with an anonymous function

... which means that we could have equivalently written:

```dhall
let Odd
    : Type
    = Natural

let Even
    : Type
    = Natural

let Zero
    : Even
    = 0

let SuccEven
    : Odd → Even
    = λ(n : Odd) → 1 + n

let SuccOdd
    : Even → Odd
    = λ(n : Even) → 1 + n

let result = SuccOdd (SuccEven (SuccOdd Zero))

in  result
```

## Smart constructors

You can create "smart constructors" for end users to use for assembling a
recursive type.  The following examples implement the same logic as the prior
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
recursive occurrence to all of the variables we brought into scope, in the
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

## JSON

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
