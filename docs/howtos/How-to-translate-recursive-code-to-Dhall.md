# How to translate recursive code to Dhall

> Embed recursive data types and functions in a non-recursive language

The Dhall configuration language only provides built-in support for three
recursive data types: `List`s, `Optional`s, and unions.  However, the language
does not provide native support for user-defined recursive types, recursive
values, or recursive functions.

Despite that limitation, you can still transform recursive code into
non-recursive Dhall code.  This guide will explain how by example, walking
through examples of progressively increasing difficulty.

## Why doesn't Dhall natively support recursion?

Dhall is a total language, which means that when it evaluates an expression
written in Dhall, it needs to consider the entire universe of the expression
at the time of evaluation, i.e. the "total" universe. For a non-recursive
expression, this is quite simple. For example, consider a `Person` who has a
name:

```dhall
-- Person.dhall
{ name : Text }
```

This `Person` type succinctly expresses the total range of possibilities. A
`Person` has a `name`, which is of type `Text`. A `Person` doesn't have any
other fields, and a `name` can't be anything other than a `Text` (i.e. it
can't be a `Natural`).

Now, let's try to expand our example - what if a `Person` has a biological
mother, who is also a `Person`? A naive approach would be to try:

```dhall
-- Person.dhall
let Person = ./Person.dhall

in { name : Text , biological-mother : Person }
```

However, if you try to evaluate this (i.e. `dhall --file Person.dhall`), you
will encounter the following error:

```text
dhall:
↳ ./Person.dhall

Cyclic import: ./Person.dhall

1│              ./Person.dhall

/path/to/current/directory/Person.dhall:1:14
```

Why is that? Well, let's try to normalize this "on paper". The first level of
recursion would normalize to the following:

```dhall
{ name : Text
, biological-mother :
  let Person = ./Person.dhall

  in { name: Text, biological-mother : Person }
}
```

This would force us to define a not just a person's biological mother, but
*also* a person's biological grandmother. And Dhall's totality won't let it
stop there - on the second level of recursion, we would need to also define a
biological great-grandmother, and so on and so forth for each additional level
of recursion that Dhall enters. And while in reality, a person's genealogical
tree will go pretty far back, at some point we either don't know or no longer
care about the detail from that long ago.

We can sidestep this issue in Dhall - the *requirement* of defining the next
person - by using a recursive data type.

## Recursive data types

There are two recursive data types in `Dhall` - `Optional` and `List`. What
makes these types recursive is the fact that they both parameterize other
types. You can define, for example, a `{ names : List Text }` or an
`{ expose-port : Optional Natural }`, but trying to define `{ foo : Optional }`
or `{ bar : List }` will result in a type error.

Because they can parameterize other types, they can also parameterize
themselves. An `Optional (Optional Text)` is a legal type in Dhall, just as
`List (List Integer)` is. These are legal ways of expressing recursion, because
they both set strict limits on the depth of the recursion. An
`Optional (Optional Text)` has two levels of recursion, and no more, because
`Text` is not a recursive type. A `List (List (List Integer))` has three levels
of recursion, and no more, because `Integer` is not a recursive type. Both an
`Optional` and a `List` allow the user to express the concept of "the end of
the recursion has been reached." Therefore, this allows the user a way of
expressing recursion in Dhall, while still adhering to Dhall's strict
totality requirements.

Whether it is correct to expression recursion with a `List` or with an
`Optional` depends on what precisely the user is trying to express - if the
user is trying to express that there may or may not be another level of
recursion, then this is simple enough to express with an `Optional` - the
recursion either does (i.e. `Some`) or does not (i.e. `None`) exist. If the
recursion has more than one branch, then these additional possibilities can
be expressed with a `List` of arbitrary length. If the precise number of
branches is known, and it is more than one, then a union can be constructed.

## Recursion using `Optional`

Let's consider our earlier example of a person with a biological mother, but
let's now express the `biological-mother` as an `Optional Person`. A naive
approach would be to try:

```dhall
-- Person.dhall
let Person = ./Person.dhall

in { name : Text , biological-mother : Optional Person }
```

However, this still results in a cyclic import error! This is because, while
`Optional` may be a permitted recursive type, its *contents* (i.e. `Person`),
in this example, are not. Dhall requires the user to more precisely define
the permitted level of recursion.

Let's start with a simple example: what does a `Person` with no biological
mother look like?

```dhall
let Person =
  { name : Text , biological-mother : Optional <> }

in { name = "Sally" , biological-mother = None <> } : Person
```

What is `<>`? It is a union with no alternatives, so it cannot be constructed
or `merge`'d. It is, in essence, the `Void` type in Dhall.

What does a `Person` with no biological grandmother look like?

```dhall
let Person =
  λ(BiologicalMother: Type) →
  { name : Text , biological-mother : Optional BiologicalMother }

let Mother-less = Person <> -- a person whose biological mother does not exist

let Grandmother-less = Person Mother-less {- a person whose biological mother
                                             is a person whose biological
                                             mother does not exist
                                          -}

in { name = "Samantha"
   , biological-mother = Some
     { name = "Sally"
     , biological-mother = None <>
     } : Mother-less
   } : Grandmother-less
```

In this example, you can see how we turn Person into a function that produces
a type, rather than a raw type itself. While this starts to show how we could
build somebody's matriarchal lineage, it's utility is still limited. A
`Person <>` is not the same type as a `Person (Person <>)`, and so, for
example, Dhall won't allow the user to put both a `Mother-less` person and a
`Grandmother-less` person in the same `List`. In order to do that, we need for
Person not to be a function that produces a type, but *the type of a function
that produces a type*. What does this look like?

```dhall
-- example0.dhall

let Person
    : Type
    =   ∀(Person : Type)
      → ∀(MakePerson : { biological-mother : Optional Person, name : Text } → Person)
      → Person

let example
    : Person
    =   λ(Person : Type)
      → λ(MakePerson : { biological-mother : Optional Person, name : Text } → Person)
      → MakePerson
        { biological-mother = Some
            (MakePerson { biological-mother = None Person, name = "Mary" })
        , name =
            "John"
        }

let everybody
    : Person → List Text
    = let concat = http://prelude.dhall-lang.org/Optional/concat.dhall

      let toList = http://prelude.dhall-lang.org/Optional/toList.dhall

      in    λ(x : Person)
          → x
            (List Text)
            (   λ(p : { biological-mother : Optional (Optional Text), name : Text })
              → [ p.name ] # toList Text (concat Text p.biological-mother )
            )

let result : List Text = everybody example

in  result
```

... which evaluates to the same result:

```console
$ dhall <<< './example0.dhall'
List Text

[ "John", "Mary" ]
```

## Recursion using `List`

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

The equivalent Dhall code would be:

```dhall
-- example0.dhall

let Person
    : Type
    =   ∀(Person : Type)
      → ∀(MakePerson : { children : List Person, name : Text } → Person)
      → Person

let example
    : Person
    =   λ(Person : Type)
      → λ(MakePerson : { children : List Person, name : Text } → Person)
      → MakePerson
        { children =
            [ MakePerson { children = [] : List Person, name = "Mary" }
            , MakePerson { children = [] : List Person, name = "Jane" }
            ]
        , name =
            "John"
        }

let everybody
    : Person → List Text
    = let concat = http://prelude.dhall-lang.org/List/concat

      in    λ(x : Person)
          → x
            (List Text)
            (   λ(p : { children : List (List Text), name : Text })
              → [ p.name ] # concat Text p.children
            )

let result : List Text = everybody example

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
    =   ∀(_Person : Type)
      → ∀(MakePerson : { children : List _Person, name : Text } → _Person)
      → _Person

let example
    : Person
    =   λ(_Person : Type)
      → λ(MakePerson : { children : List _Person, name : Text } → _Person)
      → MakePerson
        { children =
            [ MakePerson { children = [] : List _Person, name = "Mary" }
            , MakePerson { children = [] : List _Person, name = "Jane" }
            ]
        , name =
            "John"
        }

let everybody
    : Person → List Text
    = let concat = http://prelude.dhall-lang.org/List/concat

      in    λ(x : Person)
          → x
            (List Text)
            (   λ(p : { children : List (List Text), name : Text })
              → [ p.name ] # concat Text p.children
            )

let result : List Text = everybody example

in  result
```

The way that this works is that a recursive function like `everybody` is
performing substitution.  In this specific case, `everybody` is:

*   replacing each occurrence of the type `Person` with the type `List Text`
*   replacing each occurrence of the `MakePerson` function with the following
    anonymous function:

    ```dhall
       λ(p : { children : List (List Text), name : Text })
    → [ p.name ] # concat Text p.children
    ```

... which means that our previous example could also have been written like
this:

```dhall
let concat = http://prelude.dhall-lang.org/List/concat

let Person : Type = List Text

let MakePerson
    : { children : List Person, name : Text } → Person
    =   λ(p : { children : List Person, name : Text })
      → [ p.name ] # concat Text p.children

let result =
      MakePerson
      { children =
          [ MakePerson { children = [] : List Person, name = "Mary" }
          , MakePerson { children = [] : List Person, name = "Jane" }
          ]
      , name =
          "John"
      }

in  result
```

### Recursive sum type

Sum types work in the same way, except that instead of one constructor (i.e.
`MakePerson`) we now have two constructors: `Succ` and `Zero`.  For example, this
Haskell code:

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

let Nat : Type = ∀(Nat : Type) → ∀(Zero : Nat) → ∀(Succ : Nat → Nat) → Nat

let example
    : Nat
    =   λ(Nat : Type)
      → λ(Zero : Nat)
      → λ(Succ : Nat → Nat)
      → Succ (Succ (Succ Zero))

let toNatural
    : Nat → Natural
    = λ(x : Nat) → x Natural 0 (λ(n : Natural) → 1 + n)

let result : Natural = toNatural example

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
*   replacing every occurrence of `Succ` with an anonymous funciton

... which means that we could have equivalently written:

```dhall
let Nat = Natural

let Zero : Nat = 0

let Succ : Nat → Nat = λ(n : Nat) → 1 + n

let result : Nat = Succ (Succ (Succ Zero))

in  result
```

### Mutually recursive types

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
    =   ∀(Even : Type)
      → ∀(Odd : Type)
      → ∀(Zero : Even)
      → ∀(SuccEven : Odd → Even)
      → ∀(SuccOdd : Even → Odd)
      → Odd

let example
    : Odd
    =   λ(Even : Type)
      → λ(Odd : Type)
      → λ(Zero : Even)
      → λ(SuccEven : Odd → Even)
      → λ(SuccOdd : Even → Odd)
      → SuccOdd (SuccEven (SuccOdd Zero))

let oddToNatural
    : Odd → Natural
    =   λ(o : Odd)
      → o Natural Natural 0 (λ(n : Natural) → 1 + n) (λ(n : Natural) → 1 + n)

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
let Odd : Type = Natural

let Even : Type = Natural

let Zero : Even = 0

let SuccEven : Odd → Even = λ(n : Odd) → 1 + n

let SuccOdd : Even → Odd = λ(n : Even) → 1 + n

let result = SuccOdd (SuccEven (SuccOdd Zero))

in  result
```

### JSON

You can see a real example of this pattern in the Prelude's support for
[`JSON`](https://github.com/dhall-lang/dhall-lang/blob/master/Prelude/JSON/Type)

## Conclusion

The general algorithm for translating recursive code to non-recursive code is
known as Boehm Berarducci encoding and based off of this paper:

* [Automatic synthesis of typed Λ-programs on term algebras](http://www.sciencedirect.com/science/article/pii/0304397585901355)

This guide doesn't explain the full algorithm due to the amount of detail
involved, but if you are interested you can read the above paper.

Also, if the above examples were not sufficient, feel free to open an issue to
request another example to add to this guide.
