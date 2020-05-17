# How to translate recursive code to Dhall

> Embed recursive data types and functions in a non-recursive language

The Dhall configuration language only provides built-in support for one
recursive data type: `List`s.  However, the language does not provide native
support for user-defined recursive types, recursive values, or recursive
functions.

Despite that limitation, you can still transform recursive code into
non-recursive Dhall code.  This guide will explain how by example, walking
through examples of progressively increasing difficulty.

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
*   replacing every occurrence of `Succ` with an anonymous funciton

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

```
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
[`JSON`](https://github.com/dhall-lang/dhall-lang/blob/master/Prelude/JSON/Type)

## Conclusion

The general algorithm for translating recursive code to non-recursive code is
known as Boehm Berarducci encoding and based off of this paper:

* [Automatic synthesis of typed Λ-programs on term algebras](http://www.sciencedirect.com/science/article/pii/0304397585901355)

This guide doesn't explain the full algorithm due to the amount of detail
involved, but if you are interested you can read the above paper.

Also, if the above examples were not sufficient, feel free to open an issue to
request another example to add to this guide.
