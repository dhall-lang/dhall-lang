# Built-in types, functions, and operators

> A list of all available built-in functionality

This section briefly summarizes all of the types, functions, operators, and
keywords built into the Dhall language.

```eval_rst
.. contents:: Table of Contents
   :depth: 3
   :backlinks: none
```


## Bool

A `Bool` value can be either `True` or `False`.

#### Type

```
────────────────
Γ ⊢ Bool : Type
```

… and the `True` and `False` literals both have type `Bool`:

```
───────────────
Γ ⊢ True : Bool

────────────────
Γ ⊢ False : Bool
```

### Keyword: `if`/`then`/`else`

The most general way to consume a `Bool` value is with an `if` expression.

```dhall
⊢ if True then 3 else 5

3
```

#### Type

The type of an `if` expression is the same as the type of the `then` and `else`
branches, which must both match:

```
               Γ ⊢ t : Type
               ─────────────────────
Γ ⊢ b : Bool   Γ ⊢ l : t   Γ ⊢ r : t
────────────────────────────────────
Γ ⊢ if b then l else r : t
```

#### Rules

```dhall
if b then True else False ≡ b

if True  then l else r ≡ l

if False then l else r ≡ r
```

### Operator: `||` <a op=or />

The `||` operator corresponds to the boolean logical "or".

```dhall
⊢ True || False

True
```

#### Type

Both arguments to the `||` operator must have type `Bool` and the result will
have type `Bool`:

```
Γ ⊢ x : Bool   Γ ⊢ y : Bool
───────────────────────────
Γ ⊢ x || y : Bool
```

#### Rules

```dhall
x || False ≡ x

False || x ≡ x

(x || y) || z = x || (y || z)

x || True ≡ True

True || x ≡ True

x || (y && z) = (x || y) && (x || z)

(x && y) || z = (x || z) && (y || z)
```

### Operator: `&&` <a op=and />

The `&&` operator corresponds to the boolean logical "and":

```dhall
⊢ True && False

False
```

#### Type

Both arguments to the `&&` operator must have type `Bool` and the result will
have type `Bool`:

```
Γ ⊢ x : Bool   Γ ⊢ y : Bool
───────────────────────────
Γ ⊢ x && y : Bool
```

#### Rules

```dhall
x && True ≡ x

True && x ≡ x

(x && y) && z = x && (y && z)

x && False ≡ False

False && x ≡ False

x && (y || z) = (x && y) || (x && z)

(x || y) && z = (x && z) || (y && z)
```

### Operator: `==` <a op=equal />

The `==` operator corresponds to boolean logical equality.  Carefully note that
this operator only works on `Bool` values.

```dhall
⊢ True == False

False
```

#### Type

Both arguments to the `==` operator must have type `Bool` and the result will
have type `Bool`:

```
Γ ⊢ x : Bool   Γ ⊢ y : Bool
───────────────────────────
Γ ⊢ x == y : Bool
```

#### Rules

```dhall
x == True ≡ x

True == x ≡ x

(x == y) == z = x == (y == z)

x == x ≡ True
```

### Operator: `!=` <a op=diff />

The `!=` operator corresponds to boolean logical equality.  Carefully note that
this operator only works on `Bool` values.

```dhall
⊢ True != False

True
```

#### Type

Both arguments to the `!=` operator must have type `Bool` and the result will
have type `Bool`:

```
Γ ⊢ x : Bool   Γ ⊢ y : Bool
───────────────────────────
Γ ⊢ x != y : Bool
```

#### Rules

```dhall
False != x ≡ x

x != False ≡ x

(x != y) != z = x != (y != z)

x != x ≡ False
```

## Natural

A `Natural` number is an unsigned number without a fractional component.

#### Type

```
──────────────────
Γ ⊢ Natural : Type
```

… and unsigned literals without a decimal have type `Natural`:

```dhall
⊢ :type 0

Natural
```

### Literals: `Natural`

`Natural` numbers can be represented using decimal notation:

```dhall
⊢ :type 2

Natural
```

… or using hexadecimal notation:

```dhall
⊢ :type 0xFF

Natural

⊢ :type 0xff

Natural
```

#### Rules

A `Natural` number `n` is equivalent to adding `1` `n` times

```dhall
n = 0 + 1 + 1 + … + 1 + 1
      └─────────────────┘
            n times
```

### Operator: `+`

You can add two `Natural` numbers using the `+` operator.

```dhall
⊢ 2 + 3

5
```

#### Type

Both arguments to the `+` operator must have type `Natural` and the result will
have type `Natural`:

```
Γ ⊢ x : Natural   Γ ⊢ y : Natural
─────────────────────────────────
Γ ⊢ x + y : Natural
```

#### Rules

```dhall
x + 0 ≡ x

0 + x ≡ x

(x + y) + z = x + (y + z)
```

### Operator: `*`

You can multiply two `Natural` numbers using the `*` operator.

```dhall
⊢ 2 * 3

6
```

#### Type

Both arguments to the `*` operator must have type `Natural` and the result will
have type `Natural`:

```
Γ ⊢ x : Natural   Γ ⊢ y : Natural
─────────────────────────────────
Γ ⊢ x * y : Natural
```

#### Rules

```dhall
x * 1 ≡ x

1 * x ≡ x

(x * y) * z = x * (y * z)

x * 0 ≡ 0

0 * x ≡ 0

(x + y) * z = (x * z) + (y * z)

x * (y + z) = (x * y) + (x * z)
```

### Function: `Natural/even`

The `Natural/even` built-in function returns `True` if a number is even,
`False` otherwise.

```dhall
⊢ Natural/even 6

True
```

#### Type

The input to the `Natural/even` function must be a `Natural` number and the
output is a `Bool`:

```
─────────────────────────────────
Γ ⊢ Natural/even : Natural → Bool
```

#### Rules

```dhall
Natural/even 0 ≡ True

Natural/even (x + y) = Natural/even x == Natural/even y

Natural/even 1 ≡ False

Natural/even (x * y) = Natural/even x || Natural/even y
```

### Function: `Natural/odd`

The `Natural/odd` built-in function returns `True` if a number is odd, `False`
otherwise.

```dhall
⊢ Natural/odd 6

False
```

#### Type

```
────────────────────────────────
Γ ⊢ Natural/odd : Natural → Bool
```

#### Rules

```dhall
Natural/odd 0 ≡ False

Natural/odd (x + y) = Natural/odd x != Natural/odd y

Natural/odd 1 ≡ True

Natural/odd (x * y) = Natural/odd x && Natural/odd y
```

### Function: `Natural/isZero`

The `Natural/isZero` built-in function returns `True` if a number is `0`,
`False` otherwise.

```dhall
⊢ Natural/isZero 6

False
```

#### Type

```
───────────────────────────────────
Γ ⊢ Natural/isZero : Natural → Bool
```

#### Rules

```dhall
Natural/isZero 0 ≡ True

Natural/isZero (x + y) = Natural/isZero x && Natural/isZero y

Natural/isZero 1 ≡ False

Natural/isZero (x * y) = Natural/isZero x || Natural/isZero y
```

### Function: `Natural/subtract`

The `Natural/subtract` built-in function subtracts the first argument from the
second argument, clamping to `0` if the result is negative:

```dhall
⊢ Natural/subtract 1 3

2

⊢ Natural/subtract 3 1

0
```

#### Type

```
──────────────────────────────────────────────────
Γ ⊢ Natural/subtract : Natural → Natural → Natural
```

#### Rules

```dhall
Natural/subtract 0 x ≡ x

Natural/subtract x 0 ≡ 0

Natural/subtract x x ≡ 0

Natural/subtract y (x + y) = x

Natural/subtract (x + y) y = 0
```

### Function: `Natural/fold`

The `Natural/fold` built-in function is the most general way to consume a
`Natural` number by applying a function to an argument the specified number of
times.

```dhall
⊢ Natural/fold 40 Text (λ(t : Text) → t ++ "!") "Hello"

"Hello!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
```

#### Type

```
──────────────────────────────────────────────────────────────────────────────────────────────────────────
Γ ⊢ Natural/fold : Natural → ∀(natural : Type) → ∀(succ : natural → natural) → ∀(zero : natural) → natural
```

#### Rules

```dhall
Natural/fold 0 n s z = z

Natural/fold (x + y) n s z = Natural/fold x n s (Natural/fold y n s z)

Natural/fold 1 n s = s

Natural/fold (x * y) n s = Natural/fold x n (Natural/fold y n s)
```

### Function: `Natural/build`

The `Natural/build` built-in function is the most general way to create a
`Natural` number by specifying how many times to increment zero:

```dhall
⊢ Natural/build (λ(natural : Type) → λ(succ : natural → natural) → λ(zero : natural) → succ (succ zero))

2
```

#### Type

```
─────────────────────────────────────────────────────────────────────────────────────────────────────────────
Γ ⊢ Natural/build : (∀(natural : Type) → ∀(succ : natural → natural) → ∀(zero : natural) → natural) → Natural
```

#### Rules

```dhall
Natural/fold (Natural/build x) = x

Natural/build (Natural/fold x) = x
```

### Function `Natural/show`

The `Natural/show` built-in function renders a `Natural` number as `Text`, using
decimal notation:

```dhall
⊢ Natural/show 42

"42"
```

#### Type

```
─────────────────────────────────
Γ ⊢ Natural/show : Natural → Text
```

### Function `Natural/showHex`

The `Natural/showHex` built-in function renders a `Natural` number as `Text` in
hexadecimal notation, with a `0x` prefix and using uppercase letters:

```dhall
⊢ Natural/showHex 42

"0x2A"
```

#### Type

```
────────────────────────────────────
Γ ⊢ Natural/showHex : Natural → Text
```

### Function `Natural/toInteger`

The `Natural/toInteger` built-in function converts a `Natural` number to the
corresponding `Integer`:

```dhall
⊢ Natural/toInteger 2

+2
```

#### Type

```
─────────────────────────────────────────
Γ ⊢ Natural/toInteger : Natural → Integer
```

## Integer

An `Integer` is a positive or negative number without a fractional component.

#### Type

```
────────────────
Γ ⊢ Integer : Type
```

… and signed literals without a decimal component have type `Integer`:

```dhall
⊢ :type +2

Integer

⊢ :type -3

Integer
```

### Literals: `Integer`

`Integer`s can be represented using decimal notation:

```dhall
⊢ :type +2

Integer
```

… or hexadecimal notation:

```dhall
⊢ +0xFF

+255

⊢ +0xff

+255
```

### Function `Integer/negate`

The `Integer/negate` built-in function negates its argument:

```dhall
⊢ Integer/negate +2

-2

⊢ Integer/negate -3

+3
```

#### Type

```
──────────────────────────────────────
Γ ⊢ Integer/negate : Integer → Integer
```

#### Rules

```dhall
Integer/negate (Integer/negate x) = x
```

### Function `Integer/clamp`

The `Integer/clamp` built-in function converts an `Integer` to a `Natural`
number, clamping negative values to `0`:

```dhall
⊢ Integer/clamp +2

2

⊢ Integer/clamp -3

0
```

#### Type

```
─────────────────────────────────────
Γ ⊢ Integer/clamp : Integer → Natural
```

#### Rules

```dhall
Natural/isZero (Integer/clamp -x) = True

Integer/clamp (Natural/toInteger x) = x

Natural/isZero (Integer/clamp x) || Natural/isZero (Integer/clamp (Integer/negate x)) = True
```

### Function `Integer/toDouble`

The `Integer/toDouble` built-in function converts an `Integer` to a `Double`:

```dhall
⊢ Integer/toDouble -3

-3.0
```

#### Type

```
───────────────────────────────────────
Γ ⊢ Integer/toDouble : Integer → Double
```

### Function `Integer/show`

The `Integer/show` built-in function renders an `Integer` as `Text`, using
decimal notation:

```dhall
⊢ Integer/show +2

"+2"

⊢ Integer/show -3

"-3"
```

#### Type

```
─────────────────────────────────
Γ ⊢ Integer/show : Integer → Text
```

## Double

A `Double` is an IEEE 754 double-precision floating-point number.

#### Type

```
────────────────
Γ ⊢ Double : Type
```

… and numeric literals with a decimal have type `Double`:

```dhall
⊢ :type 3.14159

Double
```

### Literals: `Double`

A `Double` literal must have either at least one decimal place:

```dhall
⊢ :type 1.0

Double
```

…or an exponent:

```dhall
⊢ :type -2e10

Double
```

… or both:

```dhall
⊢ :type 6.0221409e+23

Double
```

### Function `Double/show`

The `Double/show` built-in function renders a `Double` as `Text` using decimal
notation:

```dhall
⊢ Double/show 2.0

"2.0"

⊢ Double/show -1e2

"-100.0"
```

#### Type

```
───────────────────────────────
Γ ⊢ Double/show : Double → Text
```

## Text

`Text` represents human-readable text.

#### Type

```
────────────────
Γ ⊢ Text : Type
```

### Literals: `Text`

A `Text` literal is either a double-quoted string literal with JSON-style
escaping rules or a Nix-style multi-line string literal:

```dhall
⊢ :type "ABC"

Text
```

```dhall
⊢ :paste
-- Entering multi-line mode. Press <Ctrl-D> to finish.
| :type
| ''
|     Line 1
|     Line 2
| ''
| 

Text
```

### Function `Text/show`

The `Text/show` built-in function renders a `Text` literal as a valid JSON
string:

```dhall
⊢ Text/show "ABC"

"\"ABC\""

⊢ Text/show "\n🎉"

"\"\\n🎉\""
```

### Function `Text/replace`

The `Text/replace` built-in function modifies a substring of a given `Text` literal. It takes 3 arguments, the `Text` literal substring to match, the `Text` literal replacement, and the `Text` literal in which to replace all matches:

```dhall
⊢ Text/replace "foo" "bar" "foobar"

"barbar"
```

#### Type

```
──────────────────────────────────────
Γ ⊢ Text/replace : ∀(needle : Text) → ∀(replacement : Text) → ∀(haystack : Text) → Text
```

#### Type

```
───────────────────────────
Γ ⊢ Text/show : Text → Text
```

### Operator: `++`

You can concatenate `Text` using the `++` operator:

```dhall
⊢ "Hello, " ++ "world!"

"Hello, world!"
```

#### Type

Both arguments to the `++` operator must have type `Text` and the result will
have type `Text`:


```
Γ ⊢ x : Text   Γ ⊢ y : Text
───────────────────────────
Γ ⊢ x ++ y : Text
```

#### Rules

```dhall
x ++ "" ≡ x

"" ++ x ≡ x

(x ++ y) ++ z = x ++ (y ++ z)
```

## Date

`Date` represents a day of the year

#### Type

```
────────────────
Γ ⊢ Date : Type
```

### Literals: `Date`

A `Date` literal has the form `YYYY-MM-DD`

```dhall
⊢ :type 2000-01-01

Date
```

### Function `Date/show`

The `Date/show` built-in function renders a `Date` as a valid Dhall literal:

```dhall
⊢ Date/show 2000-01-01

"2000-01-01"
```

## Time

`Time` represents a time of day

#### Type

```
────────────────
Γ ⊢ Time : Type
```

### Literals: `Time`

A `Time` literal has the form `HH:MM:SS` and the seconds may have a fractional
component.

```dhall
⊢ :type 00:00:00

Time

⊢ :type 11:59:59.99

Time
```

### Function `Time/show`

The `Time/show` built-in function renders a `Time` as a valid Dhall literal:

```dhall
⊢ Date/show 00:00:00

"00:00:00"
```

## TimeZone

`TimeZone` represents a time offset.

#### Type

```
────────────────
Γ ⊢ TimeZone : Type
```

### Literals: `TimeZone`

A `TimeZone` literal has the form `±HH:MM`.

```dhall
⊢ :type +07:00

TimeZone

⊢ :type -05:00

TimeZone
```

### Function `TimeZone/show`

The `TimeZone/show` built-in function renders a `TimeZone` as a valid Dhall literal:

```dhall
⊢ TimeZone/show +00:00

"+00:00"
```

## List

A `List` is an ordered sequence of elements, all of which have the same type.

#### Type

```
──────────────────────
Γ ⊢ List : Type → Type
```

### Literals: `List`

A `List` literal is a sequence of 0 or more comma-separated values inside
square brackets.

An empty `List` literal must end with a type annotation.

```dhall
⊢ :type [ 1, 2, 3 ]

List Natural

⊢ :type [] : List Natural

List Natural
```

#### Type

If each element of a `List` has type `T`, then the type of the `List` is
`List T`

```
Γ ⊢ T : Type   Γ ⊢ x : T   Γ ⊢ y : T   …
────────────────────────────────────────
Γ ⊢ [ x, y, … ] : List T
```

#### Rules

```dhall
[ a, b, c, …, x, y, z ] = [ a ] # [ b ] # [ c ] # … # [ x ] # [ y ] # [ z ]
```

### Operator: `#`

You can concatenate `List`s using the `#` operator:

```dhall
⊢ [ 1, 2, 3 ] # [ 4, 5, 6 ]

[ 1, 2, 3, 4, 5, 6 ]
```

#### Type

Both arguments to the `#` operator must be `List`s that share the same type and
the result will also be a `List` that shares the same type:

```
Γ ⊢ x : List a    Γ ⊢ y : List a
─────────────────────────────────
Γ ⊢ x # y : List a
```

#### Rules

```dhall
([] : List a) # xs ≡ xs

xs # ([] : List a) ≡ xs

(xs # ys) # zs = xs # (ys # zs)
```

### Function: `List/fold`

The `List/fold` built-in function is the most general way to consume a `List`:

```dhall
⊢ List/fold Bool [True, False, True] Bool (λ(x : Bool) → λ(y : Bool) → x && y) True

False
```

#### Type

```
────────────────────────────────────────────────────────────────────────────────────────────────────────
Γ ⊢ List/fold : ∀(a : Type) → List a → ∀(list : Type) → ∀(cons : a → list → list) → ∀(nil : list) → list
```

#### Rules

```dhall
List/fold a ([] : List a) b c n = n

List/fold a (xs # ys) b c n = List/fold a xs b c (List/fold ys b c n)

List/fold a ([x] : List a) b c = c x
```

### Function: `List/build`

The `List/build` built-in function is the most general way to create a `List`:

```dhall
⊢ List/build Natural (λ(list : Type) → λ(cons : Natural → list → list) → λ(nil : list) → cons 1 (cons 2 (cons 3 nil)))

[ 1, 2, 3 ]
```

#### Type

```
───────────────────────────────────────────────────────────────────────────────────────────────────────────
Γ ⊢ List/build : ∀(a : Type) → (∀(list : Type) → ∀(cons : a → list → list) → ∀(nil : list) → list) → List a
```

#### Rules

```dhall
List/build t (List/fold t x) = x

List/fold t (List/build t x) = x
```

### Function: `List/length`

The `List/length` built-in function returns the length of a `List`:

```dhall
⊢ List/length Natural [ 1, 2, 3 ]

3
```

#### Type

```
────────────────────────────────────────────────
Γ ⊢ List/length : ∀(a : Type) → List a → Natural
```

#### Rules

```dhall
List/length t ([] : List t) = 0

List/length t (xs # ys) = List/length t xs + List/length t ys

List/length t [ x ] = 1
```

### Function: `List/head`

The `List/head` built-in function returns the first element of a `List` wrapped
in a `Some`, and `None` otherwise.

```dhall
⊢ List/head Natural [ 1, 2, 3 ]

Some 1
```

#### Type

```
───────────────────────────────────────────────
Γ ⊢ List/head ∀(a : Type) → List a → Optional a
```

#### Rules

```dhall
List/head a ([] : List a) = None a

List/head a (xs # ys) =
  let combine =
        λ(a : Type) →
        λ(l : Optional a) →
        λ(r : Optional a) →
          merge { None = r, Some = λ(x : a) → Some x } l

  in  combine a (List/head a xs) (List/head a ys)

List/head a [ x ] = Some x
```

### Function: `List/last`

The `List/last` built-in function returns the last element of a `List` wrapped
in a `Some`, and `None` otherwise:

```dhall
⊢ List/last Natural [ 1, 2, 3 ]

Some 3
```

#### Type

```
─────────────────────────────────────────────────
Γ ⊢ List/last : ∀(a : Type) → List a → Optional a
```

#### Rules

```dhall
List/last a ([] : List a) = None a

List/last a (xs # ys) =
  let combine =
        λ(a : Type) →
        λ(l : Optional a) →
        λ(r : Optional a) →
          merge { None = l, Some = λ(x : a) → Some x } r

  in  combine a (List/last a xs) (List/last a ys)

List/last a [ x ] = Some x
```

### Function: `List/indexed`

The `List/indexed` built-in function tags each element of a `List` with its
index.

```dhall
⊢ List/indexed Text [ "ABC", "DEF", "GHI" ]

[ { index = 0, value = "ABC" }
, { index = 1, value = "DEF" }
, { index = 2, value = "GHI" }
]
```

#### Type

```
─────────────────────────────────────────────────────────────────────────────
Γ ⊢ List/indexed : ∀(a : Type) → List a → List { index : Natural, value : a }
```

#### Rules

```dhall
List/indexed a ([] : List a) = [] : List { index : Natural, value : a }

List/indexed a (xs # ys) =
  let combine =
        λ(a : Type) →
        λ(xs : List { index : Natural, value : a }) →
        λ(ys : List { index : Natural, value : a }) →
            xs
          # List/build
              { index : Natural, value : a }
              ( λ(list : Type) →
                λ(cons : { index : Natural, value : a } → list → list) →
                  List/fold
                    { index : Natural, value : a }
                    ys
                    list
                    ( λ(x : { index : Natural, value : a }) →
                        cons
                          { index =
                                x.index
                              + List/length { index : Natural, value : a } xs
                          , value = x.value
                          }
                    )
              )

  in  combine a (List/indexed a xs) (List/indexed a ys)

List/indexed a [ x ] = [ { index = 0, value = x } ]
```

### Function: `List/reverse`

The `List/reverse` built-in function reverses a `List`:

```dhall
⊢ List/reverse Natural [ 1, 2, 3 ]

[ 3, 2, 1 ]
```

#### Type

```
─────────────────────────────────────────────────
Γ ⊢ List/reverse : ∀(a : Type) → List a → List a
```

#### Rules

```dhall
List/reverse a ([] : List a) = [] : List a

List/reverse a [ x ] = [ x ]

List/reverse a (List/reverse a xs) = xs

List/reverse a (xs # ys) = List/reverse a ys # List/reverse a xs

List/head a (List/reverse a xs) = List/last a xs

List/last a (List/reverse a xs) = List/head a xs

List/length a (List/reverse a xs) = List/length a xs
```

## Optional

An `Optional` value represents a value that might be present or absent.

#### Type

```
──────────────────────────
Γ ⊢ Optional : Type → Type
```

### Literals: `Optional`

An `Optional` literal is either a present value wrapped in a `Some` or an
absent value using `None` followed by a type.


```dhall
⊢ :type Some 1

Optional Natural

⊢ :type None Natural

Optional Natural
```

#### Type

If you wrap a value of type `T` in a `Some`, the final type is `Optional T`:

```
Γ ⊢ T : Type   Γ ⊢ x : T
────────────────────────
Γ ⊢ Some x : Optional T
```

```
───────────────────────
Γ ⊢ None : ∀(T : Type) → Optional T
```

## Records

### Record types

A record type is a sequence of 0 or more key-type pairs inside curly braces.

```dhall
⊢ :type { foo : Natural, bar : Bool }

Type

⊢ :type {}

Type
```

#### Rules

```dhall
{ k₀ : T₀, k₁ : T₁, k₂ : T₂, … } = { k₀ : T₀ } ⩓ { k₁ : T₁ } ⩓ { k₂ : T₂ } ⩓ …
```

### Record values

A record value is a sequence of 0 or more key-value pairs inside curly braces.

An empty record literal must have a single `=` sign between the curly braces to
distinguish the empty record literal from the empty record type.

```dhall
⊢ :type { foo = 1, bar = True }

{ bar : Bool, foo : Natural }

⊢ :type {=}

{}
```

#### Rules

```dhall
{ k₀ = v₀, k₁ = v₁, k₂ = v₂, … } = { k₀ = v₀ } ∧ { k₁ = v₁ } ∧ { k₂ = v₂ } ∧ …
```

### Operator: `⩓`

* ASCII: `//\\`
* Unicode: U+2A53

The `⩓` operator recursively merges record types.

```dhall
⊢ { foo : { bar : Bool } } ⩓ { foo : { baz : Text }, qux : List Natural }

{ foo : { bar : Bool, baz : Text }, qux : List Natural }
```

#### Rules

```dhall
x ⩓ {} = x

{} ⩓ x = x

(x ⩓ y) ⩓ z = x ⩓ (y ⩓ z)
```

### Operator: `∧`

* ASCII: `/\`
* Unicode: U+2227

The `∧` operator recursively merges record values

```dhall
⊢ { foo = { bar = True } } ∧ { foo = { baz = "ABC" }, qux = [1, 2, 3] }

{ foo = { bar = True, baz = "ABC" }, qux = [ 1, 2, 3 ] }
```

#### Rules

```dhall
x ∧ {=} ≡ x

{=} ∧ x ≡ x

(x ∧ y) ∧ z = x ∧ (y ∧ z)
```

### Operator: `⫽`

* ASCII: `//`
* Unicode: U+2AFD

The `⫽` operator non-recursively merges record values, preferring fields from
the right record when they conflict

```dhall
⊢ { foo = 1, bar = True } ⫽ { foo = 2 }

{ bar = True, foo = 2 }
```

#### Rules

```dhall
x ⫽ {=} ≡ x

{=} ⫽ x ≡ x

(x ⫽ y) ⫽ z = x ⫽ (y ⫽ z)
```

### Operator: `::`

The `::` operator auto-completes a record given a provided "schema" (a record
containing the expected `Type` and `default` values):

```dhall
⊢ :paste
-- Entering multi-line mode. Press <Ctrl-D> to finish.
| let Example = { Type = { foo : Natural, bar : Bool }, default = { bar = False } }
| in  Example::{ foo = 1 }
| 

{ bar = False, foo = 1 }
```

#### Rules

```dhall
T::r = (T.default ⫽ r) : T.Type
```

### Keyword: `toMap`

The `toMap` keyword converts a record literal to a `List` of key-value pairs:

```dhall
⊢ toMap { foo = 2, bar = 3 }

[ { mapKey = "bar", mapValue = 3 }, { mapKey = "foo", mapValue = 2 } ]
```

#### Rules

```dhall
toMap (x ∧ y) = toMap x # toMap y

toMap {=} : T = [] : T
```

### Keyword: `with`

The `with` keyword performs a nested record update:

```dhall
⊢ { bio = { name = "Jane Doe", age = 24 }, job = "Engineer" } with bio.age = 30

{ bio = { age = 30, name = "Jane Doe" }, job = "Engineer" }
```

These record updates can change a field's type:

```dhall
⊢ { foo = 1 } with foo = True
{ foo = True }
```

You can also update a value nested inside of an `Optional` value using `?` as a
path component:

```dhall
⊢ (Some { foo = 1 }) with ?.foo = 2

Some { foo = 2 }

⊢ (None { foo : Natural }) with ?.foo = 2

None { foo : Natural }
```

## Unions

### Keyword: `merge`

The `merge` keyword consumes a union value by providing one handler for
each possible alternative.

```dhall
⊢ :let Example = < Left : Natural | Right : Bool >

Example : Type

⊢ :let handlers = { Left = Natural/even, Right = λ(b : Bool) → b }

handlers : { Left : Natural → Bool, Right : ∀(b : Bool) → Bool }

⊢ merge handlers (Example.Left 1)

False

⊢ merge handlers (Example.Right True)

True
```

The `merge` keyword also works on `Optional` values, too:

```dhall
⊢ :let handlers = { Some = Natural/even, None = False }

handlers : { None : Bool, Some : Natural → Bool }

⊢ merge handlers (Some 2)

True

⊢ merge handlers (None Natural)

False
```

### Keyword: `showConstructor`

The `showConstructor` keyword converts a union value to a `Text` representation
of the union constructor's name.

```dhall
⊢ :let Example = < Left : Natural | Right : Bool >

Example : Type

⊢ showConstructor (Example.Left 0)

"Left"

⊢ showConstructor (Example.Right True)

"Right"
```

The `showConstructor` keyword also works on `Optional` values, too:

```dhall
⊢ showConstructor (None Natural)

"None"

⊢ showConstructor (Some 1)

"Some"
```

## Imports

An import is either:

* … a remote import (e.g. HTTP / HTTPS request),
* … a file import (absolute, relative, or home-anchored),
* … an environment variable import, or:
* … the `missing` keyword (an import guaranteed to fail)

```dhall
⊢ https://prelude.dhall-lang.org/v17.1.0/Bool/not.dhall

λ(b : Bool) → b == False

⊢ ~/.ssh/config as Text

''
Host *
    AddKeysToAgent yes
''

⊢ env:SHLVL

1
```

### Keyword: `missing`

```dhall
⊢ missing

Error: No valid imports

1│ missing

(input):1:1
```

### Operator: `?`

The `?` operator attempts to resolve imports for the left expression, falling
back to the right expression if the left expression fails to resolve.

```dhall
⊢ missing ? https://prelude.dhall-lang.org/v17.1.0/Bool/not.dhall

λ(b : Bool) → b == False
```

#### Rules

```dhall
missing ? x = x

x ? missing = x

(x ? y) ? z = x ? (y ? z)
```

### Keyword: `as Text`

Adding `as Text` to an import causes the import to return the raw `Text` for
that import instead of a Dhall expression:

```dhall
⊢ https://example.com as Text

''
<!doctype html>
<html>
<head>
    <title>Example Domain</title>

    <meta charset="utf-8" />
    <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <style type="text/css">
    body {
        background-color: #f0f0f2;
        margin: 0;
        padding: 0;
        font-family: -apple-system, system-ui, BlinkMacSystemFont, "Segoe UI", "Open Sans", "Helvetica Neue", Helvetica, Arial, sans-serif;
        
    }
    div {
        width: 600px;
        margin: 5em auto;
        padding: 2em;
        background-color: #fdfdff;
        border-radius: 0.5em;
        box-shadow: 2px 3px 7px 2px rgba(0,0,0,0.02);
    }
    a:link, a:visited {
        color: #38488f;
        text-decoration: none;
    }
    @media (max-width: 700px) {
        div {
            margin: 0 auto;
            width: auto;
        }
    }
    </style>    
</head>

<body>
<div>
    <h1>Example Domain</h1>
    <p>This domain is for use in illustrative examples in documents. You may use this
    domain in literature without prior coordination or asking for permission.</p>
    <p><a href="https://www.iana.org/domains/example">More information...</a></p>
</div>
</body>
</html>
''
```

### Keyword: `using`

The `using` keyword lets you add headers to an HTTP(S) request:

```dhall
⊢ https://httpbin.org/headers using (toMap { User-Agent = "dhall" }) as Text

''
{
  "headers": {
    "Accept-Encoding": "gzip", 
    "Host": "httpbin.org", 
    "User-Agent": "dhall", 
    "X-Amzn-Trace-Id": "Root=1-5f49b61e-263a9e784179107a83d8714a"
  }
}
''
```

## Other

The following keywords are not associated with any particular type.

### Keyword: `let`

You can name expressions using the `let` keyword:

```dhall
⊢ :paste
-- Entering multi-line mode. Press <Ctrl-D> to finish.
| let x = 1
| 
| let y : Natural = 2
| 
| in  x + y
| 

3
```

### Keyword: `assert`

You can write a test to verify that two expressions are equal using the
`assert` keyword combined with the `≡` operator:

```dhall
⊢ assert : 2 + 2 ≡ 4

assert : 4 ≡ 4
```
