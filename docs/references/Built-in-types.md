# Built in types, functions, and operators

This section describes all of the types, functions, and operators built into the
Dhall language.

Note that this page does not yet include certain keywords, such as:

* `let`/`in` - Used to define intermediate expressions
* `merge` - Used to consume unions
* `toMap` - Used to convert records to dictionaries
* `using`/`as` - Used to modify imports
* `assert` - Used to write tests

However, in the meantime you can still consult the [[Cheatsheet|Cheatsheet]]
to get an intuition for how these keywords work.

* [`Bool`](#bool)
    * [Literals](#literals-bool)
    * [`if`/`then`/`else`](#construct-ifthenelse)
    * [`||`](#operator-)
    * [`&&`](#operator--1)
    * [`==`](#operator--2)
    * [`!=`](#operator--3)
* [`Natural`](#natural)
    * [Literals](#literals-natural)
    * [`+`](#operator--4)
    * [`*`](#operator--5)
    * [`Natural/even`](#function-naturaleven)
    * [`Natural/odd`](#function-naturalodd)
    * [`Natural/isZero`](#function-naturaliszero)
    * [`Natural/subtract`](#function-naturalsubtract)
    * [`Natural/fold`](#function-naturalfold)
    * [`Natural/build`](#function-naturalbuild)
    * [`Natural/show`](#function-naturalshow)
    * [`Natural/toInteger`](#function-naturaltointeger)
* [`Integer`](#integer)
    * [Literals](#literals-integer)
    * [`Integer/toDouble`](#function-integertodouble)
    * [`Integer/show`](#function-integershow)
* [`Double`](#double)
    * [Literals](#literals-double)
    * [`Double/show`](#function-doubleshow)
* [`Text`](#text)
    * [Literals](#literals-text)
    * [`Text/show`](#function-textshow)
    * [`++`](#operator--6)
* [`List`](#list)
    * [Literals](#literals-list)
    * [`#`](#operator--7)
    * [`List/fold`](#function-listfold)
    * [`List/build`](#function-listbuild)
    * [`List/length`](#function-listlength)
    * [`List/head`](#function-listhead)
    * [`List/last`](#function-listlast)
    * [`List/indexed`](#function-listindexed)
    * [`List/reverse`](#function-listreverse)
* [`Optional`](#optional)
    * [`Optional`](#literals-optional)
    * [`Optional/fold`](#function-optionalfold)
    * [`Optional/build`](#function-optionalbuild)
* [Records](#records)
    * [Record types](#record-types)
    * [Record values](#record-values)
    * [`⩓`](#operator--8)
    * [`∧`](#operator--9)
    * [`⫽`](#operator--10)
    * [`::`](#operator--11)

## Bool

#### Example

```bash
$ dhall --annotate <<< 'Bool'
```
```dhall
Bool : Type
```

#### Type

```
────────────────
Γ ⊢ Bool : Type
```

### Literals: `Bool`

#### Example

```bash
$ dhall --annotate <<< 'True'
```
```dhall
True : Bool
```

#### Type

```
───────────────
Γ ⊢ True : Bool
```

```
────────────────
Γ ⊢ False : Bool
```

### Construct: `if`/`then`/`else`

#### Example

```bash
$ dhall <<< 'if True then 3 else 5'
```
```dhall
3
```

#### Type

```
               Γ ⊢ t : Type
               ─────────────────────
Γ ⊢ b : Bool   Γ ⊢ l : t   Γ ⊢ r : t
────────────────────────────────────
Γ ⊢ if b then l else r : t
```

#### Rules

```dhall
if b then True else False = b

if True  then l else r = l

if False then l else r = r
```

### Operator: `||`

#### Example

```bash
$ dhall <<< 'True || False'
```
```dhall
True
```

#### Type

```
Γ ⊢ x : Bool   Γ ⊢ y : Bool
───────────────────────────
Γ ⊢ x || y : Bool
```

#### Rules

```dhall
x || False = x

False || x = x

(x || y) || z = x || (y || z)

x || True = True

True || x = True

x || (y && z) = (x || y) && (x || z)

(x && y) || z = (x || z) && (y || z)
```

### Operator: `&&`

#### Example

```bash
$ dhall <<< 'True && False'
```
```dhall
False
```

#### Type

```
Γ ⊢ x : Bool   Γ ⊢ y : Bool
───────────────────────────
Γ ⊢ x && y : Bool
```

#### Rules

```dhall
x && True = x

True && x = x

(x && y) && z = x && (y && z)

x && False = False

False && x = False

x && (y || z) = (x && y) || (x && z)

(x || y) && z = (x && z) || (y && z)
```

### Operator: `==`

#### Example

```bash
$ dhall <<< 'True == False'
```
```dhall
False
```

#### Type

```
Γ ⊢ x : Bool   Γ ⊢ y : Bool
───────────────────────────
Γ ⊢ x == y : Bool
```

#### Rules

```dhall
x == x = True

x == True = x

True == x = x

(x == y) == z = x == (y == z)
```

### Operator: `!=`

#### Example

```bash
$ dhall <<< 'True != False'
```
```dhall
True
```

#### Type

```
Γ ⊢ x : Bool   Γ ⊢ y : Bool
───────────────────────────
Γ ⊢ x != y : Bool
```

#### Rules

```dhall
x != x = False

False != x = x

x != False = x

(x != y) != z = x != (y != z)
```

## Natural

#### Example

```bash
$ dhall --annotate <<< 'Natural'
```
```dhall
Natural : Type
```

#### Type

```
──────────────────
Γ ⊢ Natural : Type
```

### Literals: `Natural`

A `Natural` number literal is an unsigned non-negative integer

#### Example

```bash
$ dhall --annotate <<< '2'
```
```dhall
2 : Natural
```

#### Type

```
────────────────
Γ ⊢ n : Natural
```

#### Rules

```dhall
n = 1 + 1 + … + 1 + 1  -- n times
```

### Operator: `+`

#### Example

```bash
$ dhall <<< '2 + 3'
```
```dhall
5
```

#### Type

```
Γ ⊢ x : Natural   Γ ⊢ y : Natural
─────────────────────────────────
Γ ⊢ x + y : Natural
```

#### Rules

```dhall
x + 0 = x

0 + x = x

(x + y) + z = x + (y + z)
```

### Operator: `*`

#### Example

```bash
$ dhall <<< '2 * 3'
```
```dhall
6
```

#### Type

```
Γ ⊢ x : Natural   Γ ⊢ y : Natural
─────────────────────────────────
Γ ⊢ x * y : Natural
```

#### Rules

```dhall
x * 1 = x

1 * x = x

(x * y) * z = x * (y * z)

x * 0 = 0

0 * x = 0

(x + y) * z = (x * z) + (y * z)

x * (y + z) = (x * y) + (x * z)
```

### Function: `Natural/even`

#### Example

```bash
$ dhall <<< 'Natural/even 6'
```
```dhall
True
```

#### Type

```
─────────────────────────────────
Γ ⊢ Natural/even : Natural → Bool
```

#### Rules

```dhall
Natural/even 0 = True

Natural/even (x + y) = Natural/even x == Natural/even y

Natural/even 1 = False

Natural/even (x * y) = Natural/even x || Natural/even y
```

### Function: `Natural/odd`

#### Example

```bash
$ dhall <<< 'Natural/odd 6'
```
```dhall
False
```

#### Type

```
────────────────────────────────
Γ ⊢ Natural/odd : Natural → Bool
```

#### Rules

```dhall
Natural/odd 0 = False

Natural/odd (x + y) = Natural/odd x != Natural/odd y

Natural/odd 1 = True

Natural/odd (x * y) = Natural/odd x && Natural/odd y
```

### Function: `Natural/isZero`

#### Example

```bash
$ dhall <<< 'Natural/isZero 6'
```
```dhall
False
```

#### Type

```
───────────────────────────────────
Γ ⊢ Natural/isZero : Natural → Bool
```

#### Rules

```dhall
Natural/isZero 0 = True

Natural/isZero (x + y) = Natural/isZero x && Natural/isZero y

Natural/isZero 1 = False

Natural/isZero (x * y) = Natural/isZero x || Natural/isZero y
```

### Function: `Natural/subtract`

#### Examples

```bash
$ dhall <<< 'Natural/subtract 1 3'
```
```dhall
2
```
```bash
$ dhall <<< 'Natural/subtract 3 1'
```
```dhall
0
```

#### Type

```
──────────────────────────────────────────────────
Γ ⊢ Natural/subtract : Natural → Natural → Natural
```

#### Rules

```dhall
Natural/subtract 0 x = x

Natural/subtract x 0 = 0

Natural/subtract x x = 0

Natural/subtract y (x + y) = x
```

### Function: `Natural/fold`

#### Example

```bash
$ dhall <<< 'Natural/fold 40 Text (λ(t : Text) → t ++ "!") "Hello"'
```
```dhall
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

#### Example

```bash
$ dhall <<< 'Natural/build (λ(natural : Type) → λ(succ : natural → natural) → λ(zero : natural) → succ (succ zero))'
```
```dhall
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

#### Example

```bash
$ dhall <<< 'Natural/show 42'
```
```dhall
"42"
```

#### Type

```
─────────────────────────────────
Γ ⊢ Natural/show : Natural → Text
```

### Function `Natural/toInteger`

#### Example

```bash
$ dhall <<< 'Natural/toInteger 2'
```
```dhall
+2
```

#### Type

```
─────────────────────────────────────────
Γ ⊢ Natural/toInteger : Natural → Integer
```

## Integer

#### Example

```bash
$ dhall --annotate <<< 'Integer'
```
```dhall
Integer : Type
```

#### Type

```
────────────────
Γ ⊢ Integer : Type
```

### Literals: `Integer`

An `Integer` literal is a either a non-negative integer prefixed with a `+` or
a negative integer prefixed with a `-`.

#### Examples

```bash
$ dhall --annotate <<< '+3'
```
```dhall
+3 : Integer
```

```bash
$ dhall --annotate <<< '-2'
```
```dhall
-2 : Integer
```

#### Type

```
────────────────
Γ ⊢ ±n : Integer
```

### Function `Integer/toDouble`

#### Example

```bash
$ dhall <<< 'Integer/toDouble +2'
```
```dhall
2.0
```

```bash
$ dhall <<< 'Integer/toDouble -3'
```
```dhall
-3.0
```

#### Type

```
───────────────────────────────────────
Γ ⊢ Integer/toDouble : Integer → Double
```

### Function `Integer/show`

#### Example

```bash
$ dhall <<< 'Integer/show +2'
```
```dhall
"+2"
```

```bash
$ dhall <<< 'Integer/show -3'
```
```dhall
"-3"
```

#### Type

```
─────────────────────────────────
Γ ⊢ Integer/show : Integer → Text
```

## Double

#### Example

```bash
$ dhall --annotate <<< 'Double'
```
```dhall
Double : Type
```

#### Type

```
────────────────
Γ ⊢ Double : Type
```

### Literals: `Double`

A `Double` literal must have either at least one decimal place or an exponent
(or both):

#### Examples

```bash
$ dhall --annotate <<< '3.14159'
```
```dhall
3.14159 : Double
```

```bash
$ dhall --annotate <<< '-2e10'
```
```dhall
-2.0e10 : Double
```

#### Type

```
────────────────
Γ ⊢ n.n : Double
```

### Function `Double/show`

#### Example

```bash
$ dhall <<< 'Double/show 2.0'
```
```dhall
"2.0"
```

```bash
$ dhall <<< 'Double/show -1e2'
```
```dhall
"-100"
```

#### Type

```
───────────────────────────────
Γ ⊢ Double/show : Double → Text
```

## Text

#### Example

```bash
$ dhall --annotate <<< 'Text'
```
```dhall
Text : Type
```

#### Type

```
────────────────
Γ ⊢ Text : Type
```

### Literals: `Text`

A `Text` literal is either a double-quoted string literal with JSON-style
escaping rules or a Nix-style multi-line string literal:

#### Examples

```bash
$ dhall --annotate <<< '"ABC"'
```
```dhall
"ABC" : Text
```

```bash
$ dhall <<EOF
> ''
>     Line 1
>     Line 2
> ''
> EOF
```
```dhall
"Line 1\nLine 2\n"
```

#### Type

```
────────────────
Γ ⊢ "…" : Text
```

#### Rules

```dhall
"abc…xyz" = "a" ++ "b" ++ "c" ++ … ++ "x" ++ "y" ++ "z"
```

### Function `Text/show`

#### Example

```bash
$ dhall <<< 'Text/show "ABC"'
```
```dhall
"\"ABC\""
```

```bash
$ dhall <<< 'Text/show "\n🎉"'
```
```dhall
"\"\\n🎉\""
```

#### Type

```
───────────────────────────
Γ ⊢ Text/show : Text → Text
```

### Operator: `++`

#### Example

```bash
$ dhall <<< '"Hello, " ++ "world!"'
```
```dhall
"Hello, world!"
```

#### Type

```
Γ ⊢ x : Text   Γ ⊢ y : Text
───────────────────────────
Γ ⊢ x ++ y : Text
```

#### Rules

```dhall
(x ++ y) ++ z = x ++ (y ++ z)

x ++ "" = x

"" ++ x = x
```

## List

#### Example

```bash
$ dhall --annotate <<< 'List'
```
```dhall
List : Type → Type
```

#### Type

```
──────────────────────
Γ ⊢ List : Type → Type
```

### Literals: `List`

A `List` literal is a sequence of 0 or more comma-separated values inside
square brackets.

An empty `List` literal must end with a type annotation.

#### Examples

```bash
$ dhall --annotate <<< '[ 1, 2, 3 ]'
```
```dhall
[ 1, 2, 3 ] : List Natural
```

```bash
dhall <<< '[] : List Natural'
```
```dhall
[] : List Natural
```

#### Type

```
Γ ⊢ t : Type   Γ ⊢ x : t   Γ ⊢ y : t   …
────────────────────────────────────────
Γ ⊢ [x, y, … ] : List t
```

#### Rules

```dhall
[ a, b, c, …, x, y, z ] = [ a ] # [ b ] # [ c ] # … # [ x ] # [ y ] # [ z ]
```

### Operator: `#`

#### Example

```bash
$ dhall <<< '[ 1, 2, 3] # [ 4, 5, 6 ]'
```
```dhall
[ 1, 2, 3, 4, 5, 6, ]
```

#### Type

```
Γ ⊢ x : List a    Γ ⊢ y : List a
─────────────────────────────────
Γ ⊢ x # y : List a
```

#### Rules

```dhall
([] : List a) # xs = xs

xs # ([] : List a) = xs

(xs # ys) # zs = xs # (ys # zs)
```

### Function: `List/fold`

#### Example

```bash
$ dhall <<< 'List/fold Bool [True, False, True] Bool (λ(x : Bool) → λ(y : Bool) → x && y) True'
```
```dhall
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

#### Example

```bash
$ dhall <<< 'List/build Natural (λ(list : Type) → λ(cons : Natural → list → list) → λ(nil : list) → cons 1 (cons 2 (cons 3 nil)))'
```
```dhall
[1, 2, 3]
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

#### Example

```bash
$ dhall <<< 'List/length Natural [ 1, 2, 3 ]'
```
```dhall
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

#### Function: `List/head`

```bash
$ dhall <<< 'List/head Natural [ 1, 2, 3 ]'
```
```dhall
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
              λ(a : Type)
            → λ(l : Optional a)
            → λ(r : Optional a)
            → Optional/fold a l (Optional a) (λ(x : a) → Some x) r
  in  combine a (List/head a xs) (List/head a ys)

List/head a [ x ] = Some x
```

### Function: `List/last`

#### Example

```bash
$ dhall <<< 'List/last Natural [ 1, 2, 3 ]'
```
```dhall
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
              λ(a : Type)
            → λ(l : Optional a)
            → λ(r : Optional a)
            → Optional/fold a r (Optional a) (λ(x : a) → Some x) l
  in  combine a (List/last a xs) (List/last a ys)

List/last a [ x ] = Some x
```

### Function: `List/indexed`

#### Example

```bash
$ dhall <<< 'List/indexed Text [ "ABC", "DEF", "GHI" ]'
```
```dhall
[{ index = 0, value = "ABC" }, { index = 1, value = "DEF" }, { index = 2, value = "GHI" }] : List { index : Natural, value : Text }
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
          λ(a : Type)
        → λ(xs : List { index : Natural, value : a })
        → λ(ys : List { index : Natural, value : a })
        →   xs
          # List/build
            { index : Natural, value : a }
            (   λ(list : Type)
              → λ(cons : { index : Natural, value : a } → list → list)
              → List/fold
                { index : Natural, value : a }
                ys
                list
                (   λ(x : { index : Natural, value : a })
                  → cons
                    { index = x.index + List/length { index : Natural, value : a } xs
                    , value = x.value
                    }
                )
            )
  in  combine a (List/indexed a xs) (List/indexed a ys)

List/indexed a [ x ] = [ { index = 0, value = x } ]
```

### Function: `List/reverse`

#### Example

```bash
$ dhall <<< 'List/reverse Natural [ 1, 2, 3 ]'
```
```dhall
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

List/reverse a (xs # ys) = List/reverse a ys # List/reverse a xs
```

## Optional

#### Example

```bash
$ dhall --annotate <<< 'Optional'
```
```dhall
Optional : Type → Type
```

#### Type

```
──────────────────────────
Γ ⊢ Optional : Type → Type
```

### Literals: `Optional`

An `Optional` literal is either a present value wrapped in a `Some` or an
absent value using `None` followed by a type

#### Example

```bash
$ dhall --annotate <<< 'None Natural'
```
```dhall
None Natural : Optional Natural
```

```bash
$ dhall --annotate <<< 'Some 1'
```
```dhall
Some 1 : Optional Natural
```

#### Type

```
Γ ⊢ t : Type   Γ ⊢ x : t
────────────────────────
Γ ⊢ Some x : Optional t
```

```
Γ ⊢ t : Type
───────────────────────
Γ ⊢ None t : Optional t
```

### Function: `Optional/fold`

#### Example

```bash
$ dhall <<< 'Optional/fold Text (Some "ABC") Text (λ(t : Text) → t) ""'
```
```dhall
"ABC"
```

#### Type

```
─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
Γ ⊢ Optional/fold : ∀(a : Type) → Optional a → ∀(optional : Type) → ∀(just : a → optional) → ∀(nothing : optional) → optional
```

#### Rules

```dhall
Optional/fold a (None a) o j n = n

Optional/fold a (Some x) o j n = j x
```

### Function: `Optional/build`

#### Example

```bash
$ dhall <<< 'Optional/build Text (λ(optional : Type) → λ(just : Text → optional) → λ(nothing : optional) → just "abc")'
```
```dhall
Some "abc"
```

#### Type

```
────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
Γ ⊢ Optional/build : ∀(a : Type) → (∀(optional : Type) → ∀(just : a → optional) → ∀(nothing : optional) → optional) → Optional a
```

#### Rules

```dhall
Optional/build t (Optional/fold t x) = x

Optional/fold t (Optional/build t x) = x
```

## Records

### Record types

A record type is a sequence of 0 or more key-type pairs inside curly braces.

#### Examples

```bash
$ dhall --annotate <<< '{ foo : Natural, bar : Bool }'
```
```dhall
{ foo : Natural, bar : Bool } : Type
```

```bash
$ dhall --annotate <<< '{}'
```
```dhall
{} : Type
```

#### Rules

```dhall
{ k₀ : T₀, k₁ : T₁, k₂ : T₂, … } = { k₀ : T₀ } ⩓ { k₁ : T₁ } ⩓ { k₂ : T₂ } ⩓ …
```

### Record values

A record value is a sequence of 0 or more key-value pairs inside curly braces.

An empty record literal must have a single `=` sign between the curly braces to
distinguish the empty record literal from the empty record type.

#### Examples

```bash
$ dhall --annotate <<< '{ foo = 1, bar = True }'
```
```dhall
{ foo = 1, bar = True } : { foo : Natural, bar : Bool }
```

```bash
$ dhall --annotate <<< '{=}'
```
```dhall
{=} : {}
```

#### Rules

```dhall
{ k₀ = v₀, k₁ = v₁, k₂ = v₂, … } = { k₀ = v₀ } ∧ { k₁ = v₁ } ∧ { k₂ = v₂ } ∧ …
```

### Operator: `⩓`

* ASCII: `//\\`
* Unicode: U+2A53

The `⩓` operator recursively merges record types

#### Example

```bash
$ dhall <<< '{ foo : { bar : Bool } } ⩓ { foo : { baz : Text }, qux : List Natural }'
```
```dhall
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

#### Example

```bash
$ dhall <<< '{ foo = { bar = True } } ∧ { foo = { baz = "ABC" }, qux = [1, 2, 3] }'
```
```dhall
{ foo = { bar = True, baz = "ABC" }, qux = [ 1, 2, 3 ] }
```

#### Rules

```dhall
x ∧ {=} = x

{=} ∧ x = x

(x ∧ y) ∧ z = x ∧ (y ∧ z)
```

### Operator: `⫽`

* ASCII: `//`
* Unicode: U+2AFD

The `⫽` operator non-recursively merges record values, preferring fields from the right
record when they conflict

#### Example

```bash
$ dhall <<< '{ foo = 1, bar = True } ⫽ { foo = 2 }'
```
```dhall
{ foo = 2, bar = True }
```

#### Rules

```dhall
x ⫽ {=} = x

{=} ⫽ x = x

(x ⫽ y) ⫽ z = x ⫽ (y ⫽ z)
```

### Operator: `::`


The `::` operator auto-completes a record given a provided "schema" (a record containing
the expected `Type` and `default` values):

#### Example

```bash
$ dhall <<EOF
```
```dhall
let Example = { Type = { foo : Natural, bar : Bool }, default = { bar = False } }
in  Example::{ foo = 1 }
EOF
{ bar = False, foo = 1 }
```

#### Rules

```dhall
T::r = (T.default ⫽ r) : T.Type
```
