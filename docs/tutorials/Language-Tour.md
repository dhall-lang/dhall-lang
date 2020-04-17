# Language Tour

Walk through all language features

```eval_rst
.. contents:: Table of Contents
   :backlinks: none
```

This tutorial covers all of the Dhall configuration language features, in a
way that is not specific to an integration (like JSON or YAML) or language
binding (like Go or Rust).  In other words, this tutorial assumes that you've
already figured out use Dhall within a project and you want to dive more
comprehensively into the core language features.

## Prerequisites

This tutorial assumes that you have already completed the JSON/YAML tutorial:

* [Getting started: Generate JSON or YAML](./Getting-started_Generate-JSON-or-YAML.md)

You will need to install the command-line `dhall` tool in order to follow along.
You can install a prebuilt executable for Windows / OS X / Linux from the
following release page:

* [`dhall-lang/dhall-haskell` - Releases](https://github.com/dhall-lang/dhall-haskell/releases)

> **Exercise:** Verify that you have correctly installed the tool by running:
>
> ```bash
> $ dhall <<< '2 + 2'
> ```
> ... and verifying that you get:

> ```dhall
> 4
> ```

## REPL

The `dhall` command-line tool includes a REPL, which you can use like this:

```dhall
$ dhall repl
Welcome to the Dhall v1.31.1 REPL! Type :help for more information.
⊢ 2 + 2

4
```

Whenever you see an exercise prompt beginning with `⊢ `, that means to enter
the remainder of the line as a command within the REPL.

> **Exercise:** Within the REPL, type the `:help` command and try to learn one
> new command:
>
> ```dhall
> ⊢ :help
> ```

You can also use the REPL to interpret larger expressions by saving them to a
file and then referencing the file path

> **Exercise:** Save the following Dhall code to a file named `test.dhall`
>
> ```dhall
> let x = 1
> 
> let y = 2
> 
> in  x + y
> ```
>
> ... and then interpret the file within the REPL using this command:
>
> ```dhall
> ⊢ ./test.dhall
> ```
>
> Carefully note that you need the leading `./` in the file name.  If you enter
> only `test.dhall` then the command will fail with the following error message:
>
> ```
> ⊢ test.dhall
> 
> Error: Unbound variable: test
> 
> 1│ test
> 
> (input):1:1
> ```
>
> This is because the REPL interprets Dhall code and the language provides
> built-in support for relative paths *if* they begin with `./`.  We'll cover
> this more when we get to import resolution.
>
> The REPL uses Unicode punctuation by default.  If you prefer ASCII, then
> start the REPL using `dhall repl --ascii` instead.

## Introduction

All Dhall integrations support some way to load "plain" data into the desired
file format or language binding.

These "plain" values include simple types like:

* `Bool` values
* `Natural` numbers, `Integer`s, and `Double`s
* `Text` values

... and complex types like:

* `List`s
* `Optional` values
* Records
* Unions

Here is an example of a "plain" expression that can likely be loaded into any
such integration:

```dhall
[ { name = "Alice"
  , age = 24
  , admin = True
  }
, { name = "Bob"
  , age = 49
  , admin = True
  }
]
```

For example, the above Dhall expression corresponds to the following JSON
expression:

```json
[
  {
    "name": "Alice",
    "age": 24,
    "admin": true
  },
  {
    "name": "Bob",
    "age": 49,
    "admin": true
  }
]
```

> **Exercise:** Save the above Dhall expression to a file named `plain.dhall`
> and interpret the file by running:
>
> ```bash
> $ dhall --file plain.dhall
> ```
>
> The only difference you should notice between the input and output is that the
> output sorts the record fields.

All integrations can go a step further and interpret the Dhall expression before
converting the expression into the desired format or language.  For example,
the following expression is equivalent to our original example (albeit more
indirect, just to illustrate a point):

```dhall
let List/filter = https://prelude.dhall-lang.org/List/filter

let Person = { name : Text, age : Natural, admin : Bool }

let alice : Person =
      { name = "Alice"
      , age = 24
      , admin = True
      }

let bob : Person =
      { name = "Bob"
      , age = 49
      , admin = True
      }

let carlo : Person =
      { name = "Carlo"
      , age = 20
      , admin = False
      }

let isAdmin = \(person : Person) -> person.admin

in  List/filter Person isAdmin [ alice, bob, carlo ]
```

This is because every Dhall integration includes a built-in interpreter capable
of evaluating Dhall expressions, reducing them to plain data before converting
them further to the desired host language or file format.

> **Exercise:** Save the above Dhall expression to a file named `example.dhall`
> and run:
>
> ```bash
> $ dhall --file example.dhall
> ```
>
> ... and verify that you get the same result as interpreting `plain.dhall`.

A Dhall interpreter processes expressions in five phases:

* **Desugaring**

  Some higher-level language features are "syntactic sugar" for lower-level
  language-features.  "Desugaring" is the process of translating higher-level
  features to lower-level features.

  Example: `{ x.y = 1 }` desugars to `{ x = { y = 1 } }`

* **Import resolution**

  This phase replaces URLs, file paths, and environment variables with the
  expressions that they refer to.

  Example: `https://prelude.dhall-lang.org/v15.0.0/Bool/not False` resolves to
  `(\(b : Bool) -> b == False) False`

* **Type checking**

  This phase ensures that the code is safe to evaluate by detecting and
  forbidding expressions that might lead to crashes, loops, or internal errors.

  Example: `1 + False` will fail to type-check

* **Normalization** (a.k.a. "Evaluation")

  This phase eliminates all indirection in the expression by evaluating all
  remaining programming language features that were not already covered by one
  of the preceding phases.  The result is an expression in a canonical "normal
  form".

  Example: `\(x : Natural) -> [ 2 + 2, x ]` will normalize to
  `\(x : Natural) -> [ 4, x ]`

* **Marshalling**

  The Dhall expression is converted into a file in the desired format or an
  expression within the desired language.

  Example (converting to Bash):

  ```bash
  $ dhall-to-bash --declare FOO <<< '{ x = 1, y = 2 }'
  declare -r -A FOO=([x]=1 [y]=2)
  ```

Integrations will typically perform all of these steps in one go, but you can
use the `dhall` command-line tool to separate out some of these steps.

> **Exercise:** Using the same `./example.dhall` file as before, run the
> following commands:
>
> ```bash
> $ # `dhall resolve` performs only import resolution
> $ dhall resolve --file ./example.dhall | tee ./resolved.dhall
> ```
>
> ```bash
> $ # `dhall type` type-checks the expression and displays the inferred type
> $ dhall type --file ./resolved.dhall
> ```
>
> ```bash
> $ # `dhall normalize` normalizes the resolved expression
> $ dhall normalize --file ./resolved.dhall | tee ./normalized.dhall
> ```
>
> ```bash
> $ # `dhall-to-json` marshals the normalized expression into JSON
> $ dhall-to-json --file ./normalized.dhall 
> ```
> 
> ... and study how the Dhall expression evolves between phases.

Note that some of these commands can actually perform all of the preceding
steps.  For example, `dhall-to-json` could have taken the original
`./example.dhall` as input and produced the same result.  However, splitting
things into explicit phases can sometimes help better understand how the
interpreter processes the code.

> **Exercise:** There are two URL imports in the following Dhall expression:
>
> ```dhall
> if True
> then https://prelude.dhall-lang.org/Bool/not True
> else https://prelude.dhall-lang.org/Natural/sum [ 2, 3, 5 ]
> ```
>
> Which imports are resolved by the interpreter?
>
> Does the expression type-check?

## Comments

You can add comments to Dhall expressions which are ignored by the
interpreter.  These comments come in two forms:

* Single-line comments that begin with `--`
* Block comments that begin with `{-` and end with `-}`

For example:

```dhall
-- This is a single-line comment

{- This is
   a block
   comment

   {- You can nested block comments -}
-}

2 + {- You can embed block comments anywhere -} 2
```

Comments have no effect on how the code is interpreted.  They are purely for
the benefit of people reading the code.

## `Bool` values

The `Bool` type is one of the simplest types that the language provides
built-in support for.

The only two valid `Bool` constants are `False` and `True` and the language
provides the following logical operators which work on `Bool`s:

* `&&` - logical "and"

  ```dhall
  ⊢ True && False

  False
  ```

* `||` - logical "or"

  ```dhall
  ⊢ True || False

  True
  ```

* `==` - equality

  ```dhall
  ⊢ True == False

  False
  ```

* `!=` - inequality

  ```dhall
  ⊢ True != False

  True
  ```

Carefully note that the `==` and `!=` operators only work on values of type
`Bool`.  This is one important way that Dhall differs from many other languages.
For example, you cannot compare `Text` or `Natural` numbers for equality or
inequality using these operators.

> **Exercise:** Try to compare two numbers for equality and see what happens:
>
> ```dhall
> ⊢ 1 == 1
> ```
>
> Which interpreter phase do you think rejected the expression?

Additionally the language provides built-in support for `if` expressions.

```dhall
⊢ if True then "Hello" else "Goodbye"

"Hello"
```

## Numbers

`Natural` numbers are non-negative integers.  In other words:

```dhall
0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, …
```

All `Natural` number literals are unsigned.  You can also use hexadecimal
notation if you prefer:

```dhall
0x0, 0x1, 0x2, …, 0xE, 0xF, 0x10, 0x11, …
```

`Natural` numbers are the most commonly used numeric type for programming
utilities, since many useful functions use `Natural` numbers to forbid negative
inputs or output.

For example, the type of the built-in `List/length` function guarantees that the
function can never return a negative length:

```dhall
List/length : forall (a : Type) -> List a -> Natural
```

The `Natural` number type is also a good default choice for many configuration
options where negative values are not sensible, like:

* A person's age
* The number of CPUs to provision for a machine
* The maximum number of permitted retries for a failing service

`Integer`s are a different numeric type and they are not the same as `Natural`
numbers.  All `Integer` literals require an explicit sign:

```dhall
…, -7, -6, -5, -4, -3, -2, -1, +0, +1, +2, +3, +4, +5, +6, +7, …
```

`Integer`s also permit hexadecimal notation, like `Natural` numbers.

Both `Natural` numbers and `Integer`s are unlimited precision, which means that
there is no maximum value for either type.

`Double`s represent IEEE 754 double-precision floating point numbers, such as:

```dhall
-1.0, 3.14159265359, 6.0221409e+23, 1e6
```

> **Exercise:** What do you think will happen if you input a `Double` literal
> that is out of the valid range for a double-precision floating point number?
>
> ```dhall
> ⊢ '1e10000'
> ```
>
> Run the above command to find out!

`Double` literals require an explicit decimal point or an exponent (if using
scientific notation).  The `Double` type is also a distinct numeric type from
`Natural` numbers and `Integer`s.

The language also provides the following arithmetic operators which only work on
`Natural` numbers:

* `+` - addition

  ```dhall
  ⊢ 2 + 3

  5
  ```

* `*` - multiplication

  ```dhall
  ⊢ 2 * 3

  6
  ```

These operators do not work on `Integer`s or `Double` values, although you can
convert between `Natural` numbers and `Integer`s using built-in functions that
we will cover later in this tutorial.

On the other hand, `Double`s are essentially "opaque", meaning that you cannot
perform any arithmetic with them.

## `Text`

`Text` is the most complex of the primitive types because:

* the language supports `Text` interpolation
* the language also supports multi-line `Text` literals

In the simple case a `Text` literal is surrounded by double quotes:

```dhall
"Hello, world!"
```

... and these literals permit escape sequences similar to JSON using a
backslash:

```dhall
"Line 1\nLine 2\n"
```

The full set of escape sequences are:

* `\"` - Escape a quotation mark
* `\\` - Escape a backslash
* `\/` - Escape a forward slash
* `\b` - Escape a backspace
* `\f` - Escape a form feed
* `\n` - Escape a line feed
* `\r` - Escape a carriage return
* `\t` - Escape a tab
* `\$` - Escape a dollar sign
* `\uXXXX` / `\u{XXXX}` - Escape a Unicode sequence (specified using hex)

The language also permits Unicode characters in `Text` literals, too.

> **Exercise:** Run the following command to test Dhall's support for special
> characters:
>
> ```dhall
> ⊢ "🍋\t🍓\t🍍\t🍉\t🍌\n\u{1F60B} \"Yum!\"\n"
> ```

### Multi-line `Text` literals

Dhall supports multi-line `Text` literals surrounded by two single quotes
on each side, like this:

```dhall
⊢ "Line 1\nLine 2\nLine 3\n"

''
Line 1
Line 2
Line 3
''
```

You can think of two single quotes as "big double quotes" if you need a
convenient mnemonic for this feature.

Multi-line `Text` literals automatically strip leading indentation for all
lines after the opening quotes, meaning that this expression

```dhall
''
ABC
DEF
'''
```

... is the same as this expression

```dhall
    ''
    ABC
    DEF
    ''
```

... which is also the same as this expression:

```dhall
{- Leading indentation here has no effect and is not part of the literal -}  ''
    ABC
    DEF
    ''
```

... all three of which are syntactic sugar for the following plain `Text`
literal:

```dhall
"ABC\nDEF\n"
```

However, the preceding multi-line literals are **NOT** the same as this
expression:

```dhall
''
    ABC
    DEF
''
```

... which desugars to:

```dhall
"    ABC\n    DEF\n"
```

This latter multi-line string literal does not strip the leading four-space
prefix because the final line before the closing quotes does not share the
same four-space prefix.

However, other blank lines within the interior of the multi-line string literal
are ignored for the purposes of computing the shared indentation to strip.  For
example, this expression does not have leading spaces in the middle line:

```dhall
␠␠␠␠␠␠''
␠␠␠␠␠␠foo

␠␠␠␠␠␠bar
␠␠␠␠␠␠''
```

... yet the indentation is still stripped, making the expression equivalent to:

```dhall
''
foo

bar
''
```

Both leading tabs and leading spaces are stripped in this way, so long as the
pattern of tabs and spaces match for each line that isn't blank.

All multi-line string literals begin with an obligatory newline character (which
is not included in the final string).  For example, this is not valid:

```dhall
''ABC''
```

... but this is valid:

```dhall
''
ABC''
```

... which desugars to:

```dhall
"ABC"
```

> **Exercise:** What does this multi-line `Text` literal desugar to?
>
> ```dhall
>       ''
>     ABC
>   DEF
> ''
> ```

### `Text` interpolation

You can also interpolate Dhall expressions into `Text` using `${…}`, like this:

```dhall
let greeting = "Hello"

in  "${greeting}, world!"
```

The expression that you interpolate must have type `Text`.  Also, the language
will not automatically convert non-`Text` values to `Text`.  For example, this
will not type-check:

```dhall
let answer = 42

in  "The answer to life, the universe, and everything: ${answer}"
```

You have to instead render values as `Text` using explicit conversion functions.
For example, you can use the `Natural/show` built-in function to convert a
`Natural` number to `Text`:

```dhall
let answer = 42

in  "The answer to life, the universe, and everything: ${Natural/show answer}"
```

> **Exercise:** How do you escape `Text` interpolation?
>
> In other words, how do you get the interpreter to not interpolate into a
> `Text` literal with the characters `${…}`?

### `Text` operations

You can concatenate `Text` literals using the `++` operator:

```dhall
⊢ "123" ++ "456"

"123456"
```

Other than that, `Text` literals are essentially opaque.  You currently cannot
parse `Text` literals nor can you compare them for equality.  This is because
the language promotes using more precise types (like enums) instead of `Text`
when the value matters.

## Types

Before introducing complex types we will take a detour to introduce Dhall's
support for types and type annotations.

Every Dhall expression has a type and whenever you see `x : T` that means that
the expression `x` has type `T`.  For example:

```dhall
True : Bool        -- The expression `True` has type `Bool`

(2 + 2) : Natural  -- The expression `(2 + 2)` has type `Natural`
```

The `:` symbol is the type annotation operator.  This operator takes two
arguments:

* The left-hand side is the expression to type-check
* The right-hand side is the expected type of the left-hand side

... and the operator returns the left-hand side after type-checking it against
the right-hand side.

> **Exercise:** Ask the REPL for the type of the `Natural/even` function:
>
> ```dhall
> ⊢ :type Natural/even
> ```
>
> Then verify that the type is correct by giving a type annotation to the
> `Natural/even` function.

You can insert this operator anywhere within Dhall code (although you may
need to wrap things in parentheses).  For example, this is a valid Dhall
expression:

```dhall
> ⊢ (2 : Natural) + (2 : Natural)

4
```

Types are expressions, too, which means that types can themselves have type
annotations:

```dhall
Bool : Type  -- The expression `Bool` has type `Type`

Type : Kind  -- The expression `Type` has type `Kind`

Kind : Sort  -- The expression `Kind` has type `Sort`
```

You can chain the type annotation operator, so we could also write:

```dhall
True : Bool : Type : Kind : Sort
```

... the hierarchy of types stops at `Sort`.  There is nothing above that.

Dhall has standard terminology for referring to expressions at different
"levels" of the type hierarchy:

* An expression `x` is a lowercase-'t' "term" if the type of the type of `x` is
  `Type`

  For example, `1` is a "term" because `1 : Natural : Type`

  `Natural/show` is also a "term" because
  `Natural/show : Natural -> Text : Type`

* An expression `x` is a lowercase-'t' "type" if the type of the type of `x` is
  `Kind`

  For example, `Bool` is a "type" because `Bool : Type : Kind`

  Also, `List` is a "type" because `List : Type -> Type : Kind`

* An expression `x` is a lowercase-'k' "kind" if the type of the type of `x` is
  `Sort`

  For example, `Type` is a "kind" because `Type : Kind : Sort`

> **Exercise:** Classify the following expressions as either "terms", "types",
> or "kinds":
>
> * `2 + 2`
>
> * `List Natural`
>
> * `{ x = True, y = "ABC" }`
>
> * `Type -> Type`

If you don't feel like classifying things, you can always call something an
"expression".  All terms, types, and kinds are expressions, too.

## `List`s

A `List` literal is comma-separated elements surrounded by square brackets, like
this:

```dhall
[ 2, 3, 5 ]
```

... and the type of a list is `List T` where `T` is the type of each element.
For example, the type of the above `List` is:

```dhall
⊢ :type [ 2, 3, 5 ]

List Natural
```

This implies that `List` elements must share the same type.  For example, the
following expression will not type-check:

```dhall
[ 1, True ]
```

... because `1` has type `Natural` whereas `True` has type `Bool`, and those two
types do not match.

Don't worry!  Later we'll illustrate ways to sensibly mix different types within
the same `List`.

However, other than that restriction you can store essentially any type of value
inside of a list so long as all elements share the same type.  For example, we
can stick the following two functions in a list because both functions have the
same type:

```dhall
[ Natural/even, Natural/odd ]
```

> **Exercise:** Ask the REPL what the type of the above list is:
>
> ```dhall
> ⊢ :type [ Natural/even, Natural/odd ]
> ```

Empty lists require an explicit type annotation, like this:

```dhall
[] : List Natural
```

Also, you can concatenate lists using the `#` operator:

```dhall
⊢ [ 1, 2 ] # [ 3, 4 ]

[ 1, 2, 3, 4 ]
```

> **Exercise:** Concatenate two empty lists

## `Optional` values

By default, all Dhall types are not "nullable", meaning that there is no special
`nil` / `null` / `None` value that suffices for those types.

For example, if a Dhall expression has type `Bool` that means that
interpreting the expression must produce a `True` or `False` value.  No
other result is possible, and in particular a null value is not possible.

However, you can opt in to empty values by wrapping types in `Optional`.  For
example, an `Optional Natural` is a `Natural` number that might be present or
might be absent.

There are two ways to create a value of type `Optional Natural`.  If the
`Natural` number is present then you wrap the `Natural` number in a `Some` to
get an `Optional Natural` number:

```dhall
⊢ :type Some 1

Optional Natural
```

If the `Natural` number is absent, then you can provide an empty placeholder
by specifying `None Natural`:

```dhall
⊢ :type None Natural

Optional Natural
```

In other words, both `Some 1` and `None Natural` share the same type, which is
`Optional Natural`, so we could store both inside the same `List`:

```dhall
⊢ :type [ Some 1, None Natural ]

List (Optional Natural)
```

However, a "bare" `1` has type `Natural`, which is a different type.  For
example, if we try to store a `1` and a `Some 1` inside of the same list then we
will get a type error:

```dhall
⊢ [ Some 1, 1 ]

Error: List elements should all have the same type

- Optional …
+ Natural

1│           1

(input):1:11
```

More generally, the type of an optional value is `Optional T` where `T` is the
type of the element that might be present.

You can make arbitrary expressions `Optional`, such as the following nested
`Optional` types that are valid:

* `Optional (List Text)`

  An `Optional` `List` of `Text` values.  For example:


  ```dhall
  Some [ "ABC", "DEF" ]  -- A present list that is non-empty

  Some ([] : List Text)  -- A present list that is empty

  None (List Text)       -- An absent list
  ```

* `Optional (Optional Bool)`

  All of the following values share this type:

  ```dhall
  Some (Some True)

  Some (None Bool)

  None (Optional Bool)
  ```

  Carefully note that the last two values do not mean the same thing.  Many
  languages only permit only one "level" of nullability, but in Dhall you can
  nest the `Optional` type to an arbitrary depth and each layer is distinct.

`Some` is a keyword that requires an argument, meaning that `Some 1` is a valid
expression, but `Some` by itself is not valid.  However, `None` is more
flexible, because `None` is an ordinary function that is valid in isolation.

> **Exercise:** What is the type of `None`?
>
> ```dhall
> ⊢ :type None
> ```
>
> What happens if you try to enter `Some` by itself?
>
> ```dhall
> ⊢ :type Some
> ```

## Records

A record literal is comma-separated key-value pairs surrounded by curly braces:

```dhall
{ name = "John Doe", age = 24 }
```

The above record literal has two fields: a field called `name` whose value is
`"John Doe"` and a field called `age` whose value is `24`.

A record type is comma-separated key-type pairs surrounded by curly braces:

```dhall
{ name : Text, age : Natural }
```

The above record type is the type of the previous record literal:

```dhall
{ name = "John Doe", age = 24 } : { name : Text, age : Natural }
```

... and you can read the type as saying that the field called `name` stores
a value of type `Text` and the field called `age` stores a value of type
`Natural`.

The way to denote an empty record literal with zero key-value pairs is:

```dhall
{=}
```

... and the way to denote an empty record type with zero key-type pairs is:

```dhall
{}
```

> **Exercise:** What is the type of this expression?
>
> ```dhall
> [ { name = "John" , manager = Some "Alice" }
> , { name = "Alice", manager = None Text    }
> ]
> ```

You can access the fields of a record using `.` (the field access operator),
like this:

```dhall
let exampleRecord = { x = 1, y = 2 }

in  exampleRecord.x + exampleRecord.y
```

## Functions

All function types are of the form `A -> B` where `A` is the type of the
function's input and `B` is the type of the function's output.  You can also
use the Unicode right arrow `→` (U+2192) to represent a function type as
`A → B`.

For example, `Natural/even : Natural -> Text` means that the `Natural/even`
function converts an input of type `Natural` to an output of type `Text`:

```dhall
⊢ :type Natural/even

Natural → Bool

⊢ :type 2

Natural

⊢ :type Natural/even 2

Bool
```

Functions types can name their input arguments using the `forall` or `∀`
keyword.  For example, the following function:

```dhall
⊢ :let example = \(x : Natural) -> x + 1
```

... has this inferred type:

```dhall
example : forall (x : Natural) -> Natural
```

... which says that `example` is a function whose input is an argument named
`x` whose type is `Natural` and the output type is also `Natural`.

The equivalent Unicode type would be:

```dhall
∀(x : Natural) → Natural
```

... where `∀` (U+2200) is the Unicode equivalent of `forall`.

Sometimes the argument name is optional, like in the above function type,
meaning that the name (e.g. `x`) is purely informative and can be changed or
omitted without affecting the type.  For example, all of the following types are
the same type as far as the type-checker is concerned:

```dhall
forall (x : Natural) -> Natural

forall (y : Natural) -> Natural

Natural -> Natural
```

Omitting the argument name is the same thing as naming the argument `_`.
In other words, `Natural -> Natural` is syntactic sugar for
`forall (_ : Natural) -> Natural`.

You can apply a function to an argument by simply separating the function and
the argument by whitespace, like this:

```dhall
Natural/even 2
```

You don't need to parenthesize function arguments (similar to function calls
in Haskell or Bash).

The simplest way to create a function is to introduce an anonymous function
using the following syntax:

```dhall
\(input : InputType) -> output
```

... or if you prefer Unicode you can use :

```dhall
λ(input : InputType) → output
```

... where `λ` (U+03BB) is the Unicode equivalent of `\`.

For example, the following function takes an input named `x` of type `Natural`
and returns the next `Natural` number (`x + 1`):

```dhall
\(x : Natural) -> x + 1
```

> **Exercise**: Apply the above anonymous function directly to the argument `2`

In practice, most anonymous Dhall functions are given a name using a `let`
binding, like this:

```dhall
let increment = \(x : Natural) -> x + 1

in  increment 2
```

> **Exercise:** Write a function that negates a `Bool` value:
>
> ```dhall
> let not = ???
>
> in  not True  -- … which should return `False`
> ```

In some cases, the argument name matters if you reference the name within the
rest of the type.  For example, the type of the `List/length` function is:

```dhall
List/length : forall (a : Type) -> List a -> Natural
```

The first argument (named `a`) is referenced within the type of the second
argument (`List a`), so the name `a` cannot be omitted from the type.  However,
we can still rename `a` so long as we also rename other occurrences within the
same type.  For example, the following types are the same type as far as the
type-checker is concerned:

```dhall
forall (a : Type) -> List a -> Natural

forall (b : Type) -> List b -> Natural

forall (elementType : Type) -> List elementType -> Natural
```

A function type is "polymorphic" if the type of one argument depends on the name
of another argument in this way.  For example, the `List/length` function has a
polymorphic type:

```dhall
List/length : forall (a : Type) -> List a -> Natural
```

> **Exercise:** Ask the REPL for the type of the `List/length` function after
> applying the function to one argument:
>
> ```dhall
> ⊢ :type List/length Natural
> ```
>
> ... and then apply the `List/length function` to one more argument (a `List`)
> to compute the length of that `List`.

> **Challenge exercise:** Actually, you can omit the `forall` for the type of
> `List/length`.  Devise an equivalent type for `List/length` that does not
> use a `forall` or `∀` and check your answer in the REPL by giving
> `List/length` that type as an annotation:
>
> ```dhall
> ⊢ List/length : ???
> ```

## Built-in functions

The Dhall language also has a few built-in functions for processing built-in
types.

For example, some built-in functions on numbers are:

* `Natural/isZero : Natural -> Bool`

  Returns `True` if the input is `0`, `False`, otherwise

  ```dhall
  ⊢ Natural/isZero 2

  False
  ```

* `Natural/toInteger : Natural -> Integer`

  Convert a `Natural` number to the corresponding `Integer`

  ```dhall
  ⊢ Natural/toInteger 2

  +2
  ```

* `Natural/show : Natural -> Text`

  Render a `Natural` number as `Text`

  ```dhall
  ⊢ Natural/show 2

  "2"
  ```

* `Integer/clamp : Integer -> Natural`

  Convert an `Integer` to a `Natural` number, clamping negative `Integer`s to
  `0`

  ```dhall
  ⊢ Integer/clamp +3

  3
  ```

* `Integer/negate : Integer -> Integer`

  Negate an `Integer`

  ```dhall
  ⊢ Integer/negate +3

  -3
  ```

* `Integer/toDouble : Integer -> Double`

  Convert an `Integer` to the corresponding `Double`

  ```dhall
  ⊢ Integer/toDouble +3

  3.0
  ```

* `Integer/show : Integer -> Text`

  Render an `Integer` as `Text` (including the obligatory sign)

  ```dhall
  ⊢ Integer/show +3

  "+3"
  ```

* `Double/show : Double -> Text`

  Render a `Double` as `Text`

  ```dhall
  ⊢ Double/show 3.0

  "3.0"
  ```

> **Challenge exercise:** Using the above built-in functions, implement the
> following `Integer/showWithoutPlus` function that can render an `Integer`
> without the leading sign if it is positive:
>
> ```dhall
> -- ./puzzle.dhall
>
> let showWithoutPlus = \(i : Integer) -> ???
> 
> let test0 = assert : showWithoutPlus +2 === "2"
> 
> let test1 = assert : showWithoutPlus -2 === "-2"
> 
> let test2 = assert : showWithoutPlus +0 === "0"
> 
> in  showWithoutPlus
> ```
>
> You can test if you got the right answer by type-checking the file:
>
> ```bash
> $ dhall type --quiet --file puzzle.dhall
> ```
>
> ... which will run the acceptance tests at the bottom of the file.
>
> Later on we'll see how we can simplify functions like these with shared
> utilities from the Dhall Prelude.

This tutorial does not cover all available built-in functions.  If you are
interested in the full list, see:

* [Built-in types, functions, and operators](../references/Built-in-types.md)

## Tests

The language provides built-in support for testing that two expressions are
equal using the `assert` keyword, like this:

```dhall
⊢ assert : (2 + 2) === 4

assert : 4 ≡ 4
```

This keyword lets you compare two expressions for equality at type-checking
time using the `===` or `≡` (U+2261) operator.

If the expressions do not match then type-checking will fail and the
interpreter will display a diff:

```dhall
⊢ assert : (2 + 2) === 5

Error: Assertion failed

- 4
+ 5

1│ assert : (2 + 2) === 5

(input):1:1
```

You don't have to limit yourself to comparing "plain" expressions.  You can
compare functions or abstract expressions for equality in this way, too:

```dhall
⊢ \(x : Natural) -> assert : List/length Natural [ x, x ] === 2

λ(x : Natural) → assert : 2 ≡ 2

⊢ \(x : Bool) -> assert : (x && True) === x

λ(x : Bool) → assert : x ≡ x
```

The interpreter cannot always verify that two abstract expressions are the same
(this is impossible in general), but the interpreter can detect some simple
equalities.  You can use this feature to author "property tests", except
verifying that the property holds for all possible values instead of a randomly
selected sample of values.

## Unions

Previously, we noted that we could not store elements of different types within
the same `List`:

```dhall
[ 1, True ]  -- This will not type-check
```

However, we can wrap elements of different types so that they agree upon a
shared composite type.  These composite types are called "unions".

A union type is bar-delimited key-type pairs surrounded by angle brackets, like
this:

```dhall
< Number : Natural | Boolean : Bool >
```

Each key is called an "alternative" and the above union type has two
alternatives named `Number` and `Boolean`.  The alternative names can be
whatever you want them to be (hopefully descriptive names!).

Each alternative may optionally be paired with a type of value that you can
store within that alternative.  The above union type can store a `Natural`
number within the `Number` alternative or a `Bool` value within the `Boolean`
alternative.

To wrap a value in a union type, use the following syntax:

```dhall
UnionType.Alternative valueToWrap
```

The result will be a value whose type is the union's type.  For example:

```dhall
⊢ :let Example = < Number : Natural | Boolean : Bool >

Example : Type

⊢ :type Example.Number 1

< Boolean : Bool | Number : Natural >

⊢ :type Example.Boolean True

< Boolean : Bool | Number : Natural >
```

Since the types match, we can store the wrapped values within the same `List`:

```dhall
let Example = < Number : Natural | Boolean : Bool >

in  [ Example.Number 1, Example.Boolean True ]
```

> **Exercise:** `Example.Number` and `Example.Boolean` are functions.  Use the
> REPL to ask for the type of each function:
>
> ```dhall
> ⊢ :let Example = < Number : Natural | Boolean : Bool >
> ⊢ :type Example.Number
> ⊢ :type Example.Boolean
> ```

> **Exercise:** Add another alternative to the `Example` type and then add
> a value wrapped in that alternative to the above `List`.

You can extract a value from a union type using the `merge` keyword.  This
keyword expects a record containing one function per alternative, like this:

```dhall
let Example = < Number : Natural | Boolean : Bool >

let renderExample
    : Example -> Text
    =     \(example : Example)
      ->  merge
            { Number = \(n : Natural) -> Natural/show n
            , Boolean = \(b : Bool) -> if b then "True" else "False"
            }
            example

let example0 = assert : renderExample (Example.Number 42) === "42"

let example1 = assert : renderExample (Example.Boolean False) === "False"

in  renderExample
```

The functions stored within this record are called "handlers" because each of
them "handles" one potential alternative.  We don't know in advance which
alternative might be stored within our union type, so we need to be prepared to
handle all of them.

The language does not let you ignore alternatives.  If you forget to provide a
handler, then that is a type error.

> **Exercise:** Delete the `Boolean` handler from the above example and
> interpret the expression to see what happens.

Each handler is a function whose input is the value wrapped within that
alternative and whose output is a result of any type, so long as each handler
shares the same result type.  In our `renderExample` function each
handler has a different input type, but they all share the same output type:
`Text`.

> **Exercise:** Implement a function that converts an `Example` to a `Natural`
> number with the following behavior:
>
> * If the alternative is a `Number`, return the wrapped number
> * If the alternative is a `Boolean`, then return `0` if `False` and `1` if
>   `True`
>
> Check your answer by writing tests for your function using `assert`.

Alternatives can be empty.  For example, you can define an "enum" as a union
with all empty alternatives:

```dhall
let DayOfWeek =
      < Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday >

let isWeekend
    : DayOfWeek -> Bool
    =     \(day : DayOfWeek)
      ->  merge
            { Sunday = True
            , Monday = False
            , Tuesday = False
            , Wednesday = False
            , Thursday = False
            , Friday = False
            , Saturday = False
            }
            day

in  isWeekend
```

A handler for an empty alternative requires no input (since an empty alternative
does not store a value).

You can also mix empty and non-empty alternatives.  For example, you could
define the `Optional` type like this:

```dhall
let Optional = \(a : Type) -> < Some : a | None >
```

... although that's not how the type actually works (it's built into the
language).  Even so, you can still use the `merge` keyword to process `Optional`
values *as if* they had the above type:

```dhall
let default =
          \(o : Optional Natural)
      ->  merge { Some = \(n : Natural) -> n, None = 0 } o

let example0 = assert : default (Some 42) === 42

let example1 = assert : default (None Natural) === 0

in  default
```

## Multiple function arguments

All Dhall functions are functions of one argument, and there are two ways you
can "simulate" a function of multiple arguments:

* **Currying**

  You can have a function return another function.  For example:

  ```dhall
  ⊢ :let example = \(x : Bool) -> \(y : Bool) -> [ x, y ]

  example : ∀(x : Bool) → ∀(y : Bool) → List Bool

  ⊢ example True

  λ(y : Bool) → [ True, y ]

  ⊢ example True False

  [ True, False ]

  ⊢ :let intermediate = example True

  intermediate : ∀(y : Bool) → List Bool

  ⊢ intermediate False

  [ True, False ]
  ```

  The advantage of "currying" is that you can "partially apply" a "curried"
  function to one argument at a time.

* **Records**

  You can create a function that expects a record containing multiple fields as
  the function's input.  For example:

  ```dhall
  ⊢ :let example = \(args : { x : Bool, y : Bool }) -> [ args.x, args.y ]

  example : ∀(args : { x : Bool, y : Bool }) → List Bool

  ⊢ example { x = True, y = False }

  [ True, False ]
  ```

  The advantage of input records is that using fields to name function arguments
  can sometimes improve code comprehension, especially for functions with a
  large number of arguments.

Neither approach is strictly better than the other, but you will typically see
the following convention in the Dhall ecosystem:

* Currying is more commonly used for simple and highly reusable utilities with
  a few arguments

  Think: "library code"

  For example, currying is used pervasively in built-in functions and the
  Prelude

* Records are used more commonly for complex and special-purpose utilities with
  a large number of arguments

  Think: "application code"

We've already seen one example of a "curried" function, which is
`List/length`:

```dhall
List/length : forall (a : Type) -> List a -> Natural
```

We can make the currying more explicit by adding the following parentheses to
the type:

```dhall
List/length : forall (a : Type) -> (List a -> Natural)
```

This type indicates that `List/length` is a function that takes one argument
(a `Type`), and returns a new intermediate function.  This intermediate function
takes an argument of its own (a `List a`) and returns the final result (a
`Natural`).  Or in other words, `List/length` is a "function that returns a
function that returns a number".

## Naming conventions

* Talk about what things are conventionally uppercase and lowercase
