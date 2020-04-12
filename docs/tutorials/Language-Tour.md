# Language Tour

Walk through all language features

```eval_rst
.. contents:: Table of Contents
   :backlinks: none
```

This tutorial covers all of the Dhall configuration language features, in a
way that is not specific to an integration (e.g. JSON / YAML) or language binding
(e.g. Go / Rust).  In other words, this tutorial assumes that you've already
figured out use Dhall within a project and you want to dive more comprehensively
into the language.

## Prerequisites

This tutorial assumes that you have already completed the JSON/YAML tutorial:

* [Getting started: Generate JSON or YAML](./Getting-started_Generate-JSON-or-YAML.md)

This tutorial will use the command-line `dhall` tool to illustrate language
features independent of any specific integration.  You can install a prebuilt
executable for Windows / OS X / Linux from the following release page:

* [`dhall-lang/dhall-haskell` - Releases](https://github.com/dhall-lang/dhall-haskell/releases)

> **Exercise**: Verify that you have correctly installed the tool by running:
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

```
$ dhall repl
Welcome to the Dhall v1.31.1 REPL! Type :help for more information.
⊢ 2 + 2

4
```

Whenever you see an exercise prompt beginning with `⊢ `, that means to use the
REPL.

> **Exercise:** Within the REPL, type the `:help` command and try to learn one
> new command

You can also use the REPL to interpret larger expressions by saving them to a
file and then referencing the file path

> **Exercise:** Save the following Dhall code to a file named `./test.dhall`
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
> (stdin):1:1
> ```

## Introduction

All Dhall integrations support some way to load "plain" data into the desired
file format or language binding.  These "plain" values include:

* `Bool` values
* `Natural` numbers, `Integer`s, and `Double`s
* `Text` values
* `List`s
* `Optional` values
* Records
* Unions

Some languages can even load Dhall functions, meaning that they are dynamically
interpreted as functions in the host language.  However, this tutorial will not
cover that since that can only be illustrated with a specific language binding.

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
> The only change you should see is that the interpreter sorts the record
> fields.

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
of evaluating Dhall expressions, reducing them to plain data before further
conversion.

> **Exercise**: Save the above Dhall expression to a file named `example.dhall`
> and run:
>
> ```bash
> $ dhall --file example.dhall
> ```
>
> ... and verify that you get the same result as interpreting `plain.dhall`.

A Dhall interpreter processes expressions in six phases:

* Desugaring

  Some higher-level language features are "syntactic sugar" for lower-level
  language-features.  "Desugaring" is the process of translating higher-level
  features to lower-level features.

  Example: `{ x.y = 1 }` desugars to `{ x = { y = 1 } }`

* Import resolution

  This phase replaces URLs, file paths, and environment variables with the
  expressions that they refer to.

  Example: `https://prelude.dhall-lang.org/v15.0.0/Bool/not False` resolves to
  `(\(b : Bool) -> b == False) False`

* Type checking

  This phase ensures that the code is safe to evaluate by detecting and
  forbidding expressions that might lead to crashes, loops, or internal errors.

  Example: `Natural/even False` will fail to type-check

* Normalization (a.k.a. "Evaluation")

  This phase eliminates all indirection in the expression by evaluating all
  remaining programming language features that were not already covered by one
  of the preceding phases.  The result is an expression in a canonical "normal
  form".

  Example: `\(x : Natural) -> x + 0` will normalize to `\(x : Natural) -> x`

* Marshalling

  The Dhall expression is converted a file in the desired format or an
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
> [ { admin = True, age = 24, name = "Alice" }
> , { admin = True, age = 49, name = "Bob" }
> ]
> ```
>
> ```bash
> $ # `dhall-to-json` marshals the normalized expression into JSON
> $ dhall-to-json --file ./normalized.dhall 
> [
>   {
>     "admin": true,
>     "age": 24,
>     "name": "Alice"
>   },
>   {
>     "admin": true,
>     "age": 49,
>     "name": "Bob"
>   }
> ]
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
* Block comments that begin with `{-` and end with -}`

For example:

```dhall
-- This is a single-line comment

{- This is
   a block
   comment

   {- Block comments can be nested -}
-}

2 + {- Block comments can also go anywhere -} 2
```

## `Bool` values

The `Bool` type is one of the simplest types that the language provides
built-in support for.

The only two valid `Bool` constants are `False` and `True` and the language
provides the following logical operators which work on `Bool`s:

* `&&` - logical "and"

  Example: `True && False` evaluates to `False`

* `||` - logical "or"

  Example: `True || False` evaluates to `True`

* `==` - equality

  Example: `True == False` evaluates to `False`

* `!=` - inequality

  Example: `True != False` evaluates to `True`

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

> **Exercise:** What do you think is the result of interpreting the following
> expression:
>
> ```dhall
> ⊢ "${if True then "Hello" else "Goodbye"}, world!"
> ```
>
> Test your guess in the REPL!

## Numbers

`Natural` numbers are non-negative integers.  In other words:

```dhall
0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, …
```

All `Natural` number literals are unsigned.  You can also use hexadecimal
notation if you prefer:

```dhall
0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9, 0xA, 0xB, 0xC, 0xD, 0xE, …
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

Some commonly used built-in functions on numbers are:

* `Natural/isZero : Natural -> Bool`

  Returns `True` if the input is `0`, `False`, otherwise

  ```dhall
  ⊢ Natural/isZero 2

  False
  ```

* `Natural/subtract : Natural -> Natural -> Natural`

  `Natural/subtract n m` is the same as `m - n`, except clamping to `0` if the
  result is negative

  ```dhall
  ⊢ Natural/subtract 2 1

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
convert between `Natural` numbers and `Integer`s using the above built-in
functions.

`Double`s are essentially "opaque", meaning that the only thing you can do with
them is to render them as `Text`.

> **Challenge exercise:** Using the above built-in functions, implement the
> following `puzzle` function that can render an `Integer` without the leading
> sign if it is positive:
>
> ```dhall
> -- ./puzzle.dhall
>
> let puzzle = \(i : Integer) -> ???
> 
> let test0 = assert : puzzle +2 === "2"
> 
> let test1 = assert : puzzle -2 === "-2"
> 
> let test2 = assert : puzzle +0 === "0"
> 
> in  puzzle
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

## `Text`

`Text` is the most complex of the primitive types because:

* the language supports `Text` interpolation
* the language also supports multi-line `Text` literals

In the simple case a `Text` literal is surrounded by double quotes:

```dhall
"Hello, world!"
```

... and these literals permit escape sequences similar to JSON escape sequences
using backslash:

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
on both side, like this:

```dhall
⊢ "Line 1\nLine 2\nLine 3\n"

''
Line 1
Line 2
Line 3
''
```

You can think of the two single quotes on each side as "big double quotes" if
you need a convenient mnemonic for this feature.

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
                   ''
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
prefix because there are not four spaces before the closing quotes.

However, blank lines within the interior of the multi-line string literal
are ignored for the purposes of computing the shared indentation to strip.  For
example, this expression does not have leading spaces in the middle line:

```dhall
␠␠␠␠␠␠''
␠␠␠␠␠␠foo

␠␠␠␠␠␠bar
␠␠␠␠␠␠''
```

... but the indentation is still stripped, making the expression equivalent to:

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

The expression that you interpolate must have type `Text` and the language will
not automatically convert non-`Text` values to `Text`.  For example, this will
not type-check:

```dhall
let answer = 42

in  "The answer to life, the universe, and everything: ${answer}"
```

You have to instead render values as `Text` using explicit conversion functions,
like this:

```dhall
let answer = 42

in  "The answer to life, the universe, and everything: ${Natural/show answer}"
```

> **Exercise:** How can you escape `Text` interpolation?

### `Text` operations

You can concatenate `Text` literals using the `++` operator:

```dhall
⊢ "123" ++ "456"

"123456"
```

... and you can also render `Text` literals as valid Dhall source code, using:

* `Text/show : Text -> Text`

  Render a `Text` literal within a `Text` literal

  ```dhall
  ⊢ Text/show "I heard you like `Text` literals"

  "\"I heard you like `Text` literals\""
  ```

Other than that, `Text` literals are essentially opaque.  You currently cannot
parse `Text` literals nor can you compare them for equality.  This is because
the language promotes using more precise types (like enums) instead of `Text`
when the value matters.

> **Exercise:** How many backslashes do you think the result will contain?
>
> ⊢ Text/show (Text/show (Text/show (Text/show "\n")))

## `List`s

A `List` literal is surrounded by square brackets and elements are separated
by commas, like this:

```dhall
[ 2, 3, 5 ]
```

list elements must be the same type.  For example, the following expression
will not type-check:

```dhall
[ 1, True ]
```

... because `1` has type `Natural` whereas `True` has type `Bool`, which is a
type mismatch.

However, other than that restriction you can store essentially anything type
of value inside of a list so long as all elements share the same type.  For
example, we can stick the following two functions in a list because both
functions have the same type:

```dhall
[ Natural/even, Natural/odd ]
```

> **Exercise**: Ask the REPL what the type of the above list is:
>
> ```dhall
> ⊢ :type [ Natural/even, Natural/odd ]
> ```

Don't worry!  Later we'll illustrate ways to safely mix different types.

Empty lists require an explicit type annotation, like this:

```dhall
[] : List Natural
```

You can concatenate lists using the `#` operator:

```dhall
⊢ [ 1, 2 ] # [ 3, 4 ]

[ 1, 2, 3, 4 ]
```

The language also provides several built-in functions for working with `List`s,
but we'll defer covering them until further below when introduce the Prelude,
since many useful utilities on `List`s are not built-in but rather derived
from lower-level functions.

## `Optional` values

By default, all Dhall types require the corresponding value to be present,
meaning that there is no special `nil` / `null` / `None` value that you can
stick anywhere you like.

For example, if a Dhall expression has type `Bool` that means that
interpreting the expression must produce a `True` or `False` value.  No
other value is possible, and in particular an empty value is not possible.

However, you can opt-in to support for empty values by wrapping types in
`Optional`.  For example, an `Optional Natural` is a `Natural` number that
might be present or might be absent.

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

In other words, both `Some 1` and `None Natural` have the same type, which is
`Optional Natural`.  However, a naked `1` has type `Natural`, which is a
different type.  For example, if we try to stick a `1` and a `Some 1` inside of
the same list then we will get a type error:

```dhall
⊢ [ Some 1, 1 ]

Error: List elements should all have the same type

- Optional …
+ Natural

1│           1

(stdin):1:11
```


