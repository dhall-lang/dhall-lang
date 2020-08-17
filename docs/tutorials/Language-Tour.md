# Language Tour

Walk through all language features

```eval_rst
.. contents:: Table of Contents
   :backlinks: none
```

This tutorial covers all of the Dhall configuration language features, in a
way that is not specific to an integration (like JSON or YAML) or language
binding (like Go or Rust).  In other words, this tutorial assumes that you've
already figured out how to use Dhall within a project and you want to dive more
comprehensively into the core language features.

## Prerequisites

This tutorial assumes that you have already completed the JSON/YAML tutorial:

* [Getting started: Generate JSON or YAML](./Getting-started_Generate-JSON-or-YAML.md)

You will need to install the command-line `dhall` tool in order to follow along.
You can install a prebuilt executable for Windows / OS X / Linux from the
following release page:

* [`dhall-lang/dhall-haskell` - Releases](https://github.com/dhall-lang/dhall-haskell/releases)

The installation instructions are essentially the same as in
[Getting started: Generate JSON or YAML](./Getting-started_Generate-JSON-or-YAML.md),
except replacing `dhall-json` with `dhall`.

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
> <details>
> <summary>Output</summary>
>
> ```dhall
> 3
> ```
>
> </details>
> <br/>
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
> Alternatively, you can also interpret the file from the command line, like
> this:
>
> ```bash
> $ dhall --file test.dhall
> ```
>
> This tutorial will not cover everything that the command-line utility can do,
> but as a general rule anything you can do within the REPL you can also be
> done from the command line without the REPL.
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

Here is an example of a "plain" expression that can be loaded into most
languages or file formats:

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
>
> <details>
> <summary>Output</summary>
>
> ```dhall
> [ { admin = True, age = 24, name = "Alice" }
> , { admin = True, age = 49, name = "Bob" }
> ]
> ```
>
> </details>

All integrations can go a step further and interpret the Dhall expression before
converting the expression into the desired format or language.  For example,
the following expression is equivalent to our original example (albeit more
indirect, just to illustrate a point):

```dhall
-- ./example.dhall

let List/filter = https://prelude.dhall-lang.org/v15.0.0/List/filter

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
>
> <details>
> <summary>Output</summary>
>
> ```dhall
> [ { admin = True, age = 24, name = "Alice" }
> , { admin = True, age = 49, name = "Bob" }
> ]
> ```
>
> </details>

A Dhall interpreter processes expressions in five phases:

* **Desugaring**

  Some higher-level language features are "syntactic sugar" for lower-level
  language-features.  "Desugaring" is the process of translating higher-level
  features to lower-level features.

  Example: `{ x.y = 1 }` desugars to `{ x = { y = 1 } }`

  <br/>

* **Import resolution**

  This phase replaces URLs, file paths, and environment variables with the
  expressions that they refer to.

  Example: `https://prelude.dhall-lang.org/v15.0.0/Bool/not False` resolves to
  `(\(b : Bool) -> b == False) False`

  <br/>

* **Type checking**

  This phase ensures that the code is safe to evaluate by detecting and
  forbidding expressions that might lead to crashes, loops, or internal errors.

  Example: `1 + False` will fail to type-check

  <br/>

* **Normalization** (a.k.a. "Evaluation")

  This phase eliminates all indirection in the expression by evaluating all
  remaining programming language features that were not already covered by one
  of the preceding phases.  The result is an expression in a canonical "normal
  form".

  Example: `\(x : Natural) -> [ 2 + 2, x ]` will normalize to
  `\(x : Natural) -> [ 4, x ]`

  Evaluation is a special-case of normalization where the result is a plain
  value.

  Example: `if True && False then 1 else 0` evaluates to `0`

  <br/>

* **Marshalling**

  The Dhall expression is converted into a file in the desired format or an
  expression within the desired language.

  Example (converting to Bash):

  ```bash
  $ dhall-to-bash --declare FOO <<< '{ x = 1, y = 2 }'
  declare -r -A FOO=([x]=1 [y]=2)
  ```

  <br/>

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
> <details>
> <summary>Output</summary>
>
> ```dhall
> let List/filter =
>       λ(a : Type) →
>       λ(f : a → Bool) →
>       λ(xs : List a) →
>         List/fold
>           a
>           xs
>           (List a)
>           (λ(x : a) → λ(xs : List a) → if f x then [ x ] # xs else xs)
>           ([] : List a)
>
> let Person = { name : Text, age : Natural, admin : Bool }
>
> let alice
>     : Person
>     = { name = "Alice", age = 24, admin = True }
>
> let bob
>     : Person
>     = { name = "Bob", age = 49, admin = True }
>
> let carlo
>     : Person
>     = { name = "Carlo", age = 20, admin = False }
>
> let isAdmin = λ(person : Person) → person.admin
>
> in  List/filter Person isAdmin [ alice, bob, carlo ]
> ```
>
> </details>
> <br/>
>
> ```bash
> $ # `dhall type` type-checks the expression and displays the inferred type
> $ dhall type --file ./resolved.dhall
> ```
>
> <details>
> <summary>Output</summary>
>
> ```dhall
> List { admin : Bool, age : Natural, name : Text }
> ```
>
> </details>
> <br/>
>
> ```bash
> $ # `dhall normalize` normalizes the resolved expression
> $ dhall normalize --file ./resolved.dhall | tee ./normalized.dhall
> ```
>
> <details>
> <summary>Output</summary>
>
> ```dhall
> [ { admin = True, age = 24, name = "Alice" }
> , { admin = True, age = 49, name = "Bob" }
> ]
> ```
>
> </details>
> <br/>
>
> ```bash
> $ # `dhall-to-json` marshals the normalized expression into JSON
> $ dhall-to-json --file ./normalized.dhall 
> ```
>
> <details>
> <summary>Output</summary>
>
> ```json
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
> </details>
> <br/>
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
> then https://prelude.dhall-lang.org/v15.0.0/Bool/not True
> else https://prelude.dhall-lang.org/v15.0.0/Natural/sum [ 2, 3, 5 ]
> ```
>
> Which imports are resolved by the interpreter?
>
> Does the expression type-check?
>
> <details>
> <summary>Solution</summary>
>
> Both imports are always resolved, regardless of which branch of the `if`
> expression is returned because import resolution strictly precedes
> normalization.
>
> The expression does not type-check because the two branches of the `if`
> expression do not return the same type of value.  This restriction applies
> even if the predicate (`True` in this case) can only ever select one branch
> because import resolution strictly precedes normalization.
>
> </details>

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

   {- You can nest block comments -}
-}

2 + {- You can embed block comments anywhere -} 2
```

Comments have no effect on how the code is interpreted.  They are purely for
the benefit of people reading the code.

## `Bool` values

The `Bool` type is one of the simplest types that the language provides
built-in support for.

The only two valid `Bool` constants are `False` and `True` and the language
provides the following logical operators which work on `Bool` values:

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
>
> <details>
> <summary>Solution</summary>
>
> The type-checking phase rejects the expression
>
> The expression is syntactically valid (so parsing succeeds) and the
> expression has no imports (so import resolution trivially succeeds), so the
> only remaining phase that can reject the expression is type-checking.  This
> is because the language guarantees that normalization never fails once
> type-checking succeeds.
>
> </details>

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
> ⊢ 1e10000
> ```
>
> <details>
> <summary>Output</summary>
>
> ```
> Error: Invalid input
>
> (input):1:1:
>   |
> 1 | 1e10000
>   | ^
> double out of bounds
> ```
>
> </details>
> <br/>
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
>
> <details>
> <summary>Output</summary>
>
> ```dhall
> ''
> 🍋	🍓	🍍	🍉	🍌
> 😋 "Yum!"
> ''
> ```
>
> </details>

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
''
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
>
> <details>
> <summary>Solution</summary>
>
> ```dhall
> "    ABC\n  DEF\n"
> ```
>
> </details>

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

You can escape interpolation within a plain `Text` literal by escaping the
interpolation with a backslash, like this:

```dhall
"\${x}"
```

You can also escape interpolation within a multi-line `Text` literal by
prefixing the interpolation with `''`, like this:

```dhall
''
''${x}
''
```

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

... and the operator returns the left-hand side after checking it's type against
the right-hand side.

> **Exercise:** Ask the REPL for the type of the following expression:
>
> ```dhall
> ⊢ :type [ True, False ]
> ```
>
> Then verify that the type is correct by giving a type annotation to the
> same expression.
>
> <details>
> <summary>Output</summary>
>
> ```dhall
> List Bool
> ```
>
> </details>

You can insert this operator anywhere within Dhall code (although you may
need to wrap things in parentheses).  For example, this is a valid Dhall
expression:

```dhall
⊢ (2 : Natural) + (2 : Natural)

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

The hierarchy of types stops at `Sort`.  There is nothing above that.

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

  A `List` is also a "type" because `List : Type -> Type : Kind`

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
>
> <details>
> <summary>Solution</summary>
>
> * `2 + 2` is a term, because `2 + 2 : Natural : Type`
>
> * `List Natural` is a type, because `List Natural : Type : Kind`
>
> * `{ x = True, y = "ABC" }` is a term, because
>   `{ x = True, y = "ABC" } : { x : Bool, y : Text } : Type`
>
> * `Type -> Type` is a kind, because `Type -> Type : Kind : Sort`
>
> </details>

If you don't feel like classifying things, you can always call something an
"expression".  All terms, types, and kinds are expressions, too.

## Tests

The language provides built-in support for testing that two expressions are
equal using the `assert` keyword, like this:

```dhall
⊢ assert : 2 + 2 === 4

assert : 4 ≡ 4
```

This keyword lets you compare two expressions for equality at type-checking
time in conjunction with the `===` or `≡` (U+2261) operator.

If the expressions do not match then type-checking will fail and the
interpreter will display a diff:

```dhall
⊢ assert : 2 + 2 === 5

Error: Assertion failed

- 4
+ 5

1│ assert : 2 + 2 === 5

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
(which is impossible in general), but the interpreter can detect some simple
equalities.  You can use this feature to author tests that exhaustively
verify that a property holds for all possible values of a variable.

## `List`s

A `List` literal is represented by comma-separated elements surrounded by square
brackets, like this:

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
>
> <details>
> <summary>Output</summary>
>
> ```dhall
> List (Natural → Bool)
> ```
>
> </details>

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
>
> <details>
> <summary>Solution</summary>
>
> The type of the list can vary, but the solution should look like:
>
> ```dhall
> ([] : List Natural) # ([] : List Natural)
> ```
>
> The required type annotation needs to be parenthesized due to the type
> annotation operator being lower precedence than the list concatenation
> operator.
>
> </details>

There are other things you can do with `List`s, but we will cover that later
when we get to the Dhall Prelude.

## `Optional` values

By default, all Dhall types are not "nullable", meaning that there is no special
`nil` / `null` value that suffices for those types.

For example, if a Dhall expression has type `Bool` that means that
interpreting the expression must produce a `True` or `False` value.  No
other result is possible, and in particular a null value is impossible.

However, you can opt in to potentially empty types by wrapping types in
`Optional`.  For example, an `Optional Natural` is a `Natural` number that might
be present or might be absent.

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

  Carefully note `Some (None Bool)` and `None (Optional Bool)` do not
  necessarily mean the same thing.  Many languages only permit one "level" of
  nullability, but in Dhall you can nest the `Optional` type to an arbitrary
  depth and each layer is tracked separately.

* `List (Optional Bool)`

  All of the following values share this type:

  ```dhall
  [ Some True ]

  [ Some False ]

  [ None Bool ]

  [] : List (Optional Bool)
  ```

`Some` is a keyword that requires an argument, meaning that `Some 1` is a valid
expression, but `Some` by itself is not valid.  However, `None` is more
flexible, because `None` is an ordinary function that is valid in isolation.

> **Exercise:** What is the type of `None`?
>
> ```dhall
> ⊢ :type None
> ```
>
> <details>
> <summary>Output</summary>
>
> ```dhall
> ∀(A : Type) → Optional A
> ```
>
> </details>
> <br/>
>
> What happens if you try to enter `Some` by itself?
>
> ```dhall
> ⊢ :type Some
> ```
>
> <details>
> <summary>Output</summary>
>
> You get a parse error:
>
> ```
> Error: Invalid input
>
> (input):2:1:
>   |
> 2 | <empty line>
>   | ^
> unexpected end of input
> expecting argument to ❰Some❱ or whitespace
> ```
>
> </details>

You can do more with `Optional` values, which we will cover later when we
discuss the Dhall Prelude.

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

> **Exercise:** What type do you think the interpreter will infer for the
> `./example.dhall` file from earlier in the tutorial:
>
> ```dhall
> ⊢ :type ./example.dhall
> ```
>
> <details>
> <summary>Solution</summary>
>
> ```dhall
> List { admin : Bool, age : Natural, name : Text }
> ````
>
> </details>

You can access the fields of a record using `.` (the field access operator),
like this:

```dhall
let exampleRecord = { x = 1, y = 2 }

in  exampleRecord.x + exampleRecord.y
```

You can also "project" out a subset of fields using comma-separate field names
in braces, like this:

```dhall
⊢ :let point = { x = 10.3, y = 2.1, z = 9.1 }

point : { x : Double, y : Double, z : Double }

⊢ point.{ x, y }

{ x = 10.3, y = 2.1 }
```

You can also "project" out a subset of fields by the expected type, too, if
you surround the type in parentheses:

```dhall
⊢ :let Point2D = { x : Double, y : Double }

Point2D : Type

⊢ point.(Point2D)

{ x = 10.3, y = 2.1 }
```

## `let` expressions

Some of the following examples will not fit on one line, so we can use
`let` expressions to split our code into smaller expressions over multiple
lines.

We've already come across some `let` expressions, like in our first example:

```dhall
let List/filter = https://prelude.dhall-lang.org/v15.0.0/List/filter

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

... and the above example illustrates how `let` expressions can come in one of
two forms:

* A `let` expression with a type annotation:

  ```dhall
  let variableName : VariableType = expression

  in  result
  ```

* A `let` expression without a type annotation

  ```dhall
  let variableName = expression

  in  result
  ```

Both types of `let` expression create a variable that is a short-hand synonym
for the expression on the right-hand side of the `=` sign.  For example, this
expression:

```dhall
let x = 1

let y = 2

in  x + y
```

... is the same thing as:

```dhall
1 + 2
```

... since all that we've done is replaced all occurrences of `x` with `1` and
replaced all occurrences of `y` with `2`.

The first part of the `let` expression (before the `in` keyword) is known as
a "`let` binding" and a `let` expression can have more than one `let` binding.

There are very few restrictions on what you can create a synonym for with a
`let` expression.  For example, you can use a `let` expression to create a
synonym for a type and we saw one case of that in `example.dhall`:

```dhall
…

-- `Person` is a synonym for a record type …
let Person = { name : Text, age : Natural, admin : Bool }

-- … which we can use as the type annotation for this `let` expression
let alice : Person =
      { name = "Alice"
      , age = 24
      , admin = True
      }

…
```

The main restrictions are:

* You cannot create synonyms for keywords (e.g. `merge`, `if`, `Some`, etc.)

  You can only create synonyms for expressions

* You cannot create a `let` expression that refers to itself

  General recursion is not permitted

> **Exercise:** What do you think will happen if you try to define a recursive
> `let` binding?
>
> ```dhall
> ⊢ :let x = x + 1
> ```
>
> <details>
> <summary>Output</summary>
>
> ```
> Error: Unbound variable: x
>
> 1│  x
>
> (input):1:2
> ```
>
> Variables cannot refer to themselves within their own definition.
>
> </details>
> <br/>
>
> If the error message does not make sense, then enable more detailed error
> messages with the following command:
>
> ```dhall
> :set --explain
> ```
>
> ... and repeat the experiment.
>
> <details>
> <summary>Output</summary>
>
> ```
> Error: Unbound variable: x
>
> Explanation: Expressions can only reference previously introduced (i.e. “bound”)
> variables that are still “in scope”
>
> For example, the following valid expressions introduce a “bound” variable named
> ❰x❱:
>
>
>     ┌─────────────────┐
>     │ λ(x : Bool) → x │  Anonymous functions introduce “bound” variables
>     └─────────────────┘
>         ⇧
>         This is the bound variable
>
>
>     ┌─────────────────┐
>     │ let x = 1 in x  │  ❰let❱ expressions introduce “bound” variables
>     └─────────────────┘
>           ⇧
>           This is the bound variable
>
>
> However, the following expressions are not valid because they all reference a
> variable that has not been introduced yet (i.e. an “unbound” variable):
>
>
>     ┌─────────────────┐
>     │ λ(x : Bool) → y │  The variable ❰y❱ hasn't been introduced yet
>     └─────────────────┘
>                     ⇧
>                     This is the unbound variable
>
>
>     ┌──────────────────────────┐
>     │ (let x = True in x) && x │  ❰x❱ is undefined outside the parentheses
>     └──────────────────────────┘
>                              ⇧
>                              This is the unbound variable
>
>
>     ┌────────────────┐
>     │ let x = x in x │  The definition for ❰x❱ cannot reference itself
>     └────────────────┘
>               ⇧
>               This is the unbound variable
>
>
> Some common reasons why you might get this error:
>
> ● You misspell a variable name, like this:
>
>
>     ┌────────────────────────────────────────────────────┐
>     │ λ(empty : Bool) → if emty then "Empty" else "Full" │
>     └────────────────────────────────────────────────────┘
>                            ⇧
>                            Typo
>
>
> ● You misspell a reserved identifier, like this:
>
>
>     ┌──────────────────────────┐
>     │ foral (a : Type) → a → a │
>     └──────────────────────────┘
>       ⇧
>       Typo
>
>
> ● You tried to define a recursive value, like this:
>
>
>     ┌────────────────────┐
>     │ let x = x + 1 in x │
>     └────────────────────┘
>               ⇧
>               Recursive definitions are not allowed
>
>
> ● You accidentally forgot a ❰λ❱ or ❰∀❱/❰forall❱
>
>
>         Unbound variable
>         ⇩
>     ┌─────────────────┐
>     │  (x : Bool) → x │
>     └─────────────────┘
>       ⇧
>       A ❰λ❱ here would transform this into a valid anonymous function
>
>
>         Unbound variable
>         ⇩
>     ┌────────────────────┐
>     │  (x : Bool) → Bool │
>     └────────────────────┘
>       ⇧
>       A ❰∀❱ or ❰forall❱ here would transform this into a valid function type
>
>
> ● You forgot to prefix a file path with ❰./❱:
>
>
>     ┌────────────────────┐
>     │ path/to/file.dhall │
>     └────────────────────┘
>       ⇧
>       This should be ❰./path/to/file.dhall❱
>
> ────────────────────────────────────────────────────────────────────────────────
>
> 1│  x
>
> (input):1:2
> ```
>
> </details>

Dhall code idiomatically uses `let` expressions heavily, so don't be afraid to
split large expressions into lots of smaller `let` bindings.  You can always
interpret the code to remove indirection if necessary.

## Functions

All function types are of the form `A -> B` where `A` is the type of the
function's input and `B` is the type of the function's output.  You can also
use the Unicode right arrow `→` (U+2192) to represent a function type as
`A → B`.

For example, `Natural/even : Natural -> Bool` means that the `Natural/even`
function converts an input of type `Natural` to an output of type `Bool`:

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

> **Exercise:** Apply the above anonymous function directly to the argument `2`
> (without using a `let` expression).
>
> <details>
> <summary>Solution</summary>
>
> You need to parenthesize the anonymous function to apply the function to
> an argument:
>
> ```dhall
> ⊢ (\(x : Natural) -> x + 1) 2
>
> 3
> ```
>
> </details>

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
> in  not True
> ```
>
> <details>
> <summary>Solution</summary>
>
> ```dhall
> let not = \(b : Bool) -> if b then False else True
>
> in  not
> ```
>
> </details>

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
> <details>
> <summary>Output</summary>
>
> ```dhall
> List Natural → Natural
> ```
>
> </details>
> <br/>
>
> ... and then apply the `List/length function` to one more argument (a `List`)
> to compute the length of that `List`.
>
> <details>
> <summary>Solution</summary>
>
> ```dhall
> ⊢ List/length Natural [ 2, 3, 5 ]
>
> 3
> ```
>
> </details>

> **Challenge exercise:** Actually, you can omit the `forall` for the type of
> `List/length`.  Devise an equivalent type for `List/length` that does not
> use a `forall` or `∀` and check your answer in the REPL by giving
> `List/length` that type as an annotation:
>
> ```dhall
> ⊢ List/length : ???
> ```
>
> <details>
> <summary>Solution</summary>
>
> ```dhall
> ⊢ List/length : Type -> List _ -> Natural
>
> List/length
> ```
>
> The type `Type -> List _ -> Natural` is the same as
> `forall (_ : Type) -> List _ -> Natural`, which works because `_` is a legal
> name for a variable.
>
> </details>

"Polymorphic" functions like `List/length` require you to explicitly specify
types as their arguments.  For example, the first argument of `List/length`
is the type of the `List` that we want to count.  This type argument cannot be
omitted.

A more interesting example is the type of the `List/map` function from the
Prelude (which we'll get to below):

```dhall
List/map
  : forall (a : Type) -> forall (b : Type) -> (a -> b) -> List a -> List b
```

`List/map` transforms a `List` by applying a function to each element.

This function takes four arguments which are, in order:

* A `Type` (named `a`) which is the type of the elements for our input `List`
* A `Type` (named `b`) which is the type of the elements for our output `List`
* A function that can convert a value of type `a` into a value of type `b`
* An input `List` with elements of type `a`

... and the function returns an output `List` with elements of type `b`

> **Exercise:** Can you guess what this function does based on the function's
> name and type?
>
> ```dhall
> List/filter : forall (a : Type) -> (a -> Bool) -> List a -> List a
> ```
>
> <details>
> <summary>Solution</summary>
>
> `List/filter` is a function that returns elements in a list that match a
> given predicate.
>
> </details>

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
>
> <details>
> <summary>Output</summary>
>
> ```dhall
> ⊢ :type Example.Number
>
> ∀(Number : Natural) → < Boolean : Bool | Number : Natural >
>
> ⊢ :type Example.Boolean
>
> ∀(Boolean : Bool) → < Boolean : Bool | Number : Natural >
> ```
>
> </details>

> **Exercise:** Add another alternative to the `Example` type and then add
> a value wrapped in that alternative to the above `List`.
>
> <details>
> <summary>Solution</summary>
>
> ```dhall
> let Example = < Number : Natural | Boolean : Bool | String : Text >
>
> in  [ Example.Number 1, Example.Boolean True, Example.String "ABC" ]
> ```
>
> </details>

You can extract a value from a union type using the `merge` keyword.  This
keyword expects a record containing one function per alternative, like this:

```dhall
let Example = < Number : Natural | Boolean : Bool >

let renderExample
    : Example -> Text
    = \(example : Example) ->
        merge
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
>
> <details>
> <summary>Output</summary>
>
> ```
> Use "dhall --explain" for detailed errors
>
> Error: Missing handler: Boolean
>
> 6│         merge
> 7│           { Number = \(n : Natural) -> Natural/show n
> 8│           }
> 9│           example
>
> (input):6:9
> ```
>
> </details>

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
>
> <details>
> <summary>Solution</summary>
>
> ```dhall
> let Example = < Number : Natural | Boolean : Bool >
>
> let toNatural
>     : Example -> Natural
>     = \(example : Example) ->
>         merge
>           { Number = \(n : Natural) -> n
>           , Boolean = \(b : Bool) -> if b then 1 else 0
>           }
>           example
>
> let example0 = assert : toNatural (Example.Number 42) === 42
>
> let example1 = assert : toNatural (Example.Boolean False) === 0
>
> let example2 = assert : toNatural (Example.Boolean True) === 1
>
> in  toNatural
> ```
>
> </details>

Alternatives can be empty.  For example, you can define an "enum" as a union
with all empty alternatives:

```dhall
let DayOfWeek =
      < Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday >

let isWeekend
    : DayOfWeek -> Bool
    = \(day : DayOfWeek) ->
        merge
          { Sunday = True
          , Monday = False
          , Tuesday = False
          , Wednesday = False
          , Thursday = False
          , Friday = False
          , Saturday = True
          }
          day

in  isWeekend DayOfWeek.Sunday
```

A handler for an empty alternative requires no input (since an empty alternative
does not store a value).

> **Exercise:** Change the following expression to use an enum to represent each
> person's favorite color instead of `Text`:
>
> ```dhall
> [ { name = "Alice", favoriteColor = "Green" }
> , { name = "Bob"  , favoriteColor = "Blue"  }
> , { name = "Carlo", favoriteColor = "Red"   }
> ]
> ```
>
> <details>
> <summary>Solution</summary>
>
> ```dhall
> let Color = < Red | Green | Blue >
>
> in  [ { name = "Alice", favoriteColor = Color.Green }
>     , { name = "Bob"  , favoriteColor = Color.Blue  }
>     , { name = "Carlo", favoriteColor = Color.Red   }
>     ]
> ```
>
> </details>

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
      \(o : Optional Natural) ->
        merge { Some = \(n : Natural) -> n, None = 0 } o

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

  This trick is known as "currying" and the advantage of currying is that you
  can "partially apply" a "curried" function to one argument at a time.
  <br/>

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

## Built-in functions

The Dhall language also has a few built-in functions for processing built-in
types.

For example, some built-in functions on numbers are:

* `Natural/isZero : Natural -> Bool`

  Returns `True` if the input is `0`, `False` otherwise

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
> <details>
> <summary>Solution</summary>
>
> ```dhall
> -- ./puzzle.dhall
>
> let showWithoutPlus =
>       \(i : Integer) ->
>         let m = Integer/clamp i
>
>         let n = Integer/clamp (Integer/negate i)
>
>         in  if    Natural/isZero m
>             then  if Natural/isZero n then "0" else "-${Natural/show n}"
>             else  Natural/show m
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
> </details>

This tutorial does not cover all available built-in functions.  If you are
interested in the full list, see:

* [Built-in types, functions, and operators](../references/Built-in-types.md)

## File imports

Dhall supports importing expressions from local filepaths.  For example, run the
following command to save the expression `2` to a file named `./two.dhall`:

```dhall
⊢ :save ./two.dhall = 2
Expression saved to `./two.dhall`
```

... and then add that file to itself:

```dhall
⊢ ./two.dhall + ./two.dhall

4
```

This works because of Dhall's "import resolution" phase, where filepaths are
replaced with the expressions stored within those files.  In the above
example, the interpreter replaces the expression:

```dhall
./two.dhall + ./two.dhall
```

... with:

```dhall
2 + 2
```

... and then continues to interpret the expression to produce `4`.

> **Exercise:** Why does the interpreter reject the following expression?
>
> ```dhall
> ⊢ ./two.dhall: Natural
> ```
>
> <details>
> <summary>Solution</summary>
>
> ```
> ↳ ./two.dhall:
>
> Error: Missing file ./two.dhall:
>
> 1│ ./two.dhall:
>
> (input):1:1
> ```
>
> There needs to be a space in between `./two.dhall` and the `:`, otherwise the
> interpreter treats the `:` as part of the file name.
>
> </details>

Expressions imported in this way must be "closed", meaning that the imported
file cannot refer to variables that are not defined within the same file.  For
example, if you save the expression `x` to a file named `./x.dhall` then that
file can never be successfully imported, even if you write:

```dhall
let x = 1 in ./x.dhall  -- Still not valid
```

There are three types of filepath imports that Dhall understands:

* Relative imports (e.g. `./example.dhall` or `../sibling/example.dhall`)
* Absolute imports (e.g. `/usr/local/share/dhall/Prelude/package.dhall`)
* Home-anchored imports (e.g. `~/lib/utils.dhall`)

... although this tutorial will only use relative imports.

You can store essentially any expression within an import (just like a `let`
binding).  For example, you can store a type inside of an import if you want
to use that import as a "schema file":

```dhall
⊢ :save ./Person.dhall = { name : Text, age : Natural }
Expression saved to `./Person.dhall`

⊢ { name = "John Doe", age = 24 } : ./Person.dhall

{ age = 24, name = "John Doe" }
```

... or you can store a function inside of a file and apply that file to a
function argument:

```dhall
⊢ :save ./increment.dhall = \(x : Natural) -> x + 1
Expression saved to `./increment.dhall`

⊢ ./increment.dhall 2

3
```

Dhall files that you import in this way can themselves import other files
(also known as "transitive" dependencies).

> **Exercise:** Outside of the REPL, create the following file:
>
> ```dhall
> -- ./infant.dhall
>
> { name = "Jerry", age = ./two.dhall } : ./Person.dhall
> ```
>
> ... and then inside the REPL enter:
>
> ```dhall
> ⊢ (./infant.dhall).age
> ```
>
> <details>
> <summary>Output</summary>
>
> ```dhall
> 4
> ```
>
> </details>

> **Exercise:** What happens if you interpret a Dhall expression that imports
> itself?
>
> <details>
> <summary>Solution</summary>
>
> ```bash
> $ echo './x.dhall' > ./x.dhall
>
> $ dhall --file ./x.dhall
> dhall: 
> ↳ ./x.dhall
>
> Cyclic import: ./x.dhall
>
> 1│ ./x.dhall
>
> ./x.dhall:1:1
> ```
>
> </details>

## URL imports

Dhall expressions can import URLs, too, and this is how Dhall packages are
distributed.  For example, the most commonly used Dhall package is the Prelude,
which you can use like this:

```dhall
let Prelude = https://prelude.dhall-lang.org/v15.0.0/package.dhall

in  Prelude.Text.concatSep "," [ "apple", "banana", "pineapple" ]
```

If importing from URLs concerns you then take a moment to read about the safety
guarantees provided by the language:

* [Safety guarantees](../discussions/Safety-guarantees.md)

You can customize the HTTP headers used to fetch an import with the `using`
keyword, like this:

```dhall
⊢ https://httpbin.org/headers using [ { mapKey = "Accept", mapValue = "application/json" } ] as Text

''
{
  "headers": {
    "Accept": "application/json", 
    "Accept-Encoding": "gzip", 
    "Host": "httpbin.org", 
    "X-Amzn-Trace-Id": "Root=1-5e9b359d-aa07e5ccfb136c70db276b79"
  }
}
''
```

These extra headers are not able to access variables that are in scope (in order
to protect against leaking program secrets).  For example:

```dhall
⊢ :let secret = "Very secret"

secret : Text

⊢ https://httpbin.org/headers using [ { mapKey = "Secret", mapValue = secret } ] as Text

Error: Unbound variable: secret

1│                                                                     secret

(input):1:69

1│ https://httpbin.org/headers using [ { mapKey = "Secret", mapValue = secret } ] as Text

(input):1:1
```

Most Dhall packages are essentially large (possibly nested) records that you can
import that contain useful types and functions as their fields.

> **Exercise:** Import the Prelude within the REPL:
>
> ```dhall
> ⊢ :let Prelude = https://prelude.dhall-lang.org/v15.0.0/package.dhall
> ```
>
> ... and then use tab completion to explore what fields are available:
>
> ```dhall
> ⊢ Prelude.<TAB>
> Prelude.Bool      Prelude.JSON      Prelude.Monoid    Prelude.XML
> Prelude.Double    Prelude.List      Prelude.Natural
> Prelude.Function  Prelude.Location  Prelude.Optional
> Prelude.Integer   Prelude.Map       Prelude.Text
> ⊢ Prelude.List.<TAB>
> Prelude.List.all        Prelude.List.filter     Prelude.List.map
> Prelude.List.any        Prelude.List.fold       Prelude.List.null
> Prelude.List.build      Prelude.List.generate   Prelude.List.partition
> Prelude.List.concat     Prelude.List.head       Prelude.List.replicate
> Prelude.List.concatMap  Prelude.List.indexed    Prelude.List.reverse
> Prelude.List.default    Prelude.List.iterate    Prelude.List.shifted
> Prelude.List.drop       Prelude.List.last       Prelude.List.take
> Prelude.List.empty      Prelude.List.length     Prelude.List.unzip
> ```

## Prelude

You can browse the Prelude online here:

* [Prelude - https://prelude.dhall-lang.org](https://prelude.dhall-lang.org)

In particular, you might find the `README` helpful:

* [Prelude `README`](https://github.com/dhall-lang/dhall-lang/blob/master/Prelude/README.md)

Each Prelude function contains a comment explaining how to use the function.

> **Exercise:** Browse the documentation and source code for the
> `List/generate` utility here:
>
> * [Prelude - `List/generate`](https://prelude.dhall-lang.org/v15.0.0/List/generate)

... and the Prelude also re-exports all of the language built-ins (e.g.
`Natural/show`, `Integer/clamp`, etc.), including documentation and examples for
each built-in.  So you can use the Prelude to better understand and explore the
available built-in functions.

You can use the Prelude's `List/generate` function as a nested field of the
Prelude record: `Prelude.List.generate`.  For example:

```dhall
⊢ Prelude.List.generate 10 Text (\(n : Natural) -> "Result #${Natural/show n}")

[ "Result #0"
, "Result #1"
, "Result #2"
, "Result #3"
, "Result #4"
, "Result #5"
, "Result #6"
, "Result #7"
, "Result #8"
, "Result #9"
]
```

... or you can import the function individually, like this:

```dhall
⊢ :let List/generate = https://prelude.dhall-lang.org/v15.0.0/List/generate

List/generate : ∀(n : Natural) → ∀(a : Type) → ∀(f : Natural → a) → List a

⊢ List/generate 10 Text (\(n : Natural) -> "Result #${Natural/show n}")

[ "Result #0"
, "Result #1"
, "Result #2"
, "Result #3"
, "Result #4"
, "Result #5"
, "Result #6"
, "Result #7"
, "Result #8"
, "Result #9"
]
```

> **Exercise:** What happens if you apply `List/generate` to just one
> argument?
>
> ```dhall
> ⊢ List/generate 10
> ```
>
> <details>
> <summary>Output</summary>
>
> ```dhall
> λ(a : Type) →
> λ(f : Natural → a) →
>   [ f 0, f 1, f 2, f 3, f 4, f 5, f 6, f 7, f 8, f 9 ]
> ```
>
> </details>

> **Challenge exercise:** Save the following expression to `./Value.dhall`
>
> ```dhall
> -- ./Value.dhall
>
> < N : Natural | I : Integer | B : Bool >
> ```
>
> ... then save the following expression to `./input.dhall`:
>
> ```dhall
> -- ./input.dhall
>
> let Value = ./Value.dhall
>
> in  [ Value.N 1, Value.I +2, Value.B True ]
> ```
>
> ... and then create a Dhall expression in a `./solution.dhall` file that
> renders each element of the list on a separate line such that the result looks
> like this:
>
> ```bash
> $ dhall --file ./solution.dhall 
> ```
> ```dhall
> ''
> 1
> +2
> True
> ''
> ```
>
> The Prelude provides utilities that may come in handy for this exercise.
>
> <details>
> <summary>Solution</summary>
>
> ```dhall
> let Prelude = https://prelude.dhall-lang.org/v15.0.0/package.dhall
>
> let Value = ./Value.dhall
>
> let input = ./input.dhall
>
> let render =
>       \(value : Value) ->
>         merge
>           { N = Natural/show, I = Integer/show, B = Prelude.Bool.show }
>           value
>
> let toLine = \(value : Value) -> "${render value}\n"
>
> in  Prelude.Text.concatMap Value toLine input
> ```
>
> </details>

## Installing packages

You can make an import "installable" by protecting the import with an integrity
check of the form `sha256:${HASH}`.  This check belongs immediately after the
import, like this:

```dhall
let Prelude =
      https://prelude.dhall-lang.org/v15.0.0/package.dhall sha256:6b90326dc39ab738d7ed87b970ba675c496bed0194071b332840a87261649dcd

in  Prelude.Text.concatSep "," [ "apple", "banana", "pineapple" ]
```

There are two main ways you can obtain the hash:

* Recommended: Use `dhall freeze`

  ```bash
  $ dhall freeze --inplace ./example.dhall
  ```

* Use `dhall hash` from the command line or the `:hash` command in the REPL

  ```dhall
  ⊢ :hash https://prelude.dhall-lang.org/v15.0.0/package.dhall
  sha256:6b90326dc39ab738d7ed87b970ba675c496bed0194071b332840a87261649dcd
  ```

The interpreter will then locally cache any import annotated with an
integrity check the first time the import is resolved.  This can greatly
accelerate the interpreter once all imports are locally cached and no longer
require the network.

> **Exercise:** Time how long the interpreter takes to interpret the above
> example with and without the integrity check.

Adding an integrity check in this way also ensures that the import will no
longer change.  The interpreter always verifies integrity checks, whether
fetching the import for the first time, or loading the import from the local
cache.

These integrity checks are resilient to cosmetic changes in the imported
expression, meaning that the hash of an expression does not change if you
make behavior-preserving changes to that expression, such as:

* Adding/removing comments
* Refactoring the code

> **Exercise:** Save the following expressions to `./example0.dhall` and
> `./example1.dhall`, respectively:
>
> ```dhall
> -- ./example0.dhall
>
> [ { name = "Alice"
>   , age = 24
>   , admin = True
>   }
> , { name = "Bob"
>   , age = 49
>   , admin = True
>   }
> ]
> ```
>
> ```dhall
> -- ./example1.dhall
>
> let List/filter = https://prelude.dhall-lang.org/v15.0.0/List/filter
>
> let Person = { name : Text, age : Natural, admin : Bool }
>
> let alice : Person =
>       { name = "Alice"
>       , age = 24
>       , admin = True
>       }
>
> let bob : Person =
>       { name = "Bob"
>       , age = 49
>       , admin = True
>       }
>
> let carlo : Person =
>       { name = "Carlo"
>       , age = 20
>       , admin = False
>       }
>
> let isAdmin = \(person : Person) -> person.admin
>
> in  List/filter Person isAdmin [ alice, bob, carlo ]
> ```
>
> ... then hash both expressions.  The hashes should match.

The hash is resilient to behavior-preserving changes because the integrity check
is a "semantic" integrity check, meaning that it is a hash of a canonical
encoding of the program's syntax tree and not a hash of the program's source
code.  Also, the program is interpreted before hashing so that the hash is
insensitive to program indirection.

This integrity check protects against any sort of tampering with the import.
At worst import resolution will fail (if there is a hash mismatch), but you
will never get the wrong import.  For example, an import annotated with an
integrity check of
`sha256:27abdeddfe8503496adeb623466caa47da5f63abd2bc6fa19f6cfcb73ecfed70` can
never successfully resolve to a value other than `True`.  Every integrity
check uniquely identifies the corresponding Dhall expression (ignoring
highly improbable hash collisions).

Since the integrity check uniquely identifies the corresponding expression the
integrity check is "authoritative", meaning that an import will succeed if the
corresponding expression is already cached, regardless of whether or not the
import is available.

> **Exercise:** First resolve the Prelude with an integrity check within the
> REPL:
>
> ```dhall
> ⊢ :let Prelude = https://prelude.dhall-lang.org/package.dhall sha256:6b90326dc39ab738d7ed87b970ba675c496bed0194071b332840a87261649dcd
> ```
>
> ... now replace the URL with any other arbitrary URL and run the command
> again.  The import should still succeed!

The language has a special import that will always fail to resolve, called
`missing`:

```dhall
⊢ missing

Error: No valid imports

1│ missing

(input):1:1
```

... but the import will succeed if you attach an integrity check for an
import that is already cached:

```dhall
⊢ :let Prelude = missing sha256:6b90326dc39ab738d7ed87b970ba675c496bed0194071b332840a87261649dcd
```

You can use this trick to fetch any import from your local cache based on the
import's hash.  Continuing with the metaphor of "installing" a package, fetching
an import from a URL is analogous to installing a source package and fetching
an import from the local cache based on the hash is analogous to installing a
binary package.  In fact, locally cached imports are stored in a compressed
binary representation for efficiency, so you can really think of them as binary
packages that your interpreter downloaded and installed along the way.

Imports that don't have an integrity check will be resolved every time you
interpret them.  However, those imports may have transitive dependencies of
their own that are protected by integrity checks and those transitive
dependencies will be locally cached.  For example, the top-level `package.dhall`
expression that we import from the Prelude protects its own transitive
dependencies in this way, as an optimization to minimize network traffic.

## Importing raw `Text`

Sometimes you want to import `Text` from a file that is not a Dhall expression.
For example, you might want to import your SSH public key as a `Text` literal,
like this:

```dhall
⊢ ~/.ssh/id_ed25519.pub as Text
```

More generally, you can turn any import into a raw `Text` import by adding
`as Text` to the end of the import.

> **Exercise:** You can use this feature to turn `dhall` into a makeshift
> `curl`!  Try this:
>
> ```dhall
> ⊢ https://example.com as Text
> ```
>
> <details>
> <summary>Output</summary>
>
> ```dhall
> ''
> <!doctype html>
> <html>
> <head>
>     <title>Example Domain</title>
>
>     <meta charset="utf-8" />
>     <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
>     <meta name="viewport" content="width=device-width, initial-scale=1" />
>     <style type="text/css">
>     body {
>         background-color: #f0f0f2;
>         margin: 0;
>         padding: 0;
>         font-family: -apple-system, system-ui, BlinkMacSystemFont, "Segoe UI", "Open Sans", "Helvetica Neue", Helvetica, Arial, sans-serif;
>         
>     }
>     div {
>         width: 600px;
>         margin: 5em auto;
>         padding: 2em;
>         background-color: #fdfdff;
>         border-radius: 0.5em;
>         box-shadow: 2px 3px 7px 2px rgba(0,0,0,0.02);
>     }
>     a:link, a:visited {
>         color: #38488f;
>         text-decoration: none;
>     }
>     @media (max-width: 700px) {
>         div {
>             margin: 0 auto;
>             width: auto;
>         }
>     }
>     </style>    
> </head>
>
> <body>
> <div>
>     <h1>Example Domain</h1>
>     <p>This domain is for use in illustrative examples in documents. You may use this
>     domain in literature without prior coordination or asking for permission.</p>
>     <p><a href="https://www.iana.org/domains/example">More information...</a></p>
> </div>
> </body>
> </html>
> ''
> ```
>
> </details>

## Environment variable imports

You can also import Dhall expressions from environment variables, too, using
the following syntax:

```dhall
env:FOO
```

... replacing `FOO` with the desired environment variable.

However, you will more commonly import environment variables as raw `Text`,
like this:

```dhall
⊢ env:USER as Text  -- Get your current username
```

> **Exercise:** Store a Dhall function in an environment variable and then use
> the function stored inside that environment variable within a larger Dhall
> expression.
>
> <details>
> <summary>Solution</summary>
>
> ```bash
> $ INCREMENT='\(n : Natural) -> n + 1' dhall <<< 'env:INCREMENT 4'
> 5
> ```
>
> </details>

## Alternative imports

Sometimes you might want to provide a fallback import if import resolution
fails, which can happen for a number of reasons:

* A file path is missing
* A remote import is temporarily unavailable
* An environment variable is unset

The language provides a built-in `?` operator which lets you specify an
arbitrary expression as a fallback if an import fails.

For example, you could get the current `HOME` environment variable wrapped in a
`Some` if the variable is present, and return `None Text` if the environment
variable is unset:

```dhall
Some (env:HOME as Text) ? None Text
```

... or you could provide a fallback mirror if a URL is unavailable:

```dhall
  https://example.com/us-east/package.dhall
? https://example.com/us-west/package.dhall
```

... or you can provide multiple levels of fallbacks that let you override
imports for customization purposes:

```dhall
  env:DHALL_PRELUDE
? /usr/local/share/dhall/Prelude
? https://prelude.dhall-lang.org/v15.0.0/package.dhall
```

## Records - Part 2

Record management is a significant aspect of using Dhall "in anger", so the
language has several features designed to simplify working with records.

First, there are three operators which you can use to extend record values or
record types:

* `/\` - Recursive record value merge - Unicode: `∧` (U+2227)

  ```dhall
  ⊢ { a = { b = 1 }, d = True } /\ { a = { c = 1 } }

  { a = { b = 1, c = 1 }, d = True }
  ```

  This operator recursively merges two records, but fails with a type error if
  any two non-record fields "collide".

* `//` - Shallow right-biased record type merge - Unicode: `⫽` (U+2AFD)

  ```dhall
  ⊢ { a = { b = 1 } } // { a = 1, d = True }

  { a = 1, d = True }
  ```

  This operator merges two records, preferring fields from the right record if
  there is a collision.  Unlike `/\`, this operator does not recursively merge
  nested fields if two record-valued fields collide.

* `//\\` - Recursive record type merge - Unicode: `⩓` (U+2A53)

  ```dhall
  ⊢ { a : { b : Natural }, d : Bool } //\\ { a : { c : Natural } }

  { a : { b : Natural, c : Natural }, d : Bool }
  ```

  This operator is the type-level analog of `/\`, recursively merging two
  record types, failing if there are any collisions.

Additionally, record literals provide two types of syntactic sugar for working
with deeply-nested records.

First, you can represent nested fields more compactly using "dot" syntax for
nested fields, like this:

```dhall
{ a.b.c = 1 }
```

... which is syntactic sugar for:

```dhall
{ a = { b = { c = 1 } } }
```

Second, if you specify the same field twice, the interpreter will merge the
two fields using `/\`.  In other words, this expression:

```dhall
{ a = { b = { c = 1 } }, a = { b = { d = True } } }
```

... is syntactic sugar for:

```dhall
{ a = { b = { c = 1 } } /\ { b = { d = True } } }
```

... which normalizes to:

```dhall
{ a = { b = { c = 1, d = True } } }
```

This feature comes in handy when paired with the dot syntax for nested fields,
because you can then easily specify multiple nested fields by specifying the
"path" to each field:

```dhall
⊢ { a.b.c = 1, a.b.d = True }

{ a.b = { c = 1, d = True } }
```

> **Exercise:** Does `{ a = 1, a = 1 }` type-check?  Test your guess!
>
> <details>
> <summary>Output</summary>
>
> ```
> Error: Invalid duplicate field: a
>
> 1│ { a = 1, a = 1 }
>
> (input):1:1
> ```
>
> `{ a = 1, a = 1 }` is syntactic sugar for `{ a = 1 /\ 1 }`, and `1 /\ 1` is
> not a valid expression because the `/\` operator only works on records.
>
> </details>

> **Challenge exercise:** When is `{ a = foo, a.b = bar }` valid?  In other
> words, what conditions must be true about `foo` and/or `bar` for that
> expression to type-check?
>
> <details>
> <summary>Solution</summary>
>
> `{ a = foo, a.b = bar }` desugars to `{ a = foo /\ { b = bar } }`
>
> The expression type-checks if and only if one of the following is true:
>
> * `foo` is a record without a field named `b`
>
> * `foo` is a record with a field named `b` and `b /\ bar` is valid
>
> </details>

You can also easily override or add nested fields using `with` expressions, like
this:

```dhall
⊢ { a.b.c = 1, a.b.d = True } with a.b.c = 2 with a.b.e = "Hey"

{ a.b = { c = 2, d = True, e = "Hey" } }
```

> **Exercise:** Can you use a `with` expression to change the type of a nested
> field?  Test your guess
>
> <details>
> <summary>Solution</summary>
>
> Yes:
>
> ```dhall
> ⊢ { x = 1 } with x = True
>
> { x = True }
> ```
>
> </details>

## Record completion

The language includes one last record operator useful for working with large
records with many default-valued fields:

* `::` - Record completion

  ```dhall
  let Person =
        { Type = { name : Text, friends : List Text }
        , default = { friends = [] : List Text }
        }

  let examples =
        [ Person::{ name = "John" }
        , Person::{ name = "Alice", friends = [ "Charles" ] }
        ]

  let results =
        [ { name = "John", friends = [] : List Text }
        , { name = "Alice", friends = [ "Charles" ] }
        ]

  let test = assert : examples === results

  in  Person
  ```

This operator expects two arguments:

* The left argument is a "schema" record containing two fields:

  * A field named `Type` containing the desired record type

  * A field named `default` containing default values for any of the fields

* The right argument is a record that you want to "complete" by providing
  default values for unspecified fields

... and an expression of the form `T::r` is syntactic sugar for
`(T.default // r) : T.Type`

In the above example, `Person::{ name = "John" }` extends the record to add
a `friends` field with a default value of `[] : List Text`.  This works because:

```dhall
Person::{ name = "John" }

= (Person.default // { name = "John" }) : Person.Type

= ({ friends = [] : List Text } // { name = "John" }) : Person.Type

= { name = "John", friends = [] : List Text } : Person.Type

= { name = "John", friends = [] : List Text } : { name : Text, friends : List Text }

= { name = "John", friends = [] : List Text }
```

The type annotation ensures that any "required" fields are present, where a
"required" field is a field where no default value is specified.  In the above
example, the `name` field is required because the Person "schema" does not
specify a default value for the `name` field.

> **Exercise:** See what happens if you omit the `name` field required by the
> `Person` schema by interpreting the expression `Person::{=}`
>
> <details>
> <summary>Output</summary>
>
> The type-checker warns you about the missing required `name` field:
>
> ```
> ⊢ Person::{=}
>
> Error: Expression doesn't match annotation
>
> { - name : …
> , …
> }
>
> 1│ Person::{=}
>
> (input):1:1
> ```
>
> </details>

> **Exercise:** Create a "schema" named `Image` for a Docker image with the
> following fields:
>
> * An optional `registry` field of type `Text` that defaults to `"docker.io"`
> * A required `repository` field of type `Text`
> * A required `name` field of type `Text`
> * An optional `tag` field of type `Text` that defaults to `"latest"`
>
> ... then use your schema to create a sample record:
>
> ```dhall
> Image::{ repository = "library", name = "postgres" }
> ```
>
> <details>
> <summary>Solution</summary>
>
> ```dhall
> let Image =
>       { Type = { registry : Text, repository : Text, name : Text, tag : Text }
>       , default = { registry = "docker.io", tag = "latest" }
>       }
>
> in  Image::{ repository = "library", name = "postgres" }
> ```
>
> </details>

## Names

You might wonder which names are supposed to be uppercase or lowercase.  The
language does not care how you capitalize names, but you will typically
encounter the following conventions in the wild:

* Terms are given lowercase names
* Types are given uppercase names
* Fields that store terms are given lowercase names
* Fields that store types are given uppercase names
* Union alternatives are given uppercase names

However, these are not hard rules and you should feel free to deviate from them
if doing so more accurately mirrors the domain that you're trying to model.

In fact, the language gives you even more leeway about naming things if you're
willing to escape the names using backticks, like this:

```dhall
⊢ :let `Avogadro's Number` = 6.0221409e+23
```

You can also escape record field names and union alternative names in the same
way.

Escaping variable names permits ASCII whitespace and punctuation, but does not
permit arbitrary Unicode characters.  In other words, you can't have a variable
with an emoji name.

## Maps

Several Dhall features, tools, packages use `Map`s, where a `Map` is defined as
a list of key-value pairs:

```dhall
⊢ https://prelude.dhall-lang.org/v15.0.0/Map/Type as Text

''
{- This is the canonical way to encode a dynamic list of key-value pairs.

   Tools (such as `dhall-to-json`/`dhall-to-yaml` will recognize values of this
   type and convert them to maps/dictionaries/hashes in the target language

   For example, `dhall-to-json` converts a Dhall value like this:

   [ { mapKey = "foo", mapValue = 1 }
   , { mapKey = "bar", mapValue = 2 }
   ] : ./Map Text Natural

   ... to a JSON value like this:

   { "foo": 1, "bar", 2 }
-}
let Map
    : Type → Type → Type
    = λ(k : Type) → λ(v : Type) → List { mapKey : k, mapValue : v }

in  Map
''
```

`Map` is our first example of a custom type-level function.  `Map` is a function
that takes two function arguments (the type of each key and the type of each
value), and returns a new type (a `List` of key-value pairs).

For example, earlier we introduced support for custom headers which are
specified as a value of type `Map Text Text`:

```dhall
⊢ :let Map = https://prelude.dhall-lang.org/v15.0.0/Map/Type

Map : ∀(k : Type) → ∀(v : Type) → Type

⊢ Map Text Text

List { mapKey : Text, mapValue : Text }

⊢ [ { mapKey = "Accept", mapValue = "application/json" } ] : Map Text Text

[ { mapKey = "Accept", mapValue = "application/json" } ]
```

The language also includes a `toMap` keyword which you can use to convert
records to `Map`s.  For example:

```dhall
⊢ toMap { Accept = "application/json" }

[ { mapKey = "Accept", mapValue = "application/json" } ]
```

... so we could use `toMap` to specify HTTP custom headers:

```dhall
⊢ https://httpbin.org/headers using (toMap { Accept = "application/json" }) as Text
```

A `Map` is a "homogeneous" map, meaning that all values must have the same type.
That implies that you can't use `toMap` on a "heterogeneous" record with
different types of values:

```dhall
⊢ toMap { x = 1, y = True }

Error: ❰toMap❱ expects a homogenous record

1│ toMap { x = 1, y = True }

(input):1:1
```

Also, if you use `toMap` on an empty record, then you need to supply an
explicit type annotation (just like an empty `List`):

```dhall
⊢ toMap {=} : Map Text Natural

[] : List { mapKey : Text, mapValue : Natural }
```

## Conclusion

That concludes the language tour!  By this point you should have touched on
every language feature.

Please let us know if this tutorial is missing any language features by opening
an issue here:

* [`dhall-lang/dhall-lang` - Issues](https://github.com/dhall-lang/dhall-lang/issues)

We do our best to keep the tutorial up-to-date as the language evolves, but we
sometimes miss things.
