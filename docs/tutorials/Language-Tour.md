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
into Dhall.

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

## Introduction

All Dhall integrations support some way to load "plain" data into the desired
file format or language binding.  These "plain" values include:

* `Bool` values
* `Double` values
* `Natural` values
* `Integer` values
* `Text` values
* `Optional` values
* `List`s
* Records

... and possibly also unions, depending on the integration.

Some languages can even load Dhall functions, meaning that they are dynamically
translated to functions in the host language.  However, this tutorial will not
cover that.

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

A Dhall interpreter processes expressions in five phases:

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
