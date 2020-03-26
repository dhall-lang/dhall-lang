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

## Introduction

All Dhall integrations support some way to load "plain" data into the desired
file format or language binding, like:

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
translated to functions in the host language.  However, this tutorial won't cover
that use case since not all integrations support this and the mechanism is
language-specific.

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

All integrations can go a step further and interpret the Dhall expression before
converting the expression into the desired format or language.  For example,
the following expression is equivalent to our original example (albeit more
indirect and long-winded):

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

This is because every Dhall integration comes complete with a built-in
interpreter capable of evaluating Dhall expressions, reducing them to plain
data before further conversion.

All integrations interpret Dhall expressions in five phases:

* Desugaring

  Some higher-level language features are "syntactic sugar" for lower-level
  language-features.  "Desugaring" is the process of translating higher-level
  features to lower-level features.

* Import resolution

  This phase replaces URLs, file paths, and environment variables with the
  expressions that they refer to.

* Type checking

  This phase ensures that the code is safe to evaluate by detecting and
  forbidding expressions that might lead to crashes, loops, or internal errors.

* Normalization (a.k.a. "Evaluation")

  This phase eliminates all indirection in the expression by evaluating all
  remaining programming language features that were not already covered by one
  of the preceding phases.  The result is an expression in a canonical "normal
  form".

* Marshalling

  The Dhall expression is converted to the desired format or an expression
  within the desired language.

Integrations will typically perform all of these steps in one go, but you can
use the `dhall` command-line tool to separate out some of these steps.

> **Exercise:** Save the above Dhall expression to `./example.dhall` and then
> run the following commands:
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

... and study how the Dhall expression evolves between phases.

Note that some of these commands can actually perform all of the preceding
steps.  For example, `dhall-to-json` could have taken the original
`./example.dhall` as input and produced the same result.  However, splitting
things into explicit phases can sometimes help better understand how the
interpreter processes the code.

> **Exercise:** There are two URL imports in the following Dhall expression:
>
> ```dhall
> if True
> then https://prelude.dhall-lang.org/Bool/not
> else https://prelude.dhall-lang.org/List/length
> ```
>
> Which imports are resolved by the interpreter?
>
> Does the expression type-check?
