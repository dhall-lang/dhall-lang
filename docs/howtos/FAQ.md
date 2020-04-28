# Frequently Asked Questions (FAQ)

> Common questions and workarounds for limitations of the language

```eval_rst
.. contents:: Table of Contents
   :backlinks: none
```

## Imports relative to my top-level file are not working

All top-level imports are relative to the current working directory.  For example, if you
have a file located at `./foo/bar.dhall` that tries to import `./foo/baz.dhall` via a
relative import:

```console
$ cat ./foo/bar.dhall
```
```dhall
./baz.dhall
```
```console
$ cat ./foo/baz.dhall
```
```dhall
1
```

... that relative import will not work correctly if you feed that file to a Dhall
interpreter via standard input:

```console
$ dhall < ./foo/bar.dhall
↳ ./baz.dhall

Error: Missing file ./baz.dhall
```

This is because the interpreter does not know that the string fed in via standard
input originally came from `./foo/bar.dhall`.  Therefore, the interpreter cannot
process the relative import correctly.

However, the relative import does work correctly if you pass the program using
the `--file` flag, like this:

```console
$ dhall --file ./foo/bar.dhall
```
```dhall
1
```

## Can I create a function with default values for function arguments?

The Dhall configuration language provides language support for completing a record with default-valued fields using the `::` operator.

For example, in Python you can write:

```python
def greet(greeting="Hello", name="John"):
    print("{0}, {1}!".format(greeting, name))

greet()
greet(greeting="Hola")
greet(name="Jane")
greet(greeting="Hola",name="Jane")
```

... which produces this result:

```console
$ python greet.py
Hello, John!
Hola, John!
Hello, Jane!
Hola, Jane!
```

The Dhall equivalent of the above code would be:

```dhall
let greet =
          \(args : { greeting : Text, name : Text })
      ->  "${args.greeting}, ${args.name}!"

let Greeting =
      { Type = { greeting : Text, name : Text }
      , default = { greeting = "Hello", name = "John" }
      }

in  ''
    ${greet Greeting::{=}}
    ${greet Greeting::{ greeting = "Hola" }}
    ${greet Greeting::{ name = "Jane" }}
    ${greet Greeting::{ greeting = "Hola", name = "Jane" }}
    ''
```

... which produces the same result:

```console
$ dhall text --file ./greet.dhall
Hello, John!
Hola, John!
Hello, Jane!
Hola, Jane!
```

## How does `dhall lint` differ from `dhall format`?

`dhall lint`:

* Removes unused `let` bindings

* Consolidates nested `let` bindings to use a multiple-`let` binding

  See: [Add support for `let` expressions with multiple `let` bindings](https://github.com/dhall-lang/dhall-lang/pull/266)

* Fixes `let` bindings containing equivalences (`≡`) that were probably intended as assertions.

  See: [Add `dhall lint` support for fixing malformed assertions](https://github.com/dhall-lang/dhall-haskell/pull/1353)

## How do I update nested fields in a record?

You have to nest updates, like this:

```dhall
let example = { coordinate = { x = 5, y = 3 }, element = "Hg" }

in  example // {
      coordinate = example.coordinate // {
        x = example.coordinate.x + 1
      }
    }
```

You can also use the `with` keyword as a more concise syntactic sugar for the same expression:

```dhall
let example = { coordinate = { x = 5, y = 3 }, element = "Hg" }
in  example
  with coordinate.x = (example.coordinate.x + 1)
```

See [Add support for `with` keyword](https://github.com/dhall-lang/dhall-lang/pull/923) for more details.

## Why do empty lists require a type annotation?

Dhall cannot infer a polymorphic type for an empty list because Dhall represents
polymorphic values as functions of types, like this:

```dhall
\(a : Type) -> [] : List a
```

If the compiler treated an empty list literal as syntactic short-hand for
the above polymorphic function then you'd get the unexpected behavior where
a list literal is a function if the list has 0 elements but not a function
otherwise.

## Does Dhall support user-defined recursive types?

No, but you can translate recursive code to non-recursive code by following
this guide: [How to translate recursive code to Dhall][howto-recursive]

[howto-recursive]: <../howtos/How-to-translate-recursive-code-to-Dhall>
