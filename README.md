![](./img/dhall-logo.png)

<head>
  <meta charset="UTF-8">
</head> 

Dhall is a programmable configuration language that provides a
non-repetitive alternative to YAML.

You can think of Dhall as: JSON + functions + types + imports

Note that while Dhall is programmable, Dhall is not Turing-complete.  Many
of Dhall's features take advantage of this restriction to provide stronger
safety guarantees and more powerful tooling.

You can try the language live in your browser by visiting the official website:

* [https://dhall-lang.org](http://dhall-lang.org/)

## Table of contents

* [Getting Started](#getting-started)
* [Tutorials](#tutorials)
* [Features](#features)
* [Documentation](#documentation)
* [Standard Library](#standard-library)
* [Development Status](#development-status)
* [Overview](#overview)
    * [Interpreter](#interpreter)
    * [Language Bindings](#language-bindings)
        * [Haskell](#haskell)
        * [Nix](#nix)
        * [Ruby](#ruby)
    * [Compilers](#compilers)
        * [JSON and YAML](#json-and-yaml)
        * [Bash](#bash)
        * [Text](#text)
* [Design Philosophy](#design-philosophy)
* [Name](#name)
* [Contributors](#contributors)
    * [Code Contributors](#code-contributors)
    * [Financial Contributors](#financial-contributors)
        * [Individuals](#individuals)
        * [Organizations](#organizations)

## Getting started

The easiest way to get started experimenting with Dhall is to install the
`dhall-to-json` and/or `dhall-to-yaml` executables, which enable you to
generate JSON and YAML, respectively, on the command line. Platform- and
runtime-specific installation instructions can be found in [the Dhall
wiki][dhall-json-tutorial-wiki].

## Tutorials

For a short introduction, read:

* [Learn Dhall in Y minutes][learn-dhall-in-y-minutes]

To learn more about core language features, read:

* [Core language features][core-language-features]

For an even longer hands-on tutorial, read:

* [Getting started: Generate JSON or YAML][dhall-json-tutorial-wiki]

## Features:

* Total - Evaluation always terminates and never hangs or infinitely loops
* Safe - Evaluation never crashes or throws exceptions
* Distributed - Expressions can reference other expressions by URL or path
* Strongly normalizing - All expressions (even functions) have a normal form
* Statically typed - Configurations can be validated ahead-of-time
* Strongly typed - No coercions, casts or subtyping
* Built-in data types - Includes lists, anonymous records and anonymous unions

## Documentation

The Dhall language originated as a Haskell-specific configuration file format
and is expanding to support more languages and file formats.  Consequently, the
Haskell package for Dhall still hosts the official tutorial and language manual:

* [Dhall tutorial and manual][dhall-haskell-tutorial]

... which will eventually become a language-agnostic tutorial

You can also read about the original motivation behind the language here:

* [Dhall - A non-Turing-complete configuration language][dhall-haskell-post]

Finally, we have a cheatsheet for a very condensed overview and quick lookup:

* [Dhall Cheatsheet](https://github.com/dhall-lang/dhall-lang/wiki/Cheatsheet)

## Standard Library

Dhall's Standard Library is called `Prelude`. It implements various utilities to
work with the builtin types. Where to find it:

* [Official link - https://prelude.dhall-lang.org](https://prelude.dhall-lang.org)
* [Github repo](https://github.com/dhall-lang/dhall-lang/tree/master/Prelude)
* [Nix](https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/interpreters/dhall/default.nix):
  both `dhall` and `dhall.prelude` derivations are built, install the `Prelude`
  with e.g. `nix-env -iA nixpkgs.dhall.prelude`

## Development status

The current version and versioning policy is detailed in the
[Versioning document](./standard/versioning.md), and you can see the latest
changes [in the Changelog](CHANGELOG.md).

There is an effort under way to formalize the language semantics for Dhall, to
help with porting it to [other languages](#language-bindings).
If you would like to assist with either standardizing the language or creating
new bindings just open a new issue or contribute to existing ones in the [issue
tracker][issue-tracker].

## Overview

You can use Dhall in one of three ways:

*   **Interpreter:** You can install a language-independent command-line program
    that can import, type-check and evaluate Dhall expressions
*   **Language binding:** You can use a language-specific library to load
    Dhall configuration files directly into your programs
*   **Compilers:** You can compile Dhall expressions to several common
    configuration file formats using command-line utilities

I recommend the following progression for adopting Dhall:

* Install the interpreter to learn more about the language
* If you like what you see, then:
    * If your preferred programming language supports Dhall:
        * Use the language binding to configure your programs using Dhall
    * If your language does not yet support Dhall
        * Compile Dhall to a supported configuration format (such as JSON)

The following sections tour each of these use cases in more detail

## Interpreter

You can install a Dhall interpreter that can type-check and evaluate Dhall
expressions from the command line.  You can use this interpreter to:

*   **Learn how the language works:**

    ```bash
    $ dhall <<< 'True && False'
    Bool

    False
    ```

    The first line of output shows the inferred type of an expression:

    ```bash
    $ dhall <<< 'List/length'
    ∀(a : Type) → List a → Natural

    List/length
    ```

    The second line of output shows the fully evaluated normal form of an
    expression:

    ```bash
    $ dhall <<< 'λ(x : Text) → let y = True in if y != False then x else "?"'
    ∀(x : Text) → Text

    λ(x : Text) → x
    ```

*   **Validate a configuration file ahead of time against a schema:**

    ```bash
    $ cat config
    { foo = List/length Natural [2, 3, 5], bar = True && False }
    ```

    ```bash
    $ cat schema
    { foo : Natural, bar : Bool }
    ```

    Dhall lets you import expressions and types by their path:

    ```bash
    $ dhall <<< './config : ./schema'
    { bar : Bool, foo : Natural }

    { bar = False, foo = 3 }
    ```

    Schema validation is the same thing as a type annotation

*   **Detect type errors:**

    ```bash
    $ dhall <<< 'λ(x : Integer) → x && True'

    Use "dhall --explain" for detailed errors

    Error: ❰&&❱ only works on ❰Bool❱s

    x && True

    (stdin):1:18
    ```

    You can ask the type checker to go into more detail using the `--explain`
    flag:

    ```bash
    $ dhall --explain <<< 'λ(x : Integer) → x && True'
    
    
    x : Integer
    
    Error: ❰&&❱ only works on ❰Bool❱s
    
    Explanation: The ❰&&❱ operator expects two arguments that have type ❰Bool❱
                                                                                    
    For example, this is a valid use of ❰&&❱:                           
                                                                                    
                                                                                    
        ┌───────────────┐                                                           
        │ True && False │                                               
        └───────────────┘                                                           
                                                                                    
                                                                                    
    You provided this argument:                                                     
                                                                                    
    ↳ x                                                                
                                                                                    
    ... which does not have type ❰Bool❱ but instead has type:                       
                                                                                    
    ↳ Integer                                                                
    
    ────────────────────────────────────────────────────────────────────────────────
    
    x && True
    
    (stdin):1:18
    ```

*   **Resolve remote expressions:**

    ```bash
    dhall <<< 'https://raw.githubusercontent.com/dhall-lang/Prelude/35deff0d41f2bf86c42089c6ca16665537f54d75/List/replicate'

    ∀(n : Natural) → ∀(a : Type) → ∀(x : a) → List a

    λ(n : Natural) → λ(a : Type) → λ(x : a) → List/build a (λ(list : Type) → λ(cons : a → list → list) → Natural/fold n list (cons x))
    ```

    You can import arbitrary expressions by URL.  In fact, the Dhall Prelude
    is hosted on GitHub to be imported in this way:

    * [Dhall Prelude][dhall-prelude]

*   **Reduce an expression to normal form:**

    ```bash
    $ cat ./example
    let replicate = https://raw.githubusercontent.com/dhall-lang/Prelude/35deff0d41f2bf86c42089c6ca16665537f54d75/List/replicate

    let exclaim = λ(t : Text) → t ++ "!"

    in  λ(x : Text) → replicate 3 Text (exclaim x)
    ```

    You can reduce functions to normal form, even when they haven't been
    applied to all of their arguments

    ```bash
    $ dhall <<< './example'
    ∀(x : Text) → List Text

    λ(x : Text) → [x ++ "!", x ++ "!", x ++ "!"] : List Text
    ```

    The normal form is equivalent to the original program except stripped of
    all imports and indirection

Learn more:

* [GitHub repository][dhall-haskell]
* [Language guide][dhall-haskell-tutorial]
* [Blog post][dhall-haskell-post]

## Language Bindings

You can use Dhall to configure programs written in other languages. This is the
most common use case for Dhall: a type-safe and non-Turing-complete
configuration language 

Dhall currently supports four complete language bindings:

* [Haskell][dhall-haskell]
* [Java - via Eta][dhall-eta]
* [Nix][dhall-nix]
* [Ruby][dhall-ruby]

... and four language bindings in progress:

* [Clojure][dhall-clj]
* [PureScript][dhall-purescript]
* [Python][dhall-python]
* [Rust][dhall-rust]

The following language bindings wrap prebuilt Dhall packages or executables:

* [`go-dhallconfig`](https://github.com/andrewchambers/go-dhallconfig) -
  Go package that wraps the `dhall-to-json` executable
* [`clay-dhall`](https://github.com/as-capabl/clay-dhall) -
  C bindings to the Haskell library

The following bindings are not maintained but you may find them
useful as a starting point:

* [Scala][dhall-scala]

### Haskell

You can load Dhall expressions into Haskell using the `input` function:

```haskell
>>> input auto "True && False' :: IO Bool
False
```

The compiler validates the expression against the expected type:

```haskell
>>> input auto "1" :: IO Bool
*** Exception: 
Error: Expression doesn't match annotation

1 : Bool

(input):1:1
```

You can even marshall some Dhall functions into Haskell:

```haskell
>>> twice <- input auto "λ(x : Integer) → [x, x]" :: IO (Integer -> [Integer])
>>> twice 5
[5,5]
```

You can also decode Dhall expressions into Haskell types that derive
`Generic`:

```haskell
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import Dhall

data Example = Example { foo :: Natural, bar :: Vector Double }
    deriving (Generic, Show)

instance Interpret Example

main :: IO ()
main = do
    x <- input auto "./config"
    print (x :: Example)
```

... which reads in this configuration file:

```bash
$ cat ./config
{ foo = 1
, bar = [3.0, 4.0, 5.0]
}
```

... and prints the configuration:

```bash
$ ./example
Example {foo = 1, bar = [3.0,4.0,5.0]}
```

Learn more:

* [GitHub repository][dhall-haskell]
* [Tutorial][dhall-haskell-tutorial]

### Nix

`nixpkgs` provides a `dhallToNix` utility which you can use to translate a
Dhall expression to the corresponding Nix expression.  This lets you carve out
a typed and total subset of Nix

For example, this Dhall expression:

```nix
let
  pkgs = import <nixpkgs> { };

in
  pkgs.dhallToNix ''
    { foo = λ(x : Bool) → [x, x]
    , bar = Natural/even 2
    , baz = https://raw.githubusercontent.com/dhall-lang/Prelude/35deff0d41f2bf86c42089c6ca16665537f54d75/Text/concat
    }
  ''
```

... translates to this Nix expression

```nix
{ foo = x : [ x x ];
  bar = true;
  baz = xs:
    (t: xs: t: cons:
      builtins.foldl' (f: y: ys: f (cons y ys)) (ys: ys) xs
    ) {} xs {} (x: y: x + y) "";
}
```

Learn more:

* [`dhallToNix` function][dhallToNix]
* [GitHub repository][dhall-nix]
* [Tutorial][dhall-nix-tutorial]
* [Blog post][dhall-nix-post]

### Ruby

You can load Dhall expressions into Ruby using `Dhall.load`:

```ruby

Dhall.load("True && False").then do |value|
  value # => #<Dhall::Bool value=false>
end
```

You can even marshall Dhall functions into Ruby:

```ruby
Dhall.load("λ(x : Integer) → [x, x]").then do |f|
  f.call(1) # => #<Dhall::List elements=[#<Dhall::Natural value=1>, #<Dhall::Natural value=1>]>
end
```

Learn more:

* [Sourcehut repository][dhall-ruby]
* [RubyDoc][dhall-ruby-rubydoc]
* [Rubygems][dhall-ruby-rubygems]

## Compilers

You can compile Dhall expressions to other configuration formats, such as:

* [JSON and YAML][dhall-json]
* [Bash][dhall-bash] declarations
* [Text][dhall-text] (i.e. Dhall as a template engine)

These compilers can only translate a subset of Dhall to these other formats.
For example, you cannot translate Dhall functions to JSON

### JSON and YAML

The `dhall-to-json` executable lets you compile Dhall expressions to JSON:

```bash
$ dhall-to-json <<< '{ foo = 1, bar = True, baz = [1, 2, 3] }'
{"foo":1,"baz":[1,2,3],"bar":true}
```

YAML is already a superset of JSON, but the `dhall-to-yaml` executable lets you
compile Dhall expressions to idiomatic YAML syntax, too:

```bash
dhall-to-yaml <<< '{ foo = 1, bar = True, baz = [1, 2, 3] }'
foo: 1
baz:
- 1
- 2
- 3
bar: true
```

Some configuration file formats are supersets of JSON such as the
[HashiCorp configuration language][hcl] (Used to configure `terraform`) so you
can compile Dhall expressions to these configuration file formats, too.

Learn more:

* [GitHub repository][dhall-json]
* [Tutorial][dhall-json-tutorial]
* [Blog post][dhall-json-post]

### Bash

You can also compile Dhall expressions to either Bash expressions:

```bash
$ dhall-to-bash <<< 'List/length Bool [True, False]'
2
```

... or Bash declarations:

```bash
$ dhall-to-bash --declare FOO <<< '{ bar = 1, baz = "ABC" }'
declare -r -A FOO=([bar]=1 [baz]=ABC)
$ eval $(dhall-to-bash --declare FOO <<< '{ bar = 1, baz = "ABC" }')
$ echo ${FOO[bar]}
1
$ echo ${FOO[baz]}
ABC
```

```bash

```

Learn more:

* [GitHub repository][dhall-bash]
* [Tutorial][dhall-bash-tutorial]
* [Blog post][dhall-bash-post]

### Text

You can also use the `dhall-to-text` executable as a template engine

For example, given this template:

```bash
$ cat function
    λ(record : { name        : Text
               , value       : Double
               , taxed_value : Double
               , in_ca       : Bool
               }
     ) → ''
Hello ${record.name}
You have just won ${Double/show record.value} dollars!
${ if record.in_ca
   then "Well, ${Double/show record.taxed_value} dollars, after taxes"
   else ""
 }
''
```

... and this payload:

```bash
$ cat value
{ name        = "Chris"
, value       = 10000.0
, taxed_value = 6000.0
, in_ca       = True
}
```

... you can apply the template to the payload and render the result using the
`dhall-to-text` executable:

```bash
$ dhall-to-text <<< './function ./value'
Hello Chris
You have just won 10000.0 dollars!
Well, 6000.0 dollars, after taxes
```

Learn more:

* [GitHub repository and tutorial][dhall-text]
* [Blog post][dhall-text-post]

## Design philosophy

Programming languages are all about design tradeoffs and the Dhall language uses
the following guiding principles (in order of descending priority) that help
navigate those tradeoffs:

* Polish

    The language should delight users.  Error messages should be fantastic,
    execution should be snappy, documentation should be excellent, and
    everything should "just work".

* Simplicity

    When in doubt, cut it out.  Every configuration language needs bindings to
    multiple programming languages, and the more complex the configuration
    language the more difficult to create new bindings.  Let the host language
    that you bind to compensate for any missing features from Dhall.

* Beginner-friendliness

    Dhall needs to be a language that anybody can learn in a day and debug
    with little to no assistance from others.  Otherwise people can't recommend
    Dhall to their team with confidence.

* Robustness

    A configuration language needs to be rock solid.  The last thing a person
    wants to debug is their configuration file.  The language should never hang
    or crash.  Ever.

* Consistency

    There should only be one way to do something.  Users should be able to
    instantly discern whether or not something is possible within the Dhall
    language or not.

The Dhall configuration language is also designed to negate many of the common
objections to programmable configuration files, such as:

> "Config files shouldn't be Turing complete"

Dhall is not Turing-complete.  Evaluation always terminates, no exceptions

> "Configuration languages become unreadable due to abstraction and indirection"

Every Dhall configuration file can be reduced to a normal form which eliminates
all abstraction and indirection

> "Users will go crazy with syntax and user-defined constructs"

Dhall is a very minimal programming language.  For example: you cannot even
compare strings for equality.  The language also forbids many other common
operations in order to force users to keep things simple

The biggest issue with using Dhall as a configuration language is that there are
currently only Haskell bindings.  If you would like to contribute bindings to
another language then go for it, otherwise I will do my best to contribute them
as time permits.

## Name

The language is named after a
[Dustman from the game Planescape: Torment][dhall-name]
who belongs to a faction obsessed with death (termination).  The logo represents
his quill and inkwell

The name rhymes with "tall"/"call"/"hall" (i.e. "dɔl" for a US speaker or
"dɔːl" for a UK speaker using the International Phonetic Alphabet).

[dhall-haskell]: https://github.com/dhall-lang/dhall-haskell
[dhall-haskell-tutorial]: https://hackage.haskell.org/package/dhall/docs/Dhall-Tutorial.html
[dhall-haskell-post]: http://www.haskellforall.com/2016/12/dhall-non-turing-complete-configuration.html
[dhall-nix]: https://github.com/dhall-lang/dhall-nix
[dhall-nix-tutorial]: https://hackage.haskell.org/package/dhall-nix/docs/Dhall-Nix.html
[dhall-nix-post]: http://www.haskellforall.com/2017/01/typed-nix-programming-using-dhall.html
[dhall-ruby]: https://git.sr.ht/~singpolyma/dhall-ruby
[dhall-ruby-rubydoc]: https://www.rubydoc.info/gems/dhall
[dhall-ruby-rubygems]: https://rubygems.org/gems/dhall
[dhall-clj]: https://github.com/f-f/dhall-clj
[dhall-purescript]: https://github.com/MonoidMusician/dhall-purescript
[dhall-eta]: https://github.com/eta-lang/dhall-eta
[dhall-python]: https://github.com/SupraSummus/dhall-python
[dhall-scala]: https://github.com/amarpotghan/dhall-scala
[dhall-rust]: https://github.com/Nadrieril/dhall-rust
[dhall-json]: https://github.com/dhall-lang/dhall-json
[dhall-json-tutorial]: https://hackage.haskell.org/package/dhall-json/docs/Dhall-JSON.html
[dhall-json-post]: http://www.haskellforall.com/2017/02/program-json-and-yaml-with-dhall.html
[dhall-bash]: https://github.com/dhall-lang/dhall-bash
[dhall-bash-tutorial]: https://hackage.haskell.org/package/dhall-bash/docs/Dhall-Bash.html
[dhall-bash-post]: http://www.haskellforall.com/2017/04/use-dhall-to-configure-bash-programs.html
[dhall-text]: https://github.com/dhall-lang/dhall-haskell/tree/master/dhall-text
[dhall-text-post]: http://www.haskellforall.com/2017/06/dhall-is-now-template-engine.html
[dhallToNix]: https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/dhall-to-nix.nix
[dhall-name]: http://torment.wikia.com/wiki/Dhall
[dhall-prelude]: https://prelude.dhall-lang.org
[hcl]: https://github.com/hashicorp/hcl
[learn-dhall-in-y-minutes]: https://learnxinyminutes.com/docs/dhall/
[readme-before-nat-int-swap]: https://github.com/dhall-lang/dhall-lang/blob/1b74481c87b3ed83ecd613420c11de92335652a3/README.md
[migration-nat-int-swap]: https://github.com/dhall-lang/dhall-lang/wiki/Migration%3A-Swapped-syntax-for-Natural-numbers-and-Integers
[issue-tracker]: https://github.com/dhall-lang/dhall-lang/issues
[core-language-features]: https://github.com/dhall-lang/dhall-lang/wiki/Core-language-features
[dhall-json-tutorial-wiki]: https://github.com/dhall-lang/dhall-lang/wiki/Getting-started%3A-Generate-JSON-or-YAML

## Contributors

### Code Contributors

This project exists thanks to all the people who contribute. [[Contribute](.github/CONTRIBUTING.md)].
<a href="https://github.com/dhall-lang/dhall-lang/graphs/contributors"><img src="https://opencollective.com/dhall/contributors.svg?width=890&button=false" /></a>

### Financial Contributors

Become a financial contributor and help us sustain our community. [[Contribute](https://opencollective.com/dhall/contribute)]

#### Individuals

<a href="https://opencollective.com/dhall"><img src="https://opencollective.com/dhall/individuals.svg?width=890"></a>

#### Organizations

Support this project with your organization. Your logo will show up here with a link to your website. [[Contribute](https://opencollective.com/dhall/contribute)]

<a href="https://opencollective.com/dhall/organization/0/website"><img src="https://opencollective.com/dhall/organization/0/avatar.svg"></a>
<a href="https://opencollective.com/dhall/organization/1/website"><img src="https://opencollective.com/dhall/organization/1/avatar.svg"></a>
<a href="https://opencollective.com/dhall/organization/2/website"><img src="https://opencollective.com/dhall/organization/2/avatar.svg"></a>
<a href="https://opencollective.com/dhall/organization/3/website"><img src="https://opencollective.com/dhall/organization/3/avatar.svg"></a>
<a href="https://opencollective.com/dhall/organization/4/website"><img src="https://opencollective.com/dhall/organization/4/avatar.svg"></a>
<a href="https://opencollective.com/dhall/organization/5/website"><img src="https://opencollective.com/dhall/organization/5/avatar.svg"></a>
<a href="https://opencollective.com/dhall/organization/6/website"><img src="https://opencollective.com/dhall/organization/6/avatar.svg"></a>
<a href="https://opencollective.com/dhall/organization/7/website"><img src="https://opencollective.com/dhall/organization/7/avatar.svg"></a>
<a href="https://opencollective.com/dhall/organization/8/website"><img src="https://opencollective.com/dhall/organization/8/avatar.svg"></a>
<a href="https://opencollective.com/dhall/organization/9/website"><img src="https://opencollective.com/dhall/organization/9/avatar.svg"></a>
