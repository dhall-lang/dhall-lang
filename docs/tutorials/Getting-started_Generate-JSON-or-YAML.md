# Getting started: Generate JSON or YAML

> Use Dhall to simplify large and repetitive JSON or YAML configuration files

```eval_rst
.. contents:: Table of Contents
   :backlinks: none
```

This tutorial teaches you how to:

* install the `dhall-to-json` and `dhall-to-yaml` executables
* author a Dhall configuration
* generate JSON or YAML from the Dhall configuration

This tutorial does not cover all of the features of the Dhall configuration
language.  The goal of this tutorial is to walk through basic tricks to simplify
repetitive JSON configuration files.

This tutorial assumes that you are comfortable with the command line.  If not,
then read this introduction to the command line:

* [Learn Enough Command Line to Be Dangerous][command-line]

This tutorial also assumes that you understand basic programming concepts like
records and functions.

## Installation

> **NOTE**: This tutorial assumes that you are using version 1.2.5 or later
> of the `dhall-json` package.  Some of the following examples will not
> work correctly for older versions because they rely on newer language features.

You will need to install the `dhall-json` package, which provides both the
`dhall-to-json` and `dhall-to-yaml` executables.  You can download and install
prebuilt executables for Windows, OS X, and Linux by following the instructions
below for each platform.

### Windows

Install [Git for Windows](http://gitforwindows.org/), which
includes Git Bash: a command line environment complete with Unix utilities.
Run the following commands within a Git Bash shell.

To install the latest stable release, visit the release page here:

* [Releases - dhall-lang/dhall-haskell](https://github.com/dhall-lang/dhall-haskell/releases)

... and download `dhall-json-X.Y.Z-x86_64-windows.zip` replacing `X.Y.Z` with
the latest available version of `dhall-json`.

Navigate to the directory where you downloaded the ZIP file and unzip the file
by running:

```console
$ unzip dhall-json-*-x86_64-windows.zip
```

That will produce the following two files:

* `./dhall-to-json.exe`
* `./dhall-to-yaml.exe`

Run the following commands (no `.exe` suffix necessary) to verify that
the executables work:

```console
$ ./dhall-to-json --help
$ ./dhall-to-yaml --help
```

... and then copy those executables to `~/bin`:

```
$ cp ./dhall-to-{json,yaml} ~/bin
```

... and then run the following command to verify that the two
executables are on your executable search `PATH`:

```console
$ dhall-to-json --help
$ dhall-to-yaml --help
```

### OS X

You can either use `brew`:

```console
$ brew install dhall-json
```

... or you can install the latest stable release by visiting the release page here:

* [Releases - dhall-lang/dhall-haskell](https://github.com/dhall-lang/dhall-haskell/releases)

... and download `dhall-json-X.Y.Z-x86_64-macos.tar.bz2` replacing `X.Y.Z` with
the latest available version of `dhall-json`.  Then navigate to the directory where you
downloaded the archive and run:

```console
$ tar --extract --bzip2 --file dhall-json-*-x86_64-macos.tar.bz2
```

That should create a `./bin` subdirectory underneath your current directory
containing two executables:

```console
$ ls ./bin
dhall-to-json  dhall-to-yaml
```

Run the following commands to verify that the executables work:

```console
$ ./bin/dhall-to-json --help
$ ./bin/dhall-to-yaml --help
```

... and then copy those executables to `/usr/local/bin`:

```console
$ cp ./bin/dhall-to-{json,yaml} /usr/local/bin
```

Finally, run the following command to verify that the two
executables are on your executable search `PATH`:

```console
$ dhall-to-json --help
$ dhall-to-yaml --help
```

### Linux

To install the latest stable release, visit the release page here:

* [Releases - dhall-lang/dhall-haskell](https://github.com/dhall-lang/dhall-haskell/releases)

... and download `dhall-json-X.Y.Z-x86_64-linux.tar.bz2` replacing `X.Y.Z` with
the latest available version of `dhall-json`.  Then navigate to the directory where you
downloaded the archive and run:

```console
$ tar --extract --bzip2 --file dhall-json-*-x86_64-linux.tar.bz2
```

That should create a `./bin` subdirectory underneath your current directory
containing two executables:

```console
$ ls ./bin
dhall-to-json  dhall-to-yaml
```

Run the following commands to verify that the executables work:

```console
$ ./bin/dhall-to-json --help
$ ./bin/dhall-to-yaml --help
```

... and then copy those executables to `/usr/local/bin`:

```console
$ cp ./bin/dhall-to-{json,yaml} /usr/local/bin
```

... and then run the following command to verify that the two
executables are on your executable search `PATH`:

```console
$ dhall-to-json --help
$ dhall-to-yaml --help
```

## Smoke test

Now you can test drive generating JSON or YAML from a Dhall expression.

The `dhall-to-json` executable takes a Dhall program on standard input and emits
JSON to standard output.

> **Exercise:** Try to guess what either of the following commands will output:
>
> ```console
> $ dhall-to-yaml <<< '{ foo = [1, 2, 3], bar = True }'
> $ dhall-to-json <<< '{ foo = [1, 2, 3], bar = True }'
> ```
>
> ... then run the commands to test your guess.

> **Note:** `<<<` is a Bash operator that feeds the string on the right as
> standard input to the command on the left.  The above commands could have also
> been written as:
>
> ```console
> $ echo '{ foo = [1, 2, 3], bar = True }' | dhall-to-json
> $ echo '{ foo = [1, 2, 3], bar = True }' | dhall-to-yaml
> ```

To avoid repetition, we'll only use the `dhall-to-json` tool throughout the rest
of this tutorial, although all of the following examples work equally well for
the `dhall-to-yaml` tool.

> **Exercise:** What Dhall expression generates the following JSON:
>
> ```json
> [
>   {
>     "x": 1,
>     "y": "ABC"
>   }
> ]
> ```

## Imports

At some point our Dhall expressions will no longer fit on the command line.  We
can save large expressions to files and then reference them within other Dhall
expressions.

> **Exercise:** Save the following Dhall expression to a file named
> `example.dhall` in your current working directory:
>
> ```dhall
> { foo = True
> , bar = [1, 2, 3, 4, 5]
> , baz = "ABC"
> }
> ```
>
> What do you think the following command will output?
>
> ```console
> $ dhall-to-json <<< '[ ./example.dhall, ./example.dhall ]'
> ```
>
> Test your guess!

## Types

The Dhall configuration language is fairly similar to JSON if you ignore
Dhall's programming language features.  They both have records, lists, numbers,
strings, and boolean values.  However, Dhall is typed and rejects some
configurations that JSON would normally accept.

For example, the following command fails because Dhall requires lists to have
elements of the same type:

```console
$ dhall-to-json <<< '[ 1, True ]'

Error: List elements should all have the same type

- Natural
+ Bool

1│      True

(stdin):1:6
```

The error messages are terse by default, but if you check the `--help` output
you can see that the executable accepts an `--explain` flag:

```console
Usage: dhall-to-json ([--explain] ([--pretty] | [--compact]) ([--omit-empty] |
                     [--preserve-null]) ([--key ARG] [--value ARG] |
                     [--no-maps]) [--approximate-special-doubles] [--file FILE]
                     [--output FILE] | [--version])
  Compile Dhall to JSON

Available options:
  -h,--help                Show this help text
  --explain                Explain error messages in detail
  --pretty                 Deprecated, will be removed soon. Pretty print
                           generated JSON
  --compact                Render JSON on one line
  --omit-empty             Omit record fields that are null or empty records
  --preserve-null          Preserve record fields that are null
  --key ARG                Reserved key field name for association
                           lists (default: mapKey)
  --value ARG              Reserved value field name for association
                           lists (default: mapValue)
  --no-maps                Disable conversion of association lists to
                           homogeneous maps
  --approximate-special-doubles
                           Use approximate representation for NaN/±Infinity
  --file FILE              Read expression from a file instead of standard input
  --output FILE            Write JSON to a file instead of standard output
  --version                Display version
```

> **Exercise**: Add the `--explain` flag to the previous command:
>
> ```console
> $ dhall-to-json --explain <<< '[ 1, True ]'
> ```
>
> ... and read the full explanation for why the executable rejected the Dhall
> expression

Dhall also supports type annotations, which are the Dhall analog of a JSON
schema.  For example:

```console
$ dhall-to-json <<< '{ foo = 1, bar = True } : { foo : Natural, bar : Bool }'
{
  "bar": true,
  "foo": 1
}
```

Anything in Dhall can be imported from another file, including the type in a
type annotation.  This means that you can save the type annotation to a file:

```console
$ echo '{ foo : Natural, bar : Bool }' > schema.dhall
```

... and reference that file in a type annotation:

```console
$ dhall-to-json <<< '{ foo = 1, bar = True } : ./schema.dhall'
{
  "bar": true,
  "foo": 1
}
```

If the expression doesn't match the "schema" (i.e. the type annotation) then
"validation fails" (i.e. you get a type error):

```console
$ dhall-to-json <<< '{ foo = 1, baz = True } : ./schema.dhall'

Error: Expression doesn't match annotation

{ - bar : …
, + baz : …
, …
}

1│ { foo = 1, baz = True } : ./schema.dhall

(stdin):1:1
```

> **Exercise:** Add the `--explain` flag to the above command to see why the
> expression failed to validate against the schema.

## Variables

Dhall also differs from JSON by offering some programming language features.
For example, you can reduce repetition by using a `let` expression to define a
variable which can be referenced multiple times.

```console
$ dhall-to-json <<< 'let x = [1, 2, 3] in [x, x, x]'
```
```json
[
  [
    1,
    2,
    3
  ],
  [
    1,
    2,
    3
  ],
  [
    1,
    2,
    3
  ]
]
```

You can define multiple variables using multiple `let`s, like this:

```console
$ dhall-to-json <<< 'let x = 1 let y = [x, x] in [y, y]'
```
```json
[
  [
    1,
    1
  ],
  [
    1,
    1
  ]
]
```

The Dhall language is whitespace-insensitive (just like JSON), so this program:

```dhall
let x = 1 let y = 2 in [x, y]
```

... is the same as this program:

```dhall
let x = 1
let y = 2
in  [x, y]
```

> **Exercise:** Save the following Dhall configuration to `employees.dhall`:
>
> ```dhall
> let job = { department = "Data Platform", title = "Software Engineer" }
>
> let john = { age = 23, name = "John Doe", position = job }
>
> let alice = { age = 24, name = "Alice Smith", position = job }
>
> in  [ john, alice ]
> ```
>
> What do you think the following command will output:
>
> ```console
> $ dhall-to-json --file ./employees.dhall
> ```
>
> Test your guess!

> **Exercise:** This JSON is repetitive
>
> ```json
> [
>     {
>         "address": {
>             "state": "TX",
>             "street": "Main Street",
>             "city": "Austin",
>             "number": "9999"
>         },
>         "name": "John Doe"
>     },
>     {
>         "address": {
>             "state": "TX",
>             "street": "Main Street",
>             "city": "Austin",
>             "number": "9999"
>         },
>         "name": "Jane Doe"
>     },
>     {
>         "address": {
>             "state": "TX",
>             "street": "Main Street",
>             "city": "Austin",
>             "number": "9999"
>         },
>         "name": "Janet Doe"
>     }
> ]
> ```
>
> Try to use a less repetitive Dhall configuration file to generate the above
> JSON output.

## Functions

Dhall also lets you write anonymous functions of the form:

```dhall
\(inputName : inputType) -> output
```

... which you can also write using Unicode characters if you prefer:

```dhall
λ(inputName : inputType) → output
```

This tutorial will use the ASCII syntax for functions in the following
examples.  If you prefer the Unicode syntax you can learn how to input
Unicode on your computer by following these instructions:

* [Wikipedia - Unicode input](https://en.wikipedia.org/wiki/Unicode_input)

... and using the following Unicode code points for lambdas and arrows:

* `λ` (U+03BB)
* `→` (U+2192)

For example, here is an anonymous function that takes a single argument named
`x` of type `Natural` and returns a list of two `x`s:

```dhall
\(x : Natural) -> [x, x]
```

You can apply an anonymous function directly to an argument like this:

```console
$ dhall-to-json <<< '(\(x : Natural) -> [x, x]) 2'
```
```json
[
  2,
  2
]
```

More commonly, you'll use a `let` expression to give the function a name and
then use that name to apply the function to an argument:

```console
$ dhall-to-json <<< 'let twice = \(x : Natural) -> [x, x] in twice 2'
```
```json
[
  2,
  2
]
```

> **Exercise**: What JSON do you think this Dhall configuration file will
> generate?
>
> ```dhall
> let smallServer =
>       \(hostName : Text) ->
>         { cpus = 1
>         , gigabytesOfRAM = 1
>         , hostName = hostName
>         , terabytesOfDisk = 1
>         }
> 
> let mediumServer =
>       \(hostName : Text) ->
>         { cpus = 8
>         , gigabytesOfRAM = 16
>         , hostName = hostName
>         , terabytesOfDisk = 4
>         }
> 
> let largeServer =
>       \(hostName : Text) ->
>         { cpus = 64
>         , gigabytesOfRAM = 256
>         , hostName = hostName
>         , terabytesOfDisk = 16
>         }
> 
> in  [ smallServer "eu-west.example.com"
>     , largeServer "us-east.example.com"
>     , largeServer "ap-northeast.example.com"
>     , mediumServer "us-west.example.com"
>     , smallServer "sa-east.example.com"
>     , largeServer "ca-central.example.com"
>     ]
> ```
>
> Test your guess!

You can nest anonymous functions to create a function of multiple arguments:

```console
$ dhall-to-json <<< 'let both = \(x : Natural) -> \(y : Natural) -> [x, y] in both 1 2'
```
```json
[
  1,
  2
]
```

> **Exercise**: What JSON do you think this Dhall configuration file will
> generate?
>
> ```dhall
> let educationalBook =
>       \(publisher : Text) ->
>       \(title : Text) ->
>         { category = "Nonfiction"
>         , department = "Books"
>         , publisher = publisher
>         , title = title
>         }
> 
> let makeOreilly = educationalBook "O'Reilly Media"
> 
> in  [ makeOreilly "Microservices for Java Developers"
>     , educationalBook "Addison Wesley" "The Go Programming Language"
>     , makeOreilly "Parallel and Concurrent Programming in Haskell"
>     ]
> ```
>
> Test your guess!

## Combining records

Dhall provides the `/\` operator for merging two records, which you can also
represent using the Unicode `∧` character (U+2227).

For example:

```console
$ dhall-to-json <<< '{ foo = 1 } /\ { bar = 2}'
```
```json
{
  "bar": 2,
  "foo": 1
}
```

... is the same as:

```console
$ dhall-to-json <<< '{ foo = 1, bar = 2}'
```
```json
{
  "bar": 2,
  "foo": 1
}
```

We can rewrite our previous server configuration example to use this operator
instead of using functions:

```dhall
let smallServer = { cpus = 1, gigabytesOfRAM = 1, terabytesOfDisk = 1 }

let mediumServer = { cpus = 8, gigabytesOfRAM = 16, terabytesOfDisk = 4 }

let largeServer = { cpus = 64, gigabytesOfRAM = 256, terabytesOfDisk = 16 }

in  [ smallServer /\ { hostName = "eu-west.example.com" }
    , largeServer /\ { hostName = "us-east.example.com" }
    , largeServer /\ { hostName = "ap-northeast.example.com" }
    , mediumServer /\ { hostName = "us-west.example.com" }
    , smallServer /\ { hostName = "sa-east.example.com" }
    , largeServer /\ { hostName = "ca-central.example.com" }
    ]
```

> **Exercise:** Refactor the previous "educational books" example to also use the
> record merge operator instead of functions

## Operators

You can concatenate two strings using the `++` operator:

```console
$ dhall-to-json <<< '[ "ABC" ++ "DEF" ]'
```
```json
[
  "ABCDEF"
]
```

... and you can concatenate two lists using the `#` operator:

```console
$ dhall-to-json <<< '[1, 2, 3] # [4, 5, 6]'
```
```json
[
  1,
  2,
  3,
  4,
  5,
  6
]
```

> **Exercise:** What JSON do you think the following Dhall expression will
> generate?
>
> ```dhall
> let three = \(x : Text) -> [x ++ x ++ x] in three "A" # three "B" # three "C"
> ```
>
> Test your guess!

> **Exercise:** Write a non-repetitive Dhall expression that generates the
> following JSON:
>
> ```json
> {
>     "administrativeUsers": [
>         "alice",
>         "bob",
>         "carol"
>     ],
>     "ordinaryUsers": [
>         "alice",
>         "bob",
>         "carol",
>         "david",
>         "eve",
>         "frank"
>     ]
> }
> ```

## `Optional` values

Dhall's type system will reject the following common JSON idiom:

```console
$ dhall-to-json <<< '[ { x = 1 }, { x = 2, y = 3 } ]'

Error: List elements should all have the same type

{ + y : …
, …
}

1│              { x = 2, y = 3 }

(stdin):1:14
```

JSON configurations often have lists of records, where different records will
have different sets of fields defined.  Dhall rejects this because adding or
removing a field from a record changes the record's type.

Despite this restriction, we still have a few options for generating the above
JSON.  For example, you can make the `y` field `Optional` (i.e. the Dhall
equivalent of a nullable value), like this:

```dhall
-- ./optional.dhall

[ { x = 1, y = None Natural }
, { x = 2, y = Some 3 }
]
```

`Optional` values can either be present (i.e. `Some` followed by the value) or
absent (i.e. `None` followed by the type).

`dhall-to-json` by default omits `Optional` fields if they are empty (i.e.
`None`):

```console
$ dhall-to-json --file ./optional.dhall
```
```json
[
  {
    "x": 1
  },
  {
    "x": 2,
    "y": 3
  }
]
```

... but also provides a `--preserve-null` flag that you can use to represent
these fields as `null` if you prefer:

```console
$ dhall-to-json --preserve-null --file ./optional.dhall
```
```json
[
  {
    "x": 1,
    "y": null
  },
  {
    "x": 2,
    "y": 3
  }
]
```

## Unions

Sometimes JSON values might differ by more than just the presence or absence of
a record field.  For example, this is valid JSON, too:

```json
[1,true]
```

We would get a type error if we were to naively translate the above JSON to Dhall:

```console
$ dhall-to-json <<< '[ 1, True ]'

Error: List elements should all have the same type

- Natural
+ Bool

1│      True

(stdin):1:6
```

However, we can still generate such JSON if we take advantage of Dhall's support
for "unions".  You can think of a "union" as a value that can be one or more
possible types.

For example, the equivalent Dhall configuration would be:

```dhall
-- ./union.dhall

let Element = < Left : Natural | Right : Bool >

in  [ Element.Left 1, Element.Right True ]
```

Every union type has multiple possible alternatives, each labeled by a name and
a type.  For example, the union type named `Element` in the above Dhall
configuration has two alternatives:

* The first alternative is named `Left` and can store `Natural` numbers
* The second alternative is named `Right` and can store `Bool`s

The `dhall-to-json` executable strips the names when translating union literals
to JSON.  This trick lets you bridge between strongly typed Dhall configuration
files and their weakly typed JSON equivalents:

```console
$ dhall-to-json --file ./union.dhall
```
```json
[
  1,
  true
]
```

Here is a more sophisticated example showcasing how each union alternative
can be a record with different fields present:

```dhall
-- ./package.dhall

let Package =
      < Local : { relativePath : Text }
      | GitHub : { repository : Text, revision : Text }
      | Hackage : { package : Text, version : Text }
      >

in  [ Package.GitHub
        { repository =
            "https://github.com/Gabriel439/Haskell-Turtle-Library.git"
        , revision = "ae5edf227b515b34c1cb6c89d9c58ea0eece12d5"
        }
    , Package.Local { relativePath = "~/proj/optparse-applicative" }
    , Package.Local { relativePath = "~/proj/discrimination" }
    , Package.Hackage { package = "lens", version = "4.15.4" }
    , Package.GitHub
        { repository = "https://github.com/haskell/text.git"
        , revision = "ccbfabedea1cf5b38ff19f37549feaf01225e537"
        }
    , Package.Local { relativePath = "~/proj/servant-swagger" }
    , Package.Hackage { package = "aeson", version = "1.2.3.0" }
    ]
```

... which generates the following JSON:

```json
[
  {
    "repository": "https://github.com/Gabriel439/Haskell-Turtle-Library.git",
    "revision": "ae5edf227b515b34c1cb6c89d9c58ea0eece12d5"
  },
  {
    "relativePath": "~/proj/optparse-applicative"
  },
  {
    "relativePath": "~/proj/discrimination"
  },
  {
    "package": "lens",
    "version": "4.15.4"
  },
  {
    "repository": "https://github.com/haskell/text.git",
    "revision": "ccbfabedea1cf5b38ff19f37549feaf01225e537"
  },
  {
    "relativePath": "~/proj/servant-swagger"
  },
  {
    "package": "aeson",
    "version": "1.2.3.0"
  }
]
```

## Dynamic records

Some programs expect JSON records with a dynamically computed set of fields.  For
example, you might require a list of students to be represented by the following
sample JSON:

```json
{
    "aiden": {
        "age": 16
    },
    "daniel": {
        "age": 17
    },
    "rebecca": {
        "age": 17
    }
}
```

You can't use a Dhall record to store the above students because then the type of
the record would change every time you add or remove a student.

The idiomatic way to encode the above information in Dhall is to use an "association
list" (i.e. a list of key-value pairs), like this:

```dhall
-- ./students.dhall

[ { mapKey = "daniel", mapValue = { age = 17 } }
, { mapKey = "rebecca", mapValue = { age = 17 } }
, { mapKey = "aiden", mapValue = { age = 16 } }
]
```

... and the `dhall-to-json` executable automatically detects any association list
that uses the field names `mapKey` and `mapValue` and converts that to the
equivalent dynamic JSON record:

```console
$ dhall-to-json --file ./students.dhall
```
```json
{
  "aiden": {
    "age": 16
  },
  "daniel": {
    "age": 17
  },
  "rebecca": {
    "age": 17
  }
}
```

This ensures that  the schema of your Dhall configuration stays the same
but you can still generate JSON records with a dynamically computed set of
fields.

You have the option to disable this feature if you want using the `--noMaps`
flag:

```console
$ dhall-to-json --no-maps --file ./students.dhall
```
```json
[
  {
    "mapKey": "daniel",
    "mapValue": {
      "age": 17
    }
  },
  {
    "mapKey": "rebecca",
    "mapValue": {
      "age": 17
    }
  },
  {
    "mapKey": "aiden",
    "mapValue": {
      "age": 16
    }
  }
]
```

... or you can specify a different set of field names to reserve using the
`--key` and `--value` options if you don't want to reserve the names
`mapKey` and `mapValue`.

## YAML

You can translate all of the above examples to YAML instead of JSON using the
`dhall-to-yaml` executable.  For example:

```console
$ dhall-to-yaml <<< 'let x = 1 in let y = [x, x] in [y, y]'
```
```yaml
- 
  - 1
  - 1
- 
  - 1
  - 1
```

> **Exercise:** Translate one of the larger previous examples to YAML.

[command-line]: https://www.learnenough.com/command-line-tutorial
[wsl]: https://msdn.microsoft.com/en-us/commandline/wsl/install-win10
[nix]: https://nixos.org/nix/download.html
[high-sierra-bug]: https://github.com/NixOS/nix/issues/1583

## Conclusion

This concludes the tutorial on how to use the Dhall configuration language to
simplify repetitive JSON and YAML configurations.  By this point you should
understand how to some basic features of Dhall and you can learn more by reading
the [main language tutorial](./Language-Tour.md).
