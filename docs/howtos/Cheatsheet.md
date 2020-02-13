# Cheatsheet

> Ramp up quickly with a cheat sheet if you're already familiar with functional programming

## Command line

*   Feed Dhall expressions to `dhall`'s standard input to type-check and evaluate them:

    ```console
    $ dhall <<< 'True && False'
    ```
    ```dhall
    Bool

    False
    ```

*   Add the `--explain` flag for detailed explanations of type errors.

## Primitive types

*   `Bool`:

    ```dhall
    True, False : Bool

    True || False = True

    True && False = False

    True == False = False

    True != False = True

    if True then 1 else 2 = 1
    ```

*   `Natural`:

    A non-negative number (unsigned):

    ```dhall
    0, 1, 2, … : Natural

    0x0, 0x1, …, 0xE, 0xF, 0x10, … : Natural -- Hexadecimal notation

    2 + 3 = 5

    2 * 3 = 6

    Natural/build (λ(natural : Type) → λ(succ : natural → natural) → λ(zero : natural) → succ (succ (succ (succ zero)))) = 4

    Natural/fold 10 Text (λ(t : Text) → t ++ "!") "Hello" = "Hello!!!!!!!!!!"

    Natural/isZero 2 = False

    Natural/even 2 = True

    Natural/odd 2 = False

    Natural/subtract 1 3 = 2

    Natural/show 2 = "2"
    ```

*   `Integer`:

    An integer, prefixed with a `+` or `-` sign:

    ```dhall
    …, -2, -1, +0, +1, +2, … : Integer

    …, -0x10, -0xF, -0xE, …, 0xE, 0xF, 0x10, … : Integer  -- Hexadecimal notation

    Integer/negate +2 = -2

    Integer/clamp +2 = 2

    Integer/clamp -3 = 0

    Integer/toDouble -3 = -3.0

    Integer/show +2 = "+2"
    ```

*   `Double`:

    A double-precision floating point number with optional scientific notation:

    ```dhall
    -2.0, 3.14159, 1e10 : Double

    Double/show 2.0 = "2.0"
    ```

*   `Text`:

    ```dhall
    "", "Hello, world!", "☺", "  \u03bb(x : Type)\n\u2192 x" : Text

    ''
      Multi-line
      string
    '' = "Multi-line\nstring\n"

    "Interpolation: ${Natural/show 2}" = "Interpolation: 2"

    ''
      ''${escaped interpolation}
    '' = "\${escaped interpolation}\n"

    "ABC" ++ "DEF" = "ABCDEF"

    Text/show "Hello, world!" = "\"Hello, world!\""
    ```

## Complex types

*   `List`:

    A collection of 0 or more elements of the same type

    Type annotation is mandatory for empty lists:

    ```dhall
    [] : List Natural, [ 1, 2, 3 ]

    [ 1, 2, 3 ] # [ 4, 5, 6 ] = [ 1, 2, 3, 4, 5, 6 ]

    List/build Natural (λ(list : Type) → λ(cons : Natural → list → list) → λ(nil : list) → cons 1 (cons 2 (cons 3 nil))) = [ 1, 2, 3 ] : List Natural

    List/fold Bool [ True, False, True ] Natural (λ(x : Bool) → λ(y : Natural) → if x then y + 1 else y) 0 = 2

    List/length Natural [ 2, 3, 5 ] = 3

    List/head Natural [ 2, 3, 5 ] = Some 2

    List/last Natural [ 2, 3, 5 ] = Some 5

    List/indexed Natural [ 2, 3, 5 ] = [ { index = 0, value = 2 }, { index = 1, value = 3 }, { index = 2, value = 5 } ]

    List/reverse Natural [ 2, 3, 5 ] = [ 5, 3, 2 ]
    ```

*   `Optional`:

    ```dhall
    None Natural, Some 1 : Optional Natural


    Optional/fold Natural (Some 2) Text Natural/show "" = "2"

    Optional/build Natural (λ(optional : Type) → λ(just : Natural → optional) → λ(nothing : optional) → just 1) = Some 1
    ```

*   Records

    A mapping from field names to values that can be different types

    ```dhall
    {=} : {}  -- Empty record value requires an `=` to distinguish it from empty record type

    { foo = 1, bar = Bool } : { foo : Natural, bar : Type }

    { foo = 1, bar = Bool }.foo = 1

    { foo = 1, bar = Bool }.{ foo } = { foo = 1 }

    { foo = 1, bar = Bool }.({ foo : Natural }) = { foo = 1 }

    { foo = { bar = Bool } } ∧ { foo = { baz = "Hi" } } = { foo = { bar = Bool, baz = "Hi" } }

    { foo : { bar : Type } } ⩓ { foo : { baz : Text } } = { foo : { bar : Type, baz : Text } }

    { foo = 1, bar = Bool } ⫽ { bar = "Hi" } = { foo = 1, bar = "Hi" }

    { foo.bar = 1 } = { foo = { bar = 1 } }

    { foo.bar = 1, foo.baz = 2 } = { foo = { bar = 1, baz = 2 } }

    toMap { foo = 1, bar = 2 }
    = [ { mapKey = "foo", mapValue = 1 }
      , { mapKey = "bar", mapValue = 2 }
      ]

    -- Record completion
    let Example = { Type = { foo : Natural, bar : Bool }, default = { bar = False } }
    in  Example::{ foo = 1 }
    = { foo = 1, bar = False }
    ```

*   Unions

    ```dhall
    let FooBar = < Foo | Bar : Natural >

    let isEven =
        \(foobar : FooBar) -> merge { Foo = False, Bar = Natural/even } foobar

    in  [ isEven FooBar.Foo, isEven (FooBar.Bar 3), isEven (FooBar.Bar 4) ]
    = [ False, False, True ]
    ```

## Programming

*   `let` expressions:

    ```dhall
    let x = True

    let y = False

    in  x && y
    ```

    You can also use `let` expressions to name functions and imported values:

    ```dhall
    let not = λ(x : Bool) → x == False

    let show = https://prelude.dhall-lang.org/Bool/show

    in  show (not False)
    ```

*   Anonymous functions

    The type of a function's input argument is required and not inferred:

    ```dhall
    \(inputArgument : inputType) -> outputResult : forall (inputArgument : inputType) -> outputType  -- ASCII syntax

    λ(inputArgument : inputType) → outputResult : ∀(inputArgument : inputType) → outputType  -- Unicode syntax

    let describe =
        λ(name : Text)
      → λ(age : Natural)
      → "Name: ${name}, Age: ${Natural/show age}"

    in  describe "John Doe" 21 = "Name: John Doe, Age: 21"
    ```

*   Polymorphism

    Type abstraction and type application are explicit:

    ```dhall
    let id = λ(a : Type) → λ(x : a) → x in id Natural 4 = 4
    ```

*   Assertions

    ```dhall
    let example0 = assert : Natural/even 2 === True

    let example1 = assert : Natural/even 2 ≡ True

    in  "whatever"
    ```

*   Imports

    Imported paths or URLs are substituted for their contents:

    ```dhall
    [ ./you/can/import/paths, https://example.com/you/can/import/urls ] : ./even/for/types
    ```

    Adding `as Text` imports the contents of the import as a `Text` value instead of a
    Dhall expression:

    ```console
    $ dhall <<< 'https://prelude.dhall-lang.org/Bool/not'
    ```
    ```dhall
    λ(b : Bool) → b == False
    ```
    ```console
    $ dhall <<< 'https://prelude.dhall-lang.org/Bool/not as Text'
    ```
    ```dhall
    ''
    {-
    Flip the value of a `Bool`

    Examples:

    ./not True = False

    ./not False = True
    -}
    let not : Bool → Bool = λ(b : Bool) → b == False in not
    ''
    ```

    You can specify a fallback expression if the import fails using `?`

    This fallback expression can contain another import:

    ```dhall
    https://prelude.dhall-lang.org/package.dhall ? ./Prelude/package.dhall
    ```

    ... or even be a pure value:

    ```dhall
    Some (env:HOME as Text) ? None Text
    ```

*   Prelude

    You can find latest Prelude of importable functions at 
    [prelude.dhall-lang.org](https://prelude.dhall-lang.org/)

    You can import Prelude as a whole with:

    ```dhall
    let Prelude = https://prelude.dhall-lang.org/package.dhall

    in Prelude.List.last Natural [2, 3, 4]
    ```

*   Comments

    Dhall has single-line and block comments (with a syntax borrowed from Haskell):

    ```dhall
    -- single-line comment

    True -- Line comments may follow code on the same line

    {- block
       comment -}

    {- block comments can be {- nested -} -}

    let x {- block comments can appear inside of expressions like whitespace -} = 1 in x
    ```
