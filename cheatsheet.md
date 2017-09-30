---
title: Cheatsheet of the dhall configuration language
date: 2017-09-03
authors: Profpatsch
---

Pipe file to `dhall` to typecheck/evaluate. 
`dhall --explain` for extensive error explanations.

# Syntax

* `value : type`
* here we use `,` to separate multiple values, which is not valid dhall
* `-- this is a dhall comment`
* multi line comment
```
{- this is multiline
   comment example -}
```
* let-syntax
  
  ```
  let
    a = …
  in let
    b = … 
  in …`
  ```

  (also for function aliases and import names)
  
## Scalars

```
True, False : Bool
+0, +1, +2, +3 : Natural
-2, -1, 0, 1, 2 : Integer
-2.0, 3.13158 : Double
"hello world" : Text
```

### Text

1. normal double-quoted strings, Haskell style escaping (`"\"abc\"\ndef"`)
2. nix-style interpolated, multi-line strings
    * newline stripping

      ```
      ''
        foo
        bar
      ''
      ```

      is `"\nfoo\nbar\n"`

    * no newlines & escaping `''`

      ```
        ''foo
      '''x
      bar''
      ```

      is `"foo\n''x\nbar"`

    * interpolation

      ```
      let template = 
            \(name : Text) ->
            \(age : Integer) ->
              ''${name} is ${Integer/show age} years old''
      in template "bilbo" 24
      ```

      outputs `"bilbo is 24 years old"`


## Complex Types

### List

`[1,2,3] : List Integer`

* empty lists must be annotated `[] : List Integer`
* items have the same type <s>`[1, "Text"]`</s> (otherwise use Unions)

### Optional

`["element"], [] : Optional Text`

* like a list, but only 0 or 1 elements (read: an optional element)
* <s>`[1, 2] : Optional Integer`</s>

### Record

```
{ foo = True
, bar = 2 }
: { foo : Bool
  , bar : Integer }
```

* empty record: `{=} : {}`
* access with `.`: `{ a = False }.a == False`

### Union

```
let 
  a : < A : Text | B : Natural | Foo : Text >
      = < B = +42 | A : Text | Foo : Text >
in let
  b : < A : Text | B : Natural | Foo : Text >
      = < Foo = "hello" | A : Text | B : Natural >
in let
  handlers =
    { A   = \(a : Text)    -> False
    , B   = \(i : Natural) -> Natural/even i
    , Foo = \(t : Text)    -> True && False }
in
  (merge handlers a : Bool)
    == True
```

* tagged unions
* attention: the `=`-part has to come first for values
* merge
  * builtin that matches on the tag
  * needs to produce the same type for each tag
  * always requires an annotation of the output type
  
# Computing

## Functions

```
let f = \(firstArgument : Text) ->
        \(secondArgument : Integer) ->
        "some text ${firstArgument} and int ${secondArgument}"
in f "my text" 5
```

* types of input arguments are required (not inferred)

## Guarantees of Dhall Evaluation

* non-turing-complete: all evaluations of well-typed dhall code terminate (eventually)
* total: nothing well-typed will ever crash or throw exceptions
* everything will be evaluated as much as possible
  * `\(x : t) -> +10 * +10` becomes `\(x : t) -> +100`

## Polymorphism

```
let
  const = \(t1 : Type) ->
          \(x : t1) ->
          \(t2 : Type) ->
          \(_ : t2) ->
            x
in let
  id = \(t : Type) ->
       \(x : t) ->
         x
in 
  const Bool False Text "text"
    == id Bool False
```

* specification of type variables happens explicitely as arguments

# Misc

* Imports: Paths are substituted by their contents
  * `./func True 103`
  * ` { foo = "abc", bar = 1 } : https://url/to/type` even
* Alternative Unicode-Syntax for Functions: `λ(x : t) → …`
* Prelude at https://ipfs.io/ipfs/QmQ8w5PLcsNz56dMvRtq54vbuPe9cNnCCUXAQp6xLc6Ccx/Prelude/
