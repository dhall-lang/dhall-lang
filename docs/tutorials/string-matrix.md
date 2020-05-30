# Using map and fold to generate a list of strings

The goal of this tutorial is to generate a YAML list of test names, such as:

```yaml
- test-variantA-suffixA
- test-variantA-suffixB
- test-variantB-suffixA
- test-variantB-suffixB
```

We'll use the `dhall` and `dhall-to-yaml` command line,
make sure to follow the [install instructions][installation] from the getting started tutorial.

[installation]: <tutorials/Getting-started_Generate-JSON-or-YAML:installation>

## Map

We would like to begin by prepending the same prefix to a list of suffixes, so that our final code looks like this:

```dhall
generate-test-name "variantA" [ "suffixA", "suffixB" ]

= [ "variantA-suffixA", "variantA-suffixB" ]
```

... but in order to do so we need a way to apply the same transformation (e.g. append a suffix) to each element of a list.

The ["map"](https://en.wikipedia.org/wiki/Map_%28higher-order_function%29) higher-order function provides
precisely this capability: we can use map to uniformly transform each element of a list, like this:

```dhall
-- ./string-matrix.dhall

let List/map =
      https://prelude.dhall-lang.org/v11.1.0/List/map sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680

let generate-test-name =
      \(prefix : Text) ->
        List/map Text Text (\(suffix : Text) -> "${prefix}-${suffix}")

in  generate-test-name "variantA" [ "suffixA", "suffixB" ]
```
```console
$ dhall --file ./string-matrix.dhall
[ "variantA-suffixA", "variantA-suffixB" ]

$ dhall-to-yaml --file ./string-matrix.dhall
- variantA-suffixA
- variantA-suffixB
```

In dhall, generic functions such as `map` require types to be explicitly specified as function arguments. The `map` signature is:

```dhall
let map : forall (a : Type) -> forall (b : Type) -> (a -> b) -> List a -> List b
```

In other words, the `map` arguments are:

* input type (`a`),
* output type (`b`),
* a function that goes from `a` to `b`,
* a list of `a`, and
* it returns a list of `b`.

Functions can be partially saturated as each argument evaluates to a new function.
This technique is called [currying](https://en.wikipedia.org/wiki/Currying).
For example, the type of the `List/map` function changes with each new argument:

```dhall
List/map
    : forall (a : Type) -> forall (b : Type) -> (a -> b) -> List a -> List b

List/map Text
    : forall (b : Type) -> (Text -> b) -> List Text -> List b

List/map Text Text
    : (Text -> Text) -> List Text -> List Text

List/map Text Text (\(prefix : Text) -> "${prefix}-${suffix}")
    : List Text -> List Text
```

Thus, `generate-test-name` pre-applies the `map` function from `Text` to `Text` using a known prefix.
Then it is given a list of suffixes to generate new strings.

## Nested map

We would like to prepend a list of prefixes to a list of suffixes, so that our final code looks like this:

```dhall
generate-test-suite [ "variantA", "variantB" ] [ "suffixA", "suffixB" ]

= [ "variantA-suffixA", "variantA-suffixB", "variantB-suffixA", "variantB-suffixB" ]
```

To do so we can call two `map`s and generate our initial goal by adding the following `generate-test-suite`
function to our `./string-matrix.dhall` file:

```dhall
-- ./string-matrix.dhall

let List/map =
      https://prelude.dhall-lang.org/v11.1.0/List/map sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680

let generate-test-name =
      \(prefix : Text) ->
        List/map Text Text (\(suffix : Text) -> "${prefix}-${suffix}")

let generate-test-suite =
      \(prefixes : List Text) ->
      \(suffixes : List Text) ->
        List/map
          Text
          (List Text)
          (\(prefix : Text) -> generate-test-name prefix suffixes)
          prefixes

in  generate-test-suite [ "variantA", "variantB" ] [ "suffixA", "suffixB" ]
```
```console
$ dhall --file ./string-matrix.dhall
[ [ "variantA-suffixA", "variantA-suffixB" ]
, [ "variantB-suffixA", "variantB-suffixB" ]
]

$ dhall-to-yaml --file ./string-matrix.dhall
- - variantA-suffixA
  - variantA-suffixB
- - variantB-suffixA
  - variantB-suffixB
```

However, by doing so we get a value of type `List (List Text)` (i.e. a `List` of `List`s of `Text`).
So we can get a flat list by using the fold function.

## Folding a list of list of texts to a flat list of text

We would like the `generate-test-suite` function to produce a flat list of test names.
To do so we need a way to combine each element of the list.

The ["fold"](https://en.wikipedia.org/wiki/Fold_%28higher-order_function%29) higher-order function
provides precisely this capability: we can use fold to reduce a list of objects to another type. Its signature is:

```dhall
let fold
    :   forall (a : Type)
      → List a
      → forall (list : Type)
      → forall (cons : a → list → list)
      → forall (nil : list)
      → list
```

In other words, the `fold` arguments are:

* input type (`a`),
* the list to fold (a List of `a`),
* output type (`b`),
* a folding function that goes from (`a`, `b`) to `b`,
* a starting value of type `b`, and
* it returns a `b`.

Therefore, we can implement a `flatten-list-text` function like so:

```dhall
-- ./concat-example.dhall
let {- Convert [[a, b], [c, d]] to [a, b, c, d] -}
    flatten-list-text =
      \(to-fold : List (List Text)) ->
          List/fold
            (List Text)
            to-fold
            (List Text)
            (\(a : List Text) -> \(b : List Text) -> a # b)
            ([] : List Text)
        : List Text

let example =
      \(a : Text) ->
      \(b : Text) ->
      \(c : Text) ->
      \(d : Text) ->
        assert : flatten-list-text [ [ a, b ], [ c, d ] ] === [ a, b, c, d ]

in  flatten-list-text [ [ "A", "B" ], [ "C", "D" ] ]
```

```console
$ dhall-to-yaml --file ./concat-example.dhall
- A
- B
- C
- D
```

> **NOTE**: This function can also be defined as:
> ```dhall
> let flatten-list-text = https://prelude.dhall-lang.org/List/concat Text
>

We can use this new function in our previous example:
```dhall
in flatten-list-text
     (generate-test-suite [ "suffixA", "suffixB" ] [ "variantA", "variantB" ])
```
```yaml
- variantA-suffixA
- variantA-suffixB
- variantB-suffixA
- variantB-suffixB
```

## Refactoring the generate-test-suite function

We would like the `generate-test-suite` function to return the correct type instead of
having to use `flatten-list-text` manually.

The `flatten-list-text` function doesn't have to be a top-level function and the
`generate-test-suite` could automatically fold the result.
Let's rewrite the above example using nested `let`:

```dhall
-- ./string-matrix.dhall
let List/map =
      https://prelude.dhall-lang.org/v11.1.0/List/map sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680

let generate-test-name =
      \(prefix : Text) ->
        List/map Text Text (\(suffix : Text) -> "${prefix}-${suffix}")

let generate-test-suite =
      \(prefixes : List Text) ->
      \(suffixes : List Text) ->
        let flatten-list-text = https://prelude.dhall-lang.org/List/concat Text

        in  flatten-list-text
              ( List/map
                  Text
                  (List Text)
                  (\(prefix : Text) -> generate-test-name prefix suffixes)
                  prefixes
              )

in  generate-test-suite [ "variantA", "variantB" ] [ "suffixA", "suffixB" ]
```

This is equivalent to the previous evaluation, but the flatten-list-text function is now hidden in
the generate-test-suite thanks to a nested `let`.

We could use the new function to generate even more variations:

```dhall
in  generate-test-suite
      ( generate-test-suite
          (generate-test-suite [ "tox" ] [ "py27", "py38" ])
          [ "postgresql", "sqlite" ]
      )
      [ "el6", "el7" ]
```
```yaml
- tox-py27-postgresql-el6
- tox-py27-postgresql-el7
- tox-py27-sqlite-el6
- tox-py27-sqlite-el7
- tox-py38-postgresql-el6
- tox-py38-postgresql-el7
- tox-py38-sqlite-el6
- tox-py38-sqlite-el7
```

## Generalizing the string matrix generation

While the above is working, the code can be improved to remove repetitive function calls.
We would like our final code to look like this:

```dhall
generate-test-matrix [ [ "tox" ], ["py27", "py38"], ["postgresql", "sqlite"] ]

= [ "tox-py27-postgresql", "tox-py38-postgresql", "tox-py27-sqlite", ...]
```

... but in order to do so we need to generalize our function.

The [fold](https://prelude.dhall-lang.org/List/fold) documentation actually mentions:

> If you treat the list `[ x, y, z ]` as `cons x (cons y (cons z nil))`, then a
> `fold` just replaces each `cons` and `nil` with something else.

This is exactly what we need, `cons` is our `generate-test-suite` and `nil` is
the initial suffix `[ "el6", "el7" ]`!

```dhall
-- ./string-matrix.dhall
let List/map =
      https://prelude.dhall-lang.org/v11.1.0/List/map sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680

let generate-test-name =
      \(prefix : Text) ->
        List/map Text Text (\(suffix : Text) -> "${prefix}-${suffix}")

let generate-test-suite =
      \(prefixes : List Text) ->
      \(suffixes : List Text) ->
        let flatten-list-text = https://prelude.dhall-lang.org/List/concat Text

        in  flatten-list-text
              ( List/map
                  Text
                  (List Text)
                  (\(prefix : Text) -> generate-test-name prefix suffixes)
                  prefixes
              )

let generate-test-matrix =
      \(variations : List (List Text)) ->
      \(last : List Text) ->
        List/fold (List Text) variations (List Text) generate-test-suite last

in  generate-test-matrix
      [ [ "tox" ], [ "py27", "py38" ], [ "postgresql", "sqlite" ] ]
      [ "el6", "el7" ]
```
```yaml
- tox-py27-postgresql-el6
- tox-py38-postgresql-el6
- tox-py27-sqlite-el6
- tox-py38-sqlite-el6
- tox-py27-postgresql-el7
- tox-py38-postgresql-el7
- tox-py27-sqlite-el7
- tox-py38-sqlite-el7
```

> **Note**: Using the `List/head` function the `generate-test-matrix` could
> be improved to take a single list of argument.
