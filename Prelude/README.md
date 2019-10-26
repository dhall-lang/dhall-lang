# Prelude

This package contains useful utilities for getting started with the Dhall
configuration language.  The Prelude is unique in that changes are approved by
the same process as changes to the language standard.

## Usage

The Prelude is hosted at:

* [https://prelude.dhall-lang.org/](https://prelude.dhall-lang.org)

... which you can use in several ways:

* To browse the latest release of the Prelude package, you can visit:

  [https://prelude.dhall-lang.org/](https://prelude.dhall-lang.org)

* To import a specific expression add the path to that expression to the URL

  For example, the URL for the `./Bool/not` expression is:

  [https://prelude.dhall-lang.org/Bool/not](https://prelude.dhall-lang.org/Bool/not)

  Here is an example of using the [`./Bool/not`](./Bool/not)  expression:

  ```dhall
  let Bool/not = https://prelude.dhall-lang.org/Bool/not

  in  Bool/not True
  ```
  
* To import the entire Prelude as a nested record, use the following import
  within your Dhall code:

  [https://prelude.dhall-lang.org/package.dhall](https://prelude.dhall-lang.org/package.dhall)

  Here is an example of accessing the [`./Bool/not`](./Bool/not) expression as a
  field of that record:

  ```dhall
  let Prelude = https://prelude.dhall-lang.org/package.dhall

  in  Prelude.Bool.not True
  ```

* You can pin an expression to a specific version of the Prelude by prefixing the
  path with the Prelude version

  For example, you could use the following URL to import version `10.0.0` of the
  Prelude:

  [https://prelude.dhall-lang.org/v10.0.0/package.dhall](https://prelude.dhall-lang.org/v10.0.0/package.dhall)

  This also works for individual expressions, too, such as:

  [https://prelude.dhall-lang.org/v10.0.0/Bool/not](https://prelude.dhall-lang.org/v10.0.0/Bool/not)

  ... or for browsing a specific release of the Prelude:

  [https://prelude.dhall-lang.org/v10.0.0](https://prelude.dhall-lang.org/v10.0.0)

The `prelude.dhall-lang.org` domain is CORS-enabled so the Prelude can be used
as a transitive import of other packages without violating the Dhall language's
CORS check.

## Scope

There is no hard rule for what belongs in the Prelude, but we can document what
we have included so far:

* General purpose utilities (like [`./List/filter`](./List/filter))

* Re-exports of language built-ins (like [`./Natural/fold`](./Natural/fold))

* Integrations with other configuration formats (like [`./JSON/package.dhall`](./JSON/package.dhall))

The [contributing instructions](../.github/CONTRIBUTING.md#how-do-i-change-the-language)
provide more details about how to discuss or propose changes if you would like to contribute to the Prelude.
