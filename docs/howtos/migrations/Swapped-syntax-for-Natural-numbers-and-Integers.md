# Swapped syntax for Natural numbers and Integers

> Migrate your code to support the change in syntax for `Natural` numbers and `Integer`s

On May 7, 2018 the [language standard changed][change] to swap the syntax for `Natural` numbers and `Integer`s.  This section describes what changed and how to migrate your code.

## Changes

Before the change, the standard specified that:

* literal numbers with a leading `+` were `Natural` numbers,
* literal numbers without a leading sign were `Integer`s, and:
* literal numbers with a leading `-` were `Integer`s.

In other words:

```dhall
-- Before

+2 : Natural

 2 : Integer

-2 : Integer
```

After the change, the standard specifies that:

* literal numbers without a leading sign are `Natural` numbers, and:
* literal numbers with a leading sign (both `+` and `-`) are `Integer`s.

In other words:

```dhall
-- After

 2 : Natural

+2 : Integer

-2 : Integer
```

Additionally, `Natural/show` was changed to render `Natural` numbers without the leading sign and `Integer/show` was changed to render non-negative `Integer`s with a leading `+` sign:

```dhall
-- Before

Natural/show +2 = "+2"

Integer/show  2 =  "2"

Integer/show -2 = "-2"
```

```dhall
-- After

Natural/show  2 =  "2"

Integer/show +2 = "+2"

Integer/show -2 = "-2"
```

## Migration

To migrate code you need to:

* Remove the leading `+` from all `Natural` number literals
* Add a leading `+` to all non-negative `Integer` literals
* Replace `Natural/show n` with `"+${Natural/show n}"`
* Replace `Integer/show (Natural/toInteger n)` with `Natural/show n`

Also, you can no longer render `Integer`s without a leading `+` or `-` sign

[change]: https://github.com/dhall-lang/dhall-lang/pull/141
