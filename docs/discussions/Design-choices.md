# Design choices

> Read this if you're interested in the motivation behind various language design choices

This page centrally documents some commonly cited design decisions that come up
in various feature requests and language proposals.  However, note that these
decisions are not set in stone and we're open to changing our minds.  The
purpose of this page is not to bless any particular design decisions but rather
to make them easier to discover so that you don't learn about them late in the
language design process.

These decisions usually fall into one of the following categories:

* Artifacts of the language's minimalist origin

  The original author of the language (Gabriel Gonzalez) added very few starting
  features to the language, mainly because it's easier to add features than to
  take them away.  If you see a missing feature, maybe that's just because the
  language started out cautious and perhaps now's the time to shake things up.

* Promoting strongly-typed programming idioms

  Dhall promotes use of the type system to make "invalid states
  unrepresentable".  Not only do we add support for strongly typed programming
  idioms, but we also avoid adding support for language features which promote
  weakly typed idioms.  In other words, we strive to design the language so that
  best practices are the path of least resistance and preferably the only
  available path.

* Marketing

  In this context, "marketing" doesn't mean advertising, but rather means
  designing the language around a specific use case (i.e. market) in order to
  gain mainstream traction and adoption.

  Dhall's current "marketing" focuses on displacing YAML (especially in the
  context of ops-related tools) and some language features are tailored towards
  that use case.  Sometimes we'll turn down a feature request if it adds
  complexity to the language without getting us any closer to displacing YAML.

In addition to those broad rules of thumb, the following choices deserve
explicit mention.

## Text manipulation

The only text manipulation allowed is concatenation.  Other than that, `Text`
values are opaque and there is no way to introspect them.  For example, you
cannot compare `Text` values for equality, nor can you compute their
length.

The motivation behind this is to encourage the use of more structured
representations that catch errors at type-checking time instead of silently
failing at runtime .  For example, instead of this code:

```dhall
-- NOTE: This is not valid Dhall code

let isWeekDay
    : String -> Bool
    = \(d : String) ->
            d == "Monday"
        ||  d == "Tuesday"
        ||  d == "Wednesday"
        ||  d == "Thursday"
        ||  d == "Friday"

in  isWeekDay "thursday" -- Oops!
```

You would instead write this code:

```dhall
let DayOfWeek =
      < Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday >

let isWeekDay
    : DayOfWeek -> Bool
    = \(d : DayOfWeek) ->
        merge
          { Sunday = False
          , Monday = True
          , Tuesday = True
          , Wednesday = True
          , Thursday = True
          , Friday = True
          , Saturday = False
          }
          d

in  isWeekDay DayOfWeek.Thursday
```

## Operators

All of the operators in the language are associative, meaning that for
any operator (which we will denote `x`), the following property holds:

```dhall
(x + y) + z = x + (y + z)
```

... and every operator has an identity value (which we will denote `0`),
such that:

```dhall
x + 0 = x

0 + x = x
```

For example, the identity value for the `*` operator is `1`, the identity
value for the `∧` operator is `{=}`, and the identity value for `==` is
`True`.

This also implies that the type of each operator's result is the type of its
input arguments.  For example, this is why the `==` operator only works on
values of type `Bool` and does not work on values of type `Text` (nor any other
type).

## Arithmetic

You cannot perform arithmetic on `Double`s.  `Natural` numbers and `Integer`s
support addition, subtraction, and multiplication, because those operations are
well-defined for all possible inputs unlike division (due to the possibility of
division by zero).

As floating-point arithmetic is imprecise and prone to surprising results (e.g.
loss of associativity/distributivity, loss of accuracy due to cancellation,
difficulty testing for equality, etc.), `Double`s are opaque values as far as
the language is concerned, meaning that a Dhall configuration file can hold them
and shuffle them around but cannot manipulate them.

## No dictionaries/maps/hashes

This is a consequence of Dhall values (especially `Text`) not being
comparable so you cannot detect duplicate keys.  The closest Dhall
data type would be an association list of the following type:

```dhall
[ { mapKey : Text, mapValue : a } ]
```

In fact, tools like `dhall-to-json` will recognize values of this type and
convert them to maps in the target configuration format (such as JSON).

Dhall also doesn't support sets, for the same reason.
