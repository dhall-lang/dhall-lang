# β-normalization

β-normalization is a function of the following form:

    t₀ ⇥ t₁

... where:

* `t₀` (the input) is the expression to normalize
* `t₁` (the output) is the normalized expression

β-normalization evaluates all β-reducible expressions:

    (λ(x : Bool) → x == False) True ⇥ False

β-normalization evaluates all built-in functions if they are fully saturated
(i.e.  no missing arguments):

    List/length Natural [1, 2, 3] ⇥ 3

β-normalization does not simplify partially applied built-in functions:

    List/length Integer ⇥ List/length Integer

β-normalization works under λ, meaning that the body of an unapplied
λ-expression can be normalized:

    λ(x : Integer) → List/length Integer [x, x, x] ⇥ λ(x : Integer) → 3

Dhall is a total language that is strongly normalizing, so evaluation order has
no effect on the language semantics and a conforming implementation can select
any evaluation strategy.

Also, note that the semantics specifies several built-in types, functions and
operators that conforming implementations must support.  Implementations are
encouraged to implement the following functions and operators in more efficient
ways than the following reduction rules so long as the result of normalization
is the same.

## Table of contents

* [Constants](#constants)
* [Variables](#variables)
* [`Bool`](#bool)
* [`Natural`](#natural)
* [`Text`](#text)
* [`List`](#list)
* [`Optional`](#optional)
* [Records](#records)
* [Unions](#unions)
* [`Integer`](#integer)
* [`Double`](#double)
* [Functions](#functions)
* [`let` expressions](#let-expressions)
* [Type annotations](#type-annotations)
* [Imports](#imports)

## Constants

Type-checking constants are in normal form:


    ───────────
    Type ⇥ Type


    ───────────
    Kind ⇥ Kind


    ───────────
    Sort ⇥ Sort


## Variables

Variables are in normal form:


    ─────────
    x@n ⇥ x@n


## `Bool`


The `Bool` type is in normal form:


    ───────────
    Bool ⇥ Bool


The `Bool` constructors are in normal form:


    ───────────
    True ⇥ True


    ─────────────
    False ⇥ False


Simplify an `if` expression if the predicate normalizes to a `Bool` literal:


    t ⇥ True   l₀ ⇥ l₁
    ────────────────────────
    if t then l₀ else r ⇥ l₁


    t ⇥ False   r₀ ⇥ r₁
    ────────────────────────
    if t then l else r₀ ⇥ r₁


Also, simplify an `if` expression if the expression trivially returns the
predicate:


    l ⇥ True   r ⇥ False   t₀ ⇥ t₁
    ──────────────────────────────
    if t₀ then l else r ⇥ t₁


Simplify `if` expressions where both alternatives are the same:


    l₀ ⇥ l₁   r₀ ⇥ r₁   l₁ ≡ r₁
    ───────────────────────────
    if t then l₀ else r₀ ⇥ l₁


Otherwise, normalize the predicate and both branches of the `if` expression:


    t₀ ⇥ t₁   l₀ ⇥ l₁   r₀ ⇥ r₁
    ─────────────────────────────────────────────  ; If no other rule matches
    if t₀ then l₀ else r₀ ⇥ if t₁ then l₁ else r₁


Even though `True`, `False`, and `if` expressions suffice for all `Bool` logic,
Dhall also supports logical operators for convenience.

Simplify the logical "or" operator so long as at least one argument normalizes
to a `Bool` literal:


    l ⇥ False   r₀ ⇥ r₁
    ───────────────────
    l || r₀ ⇥ r₁


    r ⇥ False   l₀ ⇥ l₁
    ───────────────────
    l₀ || r ⇥ l₁


    l ⇥ True
    ─────────────
    l || r ⇥ True


    r ⇥ True
    ─────────────
    l || r ⇥ True


Normalize arguments that are equivalent


    l₀ ⇥ l₁   r₀ ⇥ r₁   l₁ ≡ r₁
    ───────────────────────────
    l₀ || r₀ ⇥ l₁


Otherwise, normalize each argument:


    l₀ ⇥ l₁   r₀ ⇥ r₁
    ───────────────────  ; If no other rule matches
    l₀ || r₀ ⇥ l₁ || r₁


Simplify the logical "and" operator so long as at least one argument normalizes
to a `Bool` literal:


    l ⇥ True   r₀ ⇥ r₁
    ──────────────────
    l && r₀ ⇥ r₁


    r ⇥ True   l₀ ⇥ l₁
    ──────────────────
    l₀ && r ⇥ l₁


    l ⇥ False
    ──────────────
    l && r ⇥ False


    r ⇥ False
    ──────────────
    l && r ⇥ False


Normalize arguments that are equivalent


    l₀ ⇥ l₁   r₀ ⇥ r₁   l₁ ≡ r₁
    ───────────────────────────
    l₀ && r₀ ⇥ l₁


Otherwise, normalize each argument:


    l₀ ⇥ l₁   r₀ ⇥ r₁
    ───────────────────  ; If no other rule matches
    l₀ && r₀ ⇥ l₁ && r₁


Simplify the logical "equal" operator if one argument normalizes to a `True`
literal:


    l ⇥ True   r₀ ⇥ r₁
    ──────────────────
    l == r₀ ⇥ r₁


    r ⇥ True   l₀ ⇥ l₁
    ──────────────────
    l₀ == r ⇥ l₁


... or if both arguments are equivalent:


    l₀ ⇥ l₁   r₀ ⇥ r₁   l₁ ≡ r₁
    ───────────────────────────
    l₀ == r₀ ⇥ True


Otherwise, normalize each argument:


    l₀ ⇥ l₁   r₀ ⇥ r₁
    ───────────────────  ; If no other rule matches
    l₀ == r₀ ⇥ l₁ == r₁


Simplify the logical "not equal" operator if one argument normalizes to a
`False` literal:


    l ⇥ False   r₀ ⇥ r₁
    ───────────────────
    l != r₀ ⇥ r₁


    r ⇥ False   l₀ ⇥ l₁
    ───────────────────
    l₀ != r ⇥ l₁


... or if both arguments are equivalent:


    l₀ ⇥ l₁   r₀ ⇥ r₁   l₁ ≡ r₁
    ───────────────────────────
    l₀ != r₀ ⇥ False


Otherwise, normalize each argument:


    l₀ ⇥ l₁   r₀ ⇥ r₁
    ───────────────────  ; If no other rule matches
    l₀ != r₀ ⇥ l₁ != r₁


## `Natural`

The `Natural` number type is in normal form:


    ─────────────────
    Natural ⇥ Natural


`Natural` number literals are in normal form:


    ─────
    n ⇥ n


`Natural/build` and `Natural/fold` are inverses of one another, which leads to
the following fusion rule:


    f ⇥ Natural/build   a ⇥ Natural/fold b
    ──────────────────────────────────────
    f a ⇥ b


Otherwise, fall back on each function's respective implementation.

`Natural/build` is the canonical introduction function for `Natural` numbers:


    f ⇥ Natural/build   g Natural (λ(x : Natural) → x + 1) 0 ⇥ b
    ────────────────────────────────────────────────────────────
    f g ⇥ b


`Natural/fold` function is the canonical elimination function for `Natural`
numbers:


    f ⇥ Natural/fold 0 B g   b ⇥ t₁
    ───────────────────────────────
    f b ⇥ t₁


    f ⇥ Natural/fold (1 + n) B g
    g (Natural/fold n B g b) ⇥ t₁
    ─────────────────────────────  ; "1 + n" means "a `Natural` literal greater
    f b ⇥ t₁                       ; than `0`"


Even though `Natural/fold` and `Natural/build` suffice for all `Natural` number
programming, Dhall also supports `Natural` number literals and built-in
functions and operators on `Natural` numbers, both for convenience and
efficiency.

Use machine addition to simplify the "plus" operator if both arguments normalize
to `Natural` literals:


    l ⇥ m   r ⇥ n
    ─────────────  ; "m + n" means "use machine addition"
    l + r ⇥ m + n


Also, simplify the "plus" operator if either argument normalizes to a `0`
literal:


    l ⇥ 0   r₀ ⇥ r₁
    ───────────────
    l + r₀ ⇥ r₁


    r ⇥ 0   l₀ ⇥ l₁
    ───────────────
    l₀ + r ⇥ l₁


Otherwise, normalize each argument:


    l₀ ⇥ l₁   r₀ ⇥ r₁
    ─────────────────  ; If no other rule matches
    l₀ + r₀ ⇥ l₁ + r₁


Use machine multiplication to simplify the "times" operator if both arguments
normalize to a `Natural` literal:


    l ⇥ m   r ⇥ n
    ─────────────  ; "m * n" means "use machine multiplication"
    l * r ⇥ m * n


Also, simplify the "times" operator if either argument normalizes to either a
`0` literal:


    l ⇥ 0
    ─────────
    l * r ⇥ 0


    r ⇥ 0
    ─────────
    l * r ⇥ 0



... or a `1` literal:


    l ⇥ 1   r₀ ⇥ r₁
    ───────────────
    l * r₀ ⇥ r₁


    r ⇥ 1   l₀ ⇥ l₁
    ───────────────
    l₀ * r ⇥ l₁


Otherwise, normalize each argument:


    l₀ ⇥ l₁   r₀ ⇥ r₁
    ─────────────────  ; If no other rule matches
    l₀ * r₀ ⇥ l₁ * r₁


`Natural/isZero` detects whether or not a `Natural` number is `0`:


    f ⇥ Natural/isZero   a ⇥ 0
    ───────────────────────────
    f a ⇥ True


    f ⇥ Natural/isZero   a ⇥ 1 + n
    ──────────────────────────────  ; "1 + n" means "a `Natural` literal greater
    f a ⇥ False                     ; than `0`"


`Natural/even` detects whether or not a `Natural` number is even:


    f ⇥ Natural/even   a ⇥ 0
    ────────────────────────
    f a ⇥ True


    f ⇥ Natural/even   a ⇥ 1
    ────────────────────────
    f a ⇥ False


    f ⇥ Natural/even
    a ⇥ 1 + n
    Natural/odd n ⇥ b
    ─────────────────  ; "1 + n" means "a `Natural` literal greater than `0`"
    f a ⇥ b


`Natural/odd` detects whether or not a `Natural` number is odd:


    f ⇥ Natural/odd   a ⇥ 0
    ───────────────────────
    f a ⇥ False


    f ⇥ Natural/odd   a ⇥ 1
    ───────────────────────
    f a ⇥ True


    f ⇥ Natural/odd
    a ⇥ 1 + n
    Natural/even n ⇥ b
    ──────────────────  ; "1 + n" means "a `Natural` literal greater than `0`"
    f a ⇥ b


`Natural/toInteger` transforms a `Natural` number into the corresponding
`Integer`:


    f ⇥ Natural/toInteger   a ⇥ n
    ─────────────────────────────
    f a ⇥ +n


`Natural/show` transforms a `Natural` number into a `Text` literal representing
valid Dhall code for representing that `Natural` number:


    f ⇥ Natural/show   a ⇥ n
    ────────────────────────
    f a ⇥ "n"


All of the built-in functions on `Natural` numbers are in normal form:


    ─────────────────────────────
    Natural/build ⇥ Natural/build


    ───────────────────────────
    Natural/fold ⇥ Natural/fold


    ───────────────────────────────
    Natural/isZero ⇥ Natural/isZero


    ───────────────────────────
    Natural/even ⇥ Natural/even


    ─────────────────────────
    Natural/odd ⇥ Natural/odd


    ─────────────────────────────────────
    Natural/toInteger ⇥ Natural/toInteger


    ───────────────────────────
    Natural/show ⇥ Natural/show


## `Text`

The `Text` type is in normal form:


    ───────────
    Text ⇥ Text


A `Text` literal with no interpolations is already in normal form:


    ─────────
    "s" ⇥ "s"


As a special case, if a `Text` literal has no content except for
interpolations, and all interpolations except one normalize to the
empty string, simplify the `Text` literal:


    t₀ ⇥ t₁   "ss₀…" ⇥ ""
    ─────────────────────
    "${t₀}ss₀…" ⇥ t₁


    t₀ ⇥ ""   "ss₀…" ⇥ t₁
    ─────────────────────
    "${t₀}ss₀…" ⇥ t₁


Otherwise, normalizing `Text` literals normalizes each interpolated
expression and inlines any interpolated expression that normalize to
`Text` literals:


    t₀ ⇥ "s₁"   "ss₀…" ⇥ "ss₂…"
    ───────────────────────────
    "s₀${t₀}ss₀…" ⇥ "s₀s₁ss₂…"


    t₀ ⇥ "s₁${t₁}ss₁…"   "ss₀…" ⇥ "ss₂…"
    ────────────────────────────────────
    "s₀${t₀}ss₀…" ⇥ "s₀s₁${t₁}ss₁…ss₂…"


    t₀ ⇥ t₁   "ss₀…" ⇥ "ss₁…"
    ─────────────────────────────  ; If no other rule matches
    "s₀${t₀}ss₀…" ⇥ "s₀${t₁}ss₁…"


The "text concatenation" operator is interpreted as two interpolations together:


    "${l}${r}" ⇥ s₀
    ───────────────
    l ++ r ⇥ s₀


The `Text/show` function replaces an uninterpolated `Text` literal
with another `Text` literal representing valid Dhall source code for
the original literal.  In particular, this function both quotes and
escapes the original literal so that if you were to render the escaped
`Text` value the result would appear to be the original `Text` input
to the function:

    -- Rendering the right-hand side gives the original argument: "abc\ndef"
    Text/show "abc\ndef" = "\"abc\\ndef\""

Escaping is done in such a way that the rendered result is not only valid
Dhall source code but also valid JSON when the `Text` does not contain any
Unicode code points above `\uFFFF`.  This comes in handy if you want to use
Dhall to generate Dhall code or to generate JSON.

The body of the `Text` literal escapes the following characters according to
these rules:

* `"`  → `\"`
* `$`  → `\u0024`
* `\`  → `\\`
* `\b` → `\\b`
* `\f` → `\\f`
* `\n` → `\\n`
* `\r` → `\\r`
* `\t` → `\\t`

Carefully note that `$` is not escaped as `\$` since that is not a valid JSON
escape sequence.

`Text/show` also escapes any non-printable characters as their Unicode escape
sequences:

* `\u0000`-`\u001F` → `\\u0000`-`\\u001F`

... since that is the only valid way to represent them within the Dhall grammar.

Or in other words:


    f ⇥ Text/show   a ⇥ "…\n…\$…\\…\"…\u0000…"
    ─────────────────────────────────────────── ; "…\n…\$…\\…\"…\u0000…" contains no interpolations
    f a ⇥ "\"…\\n…\\u0024…\\\\…\\\"…\\u0000…\""


Otherwise, in isolation `Text/show` is in normal form:


    ─────────────────────
    Text/show ⇥ Text/show


## `List`

The `List` type-level function is in normal form:


    ───────────
    List ⇥ List


Normalizing a `List` normalizes each field and the type annotation:


    T₀ ⇥ T₁
    ──────────────────────────
    [] : List T₀ ⇥ [] : List T₁


    t₀ ⇥ t₁   [ ts₀… ] ⇥ [ ts₁… ]
    ─────────────────────────────
    [ t₀, ts₀… ] ⇥ [ t₁, ts₁… ]


Lists are defined here via induction as if they were linked lists, but a real
implementation might represent them using another data structure under the hood.
Dhall does not impose time complexity requirements on list operations.

`List/build` and `List/fold` are inverses of one another, which leads to the
following fusion rule:


    f ⇥ List/build A₀   a ⇥ List/fold A₁ b
    ──────────────────────────────────────
    f a ⇥ b


Otherwise, fall back on each function's respective implementation.

`List/build` is the canonical introduction function for `List`s:


    f ⇥ List/build A₀
    ↑(1, a, 0, A₀) = A₁
    g (List A₀) (λ(a : A₀) → λ(as : List A₁) → [ a ] # as) ([] : List A₀) ⇥ b
    ─────────────────────────────────────────────────────────────────────────
    f g ⇥ b


`List/fold` is the canonical elimination function for `List`s:


    f ⇥ List/fold A₀ ([] : List A₁) B g   b₀ ⇥ b₁
    ─────────────────────────────────────────────
    f b₀ ⇥ b₁


    f ⇥ List/fold A₀ [ a, as… ] B g   g a (List/fold A₀ [ as… ] B g b₀) ⇥ b₁
    ────────────────────────────────────────────────────────────────────────
    f b₀ ⇥ b₁


Even though `List/build` and `List/fold` suffice for all `List` operations,
Dhall also supports built-in functions and operators on `List`s, both for
convenience and efficiency.

Use machine concatenation to simplify the "list concatenation" operator if both
arguments normalize to `List` literals:


    ls₀ ⇥ [ ls₁… ]
    rs₀ ⇥ [ rs₁… ]
    [ ls₁… ] # [ rs₁… ] ⇥ t
    ───────────────────────   ;  "[ ls₁… ] # [ rs₁… ]" means "use machine
    ls₀ # rs₀ ⇥ t             ;  concatenation"


Also, simplify the "list concatenation" operator if either argument normalizes
to an empty `List`:


    ls ⇥ [] : List T   rs₀ ⇥ rs₁
    ────────────────────────────
    ls # rs₀ ⇥ rs₁


    rs ⇥ [] : List T   ls₀ ⇥ ls₁
    ────────────────────────────
    ls₀ # rs ⇥ ls₁


Otherwise, normalize each argument:


    ls₀ ⇥ ls₁   rs₀ ⇥ rs₁
    ─────────────────────   ; If no other rule matches
    ls₀ # rs₀ ⇥ ls₁ # rs₁


`List/length` returns the length of a list:


    f ⇥ List/length A₀   a ⇥ [] : List A₁
    ─────────────────────────────────────
    f a ⇥ 0


    f ⇥ List/length A₀   as₀ ⇥ [ a, as₁… ]   1 + List/length A₀ [ as₁… ] ⇥ n
    ────────────────────────────────────────────────────────────────────────
    f as₀ ⇥ n


`List/head` returns the first element of a list:


    f ⇥ List/head A₀   as ⇥ [] : List A₁
    ────────────────────────────────────
    f as ⇥ None A₀


    f ⇥ List/head A₀   as ⇥ [ a, … ]
    ────────────────────────────────
    f as ⇥ Some a


`List/last` returns the last element of a list:


    f ⇥ List/last A₀   as ⇥ [] : List A₁
    ────────────────────────────────────
    f as ⇥ None A₀


    f ⇥ List/last A₀   as ⇥ [ …, a ]
    ────────────────────────────────
    f as ⇥ Some a


`List/indexed` tags each element of the list with the element's index:


    f ⇥ List/indexed A₀   as ⇥ [] : List A₁
    ───────────────────────────────────────────────
    f as ⇥ [] : List { index : Natural, value : A₀ }


    f ⇥ List/indexed A₀   as ⇥ [ a₀, a₁, …, ]
    ──────────────────────────────────────────────────────────────────
    f as ⇥ [ { index = 0, value = a₀ }, { index = 1, value = a₁ }, … ]


`List/reverse` reverses the elements of the list:


    f ⇥ List/reverse A₀   as ⇥ [] : List A₁
    ───────────────────────────────────────
    f as ⇥ [] : List A₁


    f ⇥ List/reverse A₀   as ⇥ [ a₀, a₁, … ]
    ────────────────────────────────────────
    f as ⇥ [ …, a₁, a₀ ]


All of the built-in functions on `List`s are in normal form:


    ───────────────────────
    List/build ⇥ List/build


    ─────────────────────
    List/fold ⇥ List/fold


    ─────────────────────────
    List/length ⇥ List/length


    ─────────────────────
    List/head ⇥ List/head


    ─────────────────────
    List/last ⇥ List/last


    ───────────────────────────
    List/indexed ⇥ List/indexed


    ───────────────────────────
    List/reverse ⇥ List/reverse


## `Optional`

The `Optional` and `None` functions are in normal form:


    ───────────────────
    Optional ⇥ Optional


    ───────────
    None ⇥ None


Normalize a `Some` expression by normalizing its argument:


    t₀ ⇥ t₁
    ─────────────────
    Some t₀ ⇥ Some t₁


`Optional/build` and `Optional/fold` are inverses of one another, which leads to
the following fusion rule:


    f ⇥ Optional/build A₀   a ⇥ Optional/fold A₁ b
    ──────────────────────────────────────────────
    f a ⇥ b


`Optional/build` is the canonical introduction function for `Optional` values:


    f ⇥ Optional/build A₀
    g (Optional A₀) (λ(a : A₀) → Some a) (None A₀) ⇥ b
    ──────────────────────────────────────────────────
    f g ⇥ b


`Optional/fold` is the canonical elimination function for `Optional` values:


    f ⇥ Optional/fold A₀ (Some a) B₀ g   g a ⇥ b₁
    ─────────────────────────────────────────────
    f b₀ ⇥ b₁


    f ⇥ Optional/fold A₀ (None A₁) B₀ g   b₀ ⇥ b₁
    ─────────────────────────────────────────────
    f b₀ ⇥ b₁


All of the built-in functions on `Optional` values are in normal form:


    ─────────────────────────────
    Optional/fold ⇥ Optional/fold


    ───────────────────────────────
    Optional/build ⇥ Optional/build


## Records

Normalizing a record type sorts the fields and normalizes the type of each
field:


    ───────
    {} ⇥ {}


    T₀ ⇥ T₁   { xs₀… } ⇥ { xs₁… }
    ───────────────────────────────────
    { x : T₀, xs₀… } ⇥ { x : T₁, xs₁… }


Normalizing a record value sorts the fields and normalizes each field:


    ─────────
    {=} ⇥ {=}


    t₀ ⇥ t₁   { xs₀… } ⇥ { xs₁… }
    ───────────────────────────────────
    { x = t₀, xs₀… } ⇥ { x = t₁, xs₁… }


Simplify a record selection if the argument is a record literal:


    t ⇥ { x = v, … }
    ──────────────────
    t.x ⇥ v


You can also project out more than one field into a new record:


    ─────────
    t.{} ⇥ {}


    t ⇥ { x = v, ts… }   { ts… }.{ xs… } ⇥ { ys… }
    ──────────────────────────────────────────────
    t.{ x, xs… } ⇥ { x = v, ys… }


    s ⇥ {}
    ───────────
    keys(s) ⇥ ε

    s ⇥ { x : T, ss… }
    keys(ss…) ⇥ ss₁…
    ─────────────────────
    keys(s) ⇥ x, ss₁…


    s ⇥ { x : T, ss… }
    keys(s) ⇥ s₁
    t.{s₁} ⇥ ts₁
    ───────────────
    t.(s) ⇥ ts₁


    t₀ ⇥ t₁
    s₀ ⇥ s₁
    ─────────────────  ; If no other rule matches
    t₀.(s₀) ⇥ t₁.(s₁)


The type system ensures that the selected field(s) must be present.

Otherwise, normalize the argument:


    t₀ ⇥ t₁
    ───────────  ; If no other rule matches
    t₀.x ⇥ t₁.x


Recursive record merge combines two records, recursively merging any fields that
collide.  The type system ensures that colliding fields must be records:


    l ⇥ {=}   r₀ ⇥ r₁
    ─────────────────
    l ∧ r₀ ⇥ r₁


    r ⇥ {=}   l₀ ⇥ l₁
    ─────────────────
    l₀ ∧ r ⇥ l₁


    ls₀ ⇥ { x = l₁, ls₁… }
    rs₀ ⇥ { x = r₁, rs₁… }
    l₁ ∧ r₁ ⇥ t
    { ls₁… } ∧ { rs₁… } ⇥ { ts… }
    { x = t, ts… } ⇥ e             ; To ensure the fields are sorted
    ─────────────────────────────
    ls₀ ∧ rs₀ ⇥ e


    ls₀ ⇥ { x = l₁, ls₁… }
    { ls₁… } ∧ rs ⇥ { ls₂… }
    { x = l₁, ls₂… } ⇥ e      ; To ensure the fields are sorted
    ────────────────────────  ; x ∉ rs
    ls₀ ∧ rs ⇥ e


    l₀ ⇥ l₁   r₀ ⇥ r₁
    ─────────────────   ; If no other rule matches
    l₀ ∧ r₀ ⇥ l₁ ∧ r₁


Right-biased record merge is non-recursive.  Field collisions are resolved by
preferring the field from the right record and discarding the colliding field
from the left record:


    l ⇥ e
    ───────────
    l ⫽ {=} ⇥ e


    r ⇥ e
    ───────────
    {=} ⫽ r ⇥ e


    ls₀ ⇥ { x = l₁, ls₁… }
    rs₀ ⇥ { x = r₁, rs₁… }
    { ls₁… } ⫽ { rs₁… } ⇥ { ts… }
    { x = r₁, ts… } ⇥ e            ; To ensure the fields are sorted
    ─────────────────────────────
    ls₀ ⫽ rs₀ ⇥ e


    ls₀ ⇥ { x = l₁, ls₁… }
    { ls₁… } ⫽ rs ⇥ { ls₂… }
    { x = l₁, ls₂… } ⇥ e      ;  To ensure the fields are sorted
    ────────────────────────  ;  x ∉ rs
    ls₀ ⫽ rs ⇥ e


    l₀ ⇥ l₁   r₀ ⇥ r₁
    ─────────────────   ; If no other rule matches
    l₀ ⫽ r₀ ⇥ l₁ ⫽ r₁


Recursive record type merge combines two record types, recursively merging any
fields that collide.  The type system ensures that colliding fields must be
record types:


    l ⇥ {}   r₀ ⇥ r₁
    ────────────────
    l ⩓ r₀ ⇥ r₁


    r ⇥ {}   l₀ ⇥ l₁
    ────────────────
    l₀ ⩓ r ⇥ l₁


    ls₀ ⇥ { x : l₁, ls₁… }
    rs₀ ⇥ { x : r₁, rs₁… }
    l₁ ⩓ r₁ ⇥ t
    { ls₁… } ⩓ { rs₁… } ⇥ { ts… }
    { x : t, ts… } ⇥ e             ; To ensure the fields are sorted
    ─────────────────────────────
    ls₀ ⩓ rs₀ ⇥ e


    ls₀ ⇥ { x : l₁, ls₁… }
    { ls₁… } ⩓ rs ⇥ { ls₂… }
    { x : l₁, ls₂… } ⇥ e      ; To ensure the fields are sorted
    ────────────────────────  ; x ∉ rs
    ls₀ ⩓ rs ⇥ e


    l₀ ⇥ l₁   r₀ ⇥ r₁
    ─────────────────   ; If no other rule matches
    l₀ ⩓ r₀ ⇥ l₁ ⩓ r₁


## Unions

Normalizing a union type sorts the alternatives and normalizes the type of each
alternative:


    ───────
    <> ⇥ <>


    T₀ ⇥ T₁   < xs₀… > ⇥ < xs₁… >
    ─────────────────────────────────────
    < x : T₀ | xs₀… > ⇥ < x : T₁ | xs₁… >


    < xs₀… > ⇥ < xs₁… >
    ───────────────────────────
    < x | xs₀… > ⇥ < x | xs₁… >


The language still supports a deprecated union literal syntax for selecting one
alternative of the union.  Normalizing this deprecated form sorts the
alternatives, normalizes the specified value, and normalizes the type of each
alternative:


    t₀ ⇥ t₁
    ───────────────────────
    < x = t₀ > ⇥ < x = t₁ >


    T₁₀ ⇥ T₁₁   < x₀ = t₀₀ | xs₀… > ⇥ < x₁ = t₀₁ | xs₁… >
    ────────────────────────────────────────────────────────────────
    < x₀ = t₀₀ | x₁ : T₁₀ | xs₀… > ⇥ < x₀ = t₀₁ | x₁ : T₁₁ | xs₁… >


However, the newer preferred syntax is to access a union constructor as if it
were a field of the union type:


    u ⇥ < x₀ : T₀ | xs… >
    ───────────────────────────
    u.x₀ ⇥ < x₀ : T₀ | xs… >.x₀


    u ⇥ < x₀ | xs… >
    ──────────────────────
    u.x₀ ⇥ < x₀ | xs… >.x₀


Normalizing this type of constructor access only normalizes the union type but
is otherwise inert.  The expression does not reduce further until supplied to a
`merge`.

`merge` expressions are the canonical way to eliminate a union literal.  The
first argument to `merge` is a record of handlers and the second argument is a
union value, which can be in one of three forms:

* A (deprecated) union literal of the form: `< x = v | … >`
* A union constructor for a non-empty alternative: `< x : T | … >.x v`
* A union constructor for an empty alternative: `< x | … >.x`

For union literals selecting non-empty alternatives, apply the handler of the
same label to the wrapped value of the union literal:


    t ⇥ { x = f, … }   u ⇥ < x = a | … >   f a ⇥ b
    ──────────────────────────────────────────────
    merge t u : T ⇥ b


    t ⇥ { x = f, … }   u ⇥ < x = a | … >   f a ⇥ b
    ──────────────────────────────────────────────
    merge t u ⇥ b


Union constructors for non-empty alternatives behave the same as union literals:


    t ⇥ { x = f, … }   u ⇥ < x : T₀ | … >.x a   f a ⇥ b
    ───────────────────────────────────────────────────
    merge t u : T ⇥ b


    t ⇥ { x = f, … }   u ⇥ < x : T | … >.x a   f a ⇥ b
    ──────────────────────────────────────────────────
    merge t u ⇥ b


For union constructors specifying empty alternatives, return the handler of the
matching label:


    t ⇥ { x = v, … }   u ⇥ < x | … >.x
    ──────────────────────────────────
    merge t u : T ⇥ v


    t ⇥ { x = v, … }   u ⇥ < x | … >.x
    ──────────────────────────────────
    merge t u ⇥ v


If the handler or union are abstract, then normalize each subexpression:


    t₀ ⇥ t₁   u₀ ⇥ u₁   T₀ ⇥ T₁
    ───────────────────────────────────  ; If no other rule matches
    merge t₀ u₀ : T₀ ⇥ merge t₁ u₁ : T₁


    t₀ ⇥ t₁   u₀ ⇥ u₁
    ─────────────────────────  ; If no other rule matches
    merge t₀ u₀ ⇥ merge t₁ u₁


## `Integer`

The `Integer` type is in normal form:


    ─────────────────
    Integer ⇥ Integer


An `Integer` literal is in normal form:


    ───────
    ±n ⇥ ±n


`Integer/toDouble` transforms an `Integer` into the corresponding `Double`:


    f ⇥ Integer/toDouble   a ⇥ ±n
    ─────────────────────────────
    f a ⇥ ±n.0

Note that if the magnitude of `a` is greater than 2^53, `Integer/toDouble a`
may result in loss of precision. A `Double` will be selected by rounding `a` to
the nearest `Double`. Ties go to the `Double` with an even least significant
bit. When the magnitude of `a` is greater than or equal to `c`, the magnitude
will round to `Infinity`, where `c = 2^1024 - 2^970 ≈ 1.8e308`.

`Integer/show` transforms an `Integer` into a `Text` literal representing valid
Dhall code for representing that `Integer` number:


    f ⇥ Integer/show   a ⇥ ±n
    ─────────────────────────
    f a ⇥ "±n"


Note that the `Text` representation of the rendered `Integer` should include
a leading `+` sign if the number is non-negative and a leading `-` sign if
the number is negative.

All of the built-in functions on `Integer`s are in normal form:


    ───────────────────────────
    Integer/show ⇥ Integer/show


    ───────────────────────────────────
    Integer/toDouble ⇥ Integer/toDouble



## `Double`

The `Double` type is in normal form:


    ───────────────
    Double ⇥ Double


A `Double` literal is in normal form:


    ─────────
    n.n ⇥ n.n


`Double/show` transforms a `Double` into a `Text` literal representing valid
Dhall code for representing that `Double` number:


    f ⇥ Double/show   a ⇥ n.n
    ─────────────────────────
    f a ⇥ "n.n"


The `Double/show` function is in normal form:


    ─────────────────────────
    Double/show ⇥ Double/show


The following 2 properties must hold for `Double/show`:

```
show (read (show (X : Double))) = show X

read (show (read (Y : Text))) = read Y
```

where `show : Double → Text` is shorthand for `Double/show` and `read : Text →
Double` is the function in the implementation of Dhall which takes a correctly
formated text representation of a `Double` as input and outputs a `Double`.


## Functions

Normalizing a function type normalizes the types of the input and output:


    A₀ ⇥ A₁   B₀ ⇥ B₁
    ───────────────────────────────
    ∀(x : A₀) → B₀ ⇥ ∀(x : A₁) → B₁


You can introduce an anonymous function using a λ:


    A₀ ⇥ A₁   b₀ ⇥ b₁
    ───────────────────────────────
    λ(x : A₀) → b₀ ⇥ λ(x : A₁) → b₁


You can eliminate an anonymous function through β-reduction:


    f ⇥ λ(x : A) → b₀
    ↑(1, x, 0, a₀) = a₁
    b₀[x ≔ a₁] = b₁
    ↑(-1, x, 0, b₁) = b₂
    b₂ ⇥ b₃
    ──────────────────────
    f a₀ ⇥ b₃


Function application falls back on normalizing both sub-expressions if none of
the preceding function application rules apply:


    f₀ ⇥ f₁   a₀ ⇥ a₁
    ─────────────────  ; If no other rule matches
    f₀ a₀ ⇥ f₁ a₁


## `let` expressions

For the purposes of normalization, an expression of the form:

    let x : A = a₀ in b₀

... is semantically identical to:

    (λ(x : A) → b₀) a₀

... and the normalization rules for `let` expressions reflect that semantic
equivalence:


    ↑(1, x, 0, a₀) = a₁
    b₀[x ≔ a₁] = b₁
    ↑(-1, x, 0, b₁) = b₂
    b₂ ⇥ b₃
    ─────────────────────────
    let x : A = a₀ in b₀ ⇥ b₃


    ↑(1, x, 0, a₀) = a₁
    b₀[x ≔ a₁] = b₁
    ↑(-1, x, 0, b₁) = b₂
    b₂ ⇥ b₃
    ─────────────────────
    let x = a₀ in b₀ ⇥ b₃


## Type annotations

Simplify a type annotation by removing the annotation:


    t₀ ⇥ t₁
    ───────────
    t₀ : T ⇥ t₁


## Imports

An expression with unresolved imports cannot be β-normalized.

