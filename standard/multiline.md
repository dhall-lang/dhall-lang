# Multi-line literal semantics

Multi-line literals are syntactic sugar for double-quoted literals.  For
example, this expression:

    λ(x : Text) → ''
      ${x}    baz
          bar
        foo
        ''

... is syntactic sugar for this expression:

    λ(x : Text) → "${x}    baz\n    bar\n  foo\n  "

This document standardizes this conversion from a multi-line literal to a
double-quoted literal.

## Escaping

Multi-line literals use a different escaping mechanism than double-quoted
literals and the following `re-escape` judgment converts multi-line escape
sequences to double-quoted escape sequences:

    re-escape(s₀) = s₁

... where

* `s₀` (the input) is a double-quoted literal with multi-line escape sequences
* `s₁` (the output) is a double-quoted literal with double-quote escape
   sequences

`re-escape` replaces the `''${` escape sequence with `\${`:


    re-escape("ss₀…") = "ss₁…"
    ─────────────────────────────────
    re-escape("''${ss₀…") = "\${ss₁…"


`re-escape` also replaces the `'''` escape sequence with `''`:


    re-escape("ss₀…") = "ss₁…"
    ───────────────────────────────
    re-escape("'''ss₀…") = "''ss₁…"


Additionally, `re-escape` has to introduce escape sequences for characters
that need to be escaped for double-quoted literals:


    re-escape("ss₀…") = "ss₁…"
    ──────────────────────────────  ; c₀ ∈ { ", $, \ }
    re-escape("c₀ss₀…") = "c₀ss₁…"


For all other characters, re-escape leaves them unmodified:


    re-escape("ss₀…") = "ss₁…"
    ──────────────────────────────  ; c₀ is a character that does not need to
    re-escape("c₀ss₀…") = "c₀ss₁…"  ; be escaped


Note that the double-quoted literal passed to `re-escape` may contain
interpolated expressions, which this case handles:


    re-escape("ss₀…") = "ss₁…"
    ──────────────────────────────────
    re-escape("${t}ss₀…") = "${t}ss₁…"


## Indentation

Multi-line literals provide support for automatically stripping shared leading
spaces from each line after the opening single quotes (including the line before
the closing single quotes).  For example, this expression:

    ''
      foo
      bar
      ''

... is the same as this expression:

    ''
    foo
    bar
    ''

... which is also the same as this expression:

      ''
    foo
    bar
    ''

All three examples are equivalent to:

    "foo\nbar\n"

However, the previous examples are NOT the same as:

    ''
      foo
      bar
    ''

... which does not strip the two spaces before `foo` and `bar` because the
spaces are not also present before the closing single quotes:

    "  foo\n  bar\n"

This feature only strips spaces and tabs (i.e. `\u0020` and `\u0009`) and not
any other form of whitespace.

Stripping leading whitespace requires first computing the smallest indent using
the `indent` judgment:

    indent(s) = n

... where

* `s` (the input) is a multi-line literal
* `n` (the output) is the smallest indent (a string of only `\u0020` and `\u0009`)

There is always at least one line in a multi-line literal, which is the line
preceding the final pair of single quotes:


    ────────────────────  ; The "s" denotes the end of the leading indent, which
    indent(''             ; can be terminated by any of the following:
    ps'') = p             ; * any character other than `\u0020` or `\u0009`
                          ; * an interpolated expression
                          ; * the end of the multi-line literal (i.e. '')


The grammar enforces this by requiring a mandatory newline after the opening
single quotes, which means that an expression like this is not allowed:

    ''ABC''  -- Not legal

Note that string interpolation interrupts leading indentation, so this
multi-line literal:

    ''
    ${Natural/show 1}      foo
      bar
    ''

... is the same as this double-quoted literal:

    "${Natural/show 1}      foo\n  bar\n"

A multi-line literal with more than one line takes the longest common indent
prefix over all lines:


    indent(''                ; Find the longest common indent prefix for the
           ss'') = q         ; remainder of the literal

    ───────────────────────
    indent(''
    ps
    ss'') = lcip(p,q)


    lcip(x, y) = p

    ───────────────────────────────
    lcip(\u0020x, \u0020y) = \u0020p


    lcip(x, y) = p

    ───────────────────────────────
    lcip(\u0009x, \u0009y) = \u0009p


    ─────────────────────────────── ; p ∉ { \u0009, \u0020 }
    lcip(px, py) = ""


    ─────────────────────────────── ; p ≠ q
    lcip(px, qy) = ""


Blank lines, however, are not counted towards this prefix, unless they are the
last line:


    indent(''
           ss'') = p

    ───────────────────────
    indent(''

           ss'') = p


    ───────────────────────
    indent(
    ''
    '') = ""


## Line endings

As in the rest of Dhall, you can use either a plain LF character as a line
ending (`%x0A` in ABNF) or a CR followed by a LF (`%x0D.0A` in ABNF).  However,
the value of the resulting string will always use `%x0A`-style line endings,
regardless of which was used in the Dhall source.  This means that the meaning
of a Dhall file does not depend on the specific type of line ending used.

Note: If you really need a `%x0D.0A`-style line ending, you can insert a CR
character at the end of a line by interpolation:


    ''
    CRLF line ending${"\r"}
    ''


## Desugaring

Once you can compute the leading indent, you can convert the multi-line literal
into a double-quoted literal using the `flatten` judgment, which strips the
leading indent and converts escape codes:

    flatten(n, s₀) = s₁

... where

* `n` (the input) is the number of leading codepoints to strip from each line
* `s₀` (the input) is a multi-line literal
* `s₁` (the output) is a double-quoted literal

There is always at least one line in a multi-line literal:


    re-escape("s₀") = "s₁"
    ────────────────────     ; The "s" denotes everything after the first `n`
    flatten(n, ''            ; codepoints, which could be even more whitespace
    ␣␣␣␣␣n␣␣␣␣␣s₀'') = "s₁"  ; if not all lines shared the same indent


... but no newline is emitted in the corresponding double-quoted literal.  The
first newline after the opening `''` quotes is not preserved in the conversion
to a double-quoted literal.  For example, this multi-line literal:

    ''
    foo''

... is equivalent to this double-quoted literal:

    "foo"

However, each line after the first line does add a newline in the corresponding
double-quoted literal:


    re-escape("s₀") = "s₁"

    flatten(n, ''
               ss₀'') = "ss₁"
    ────────────────────────────
    flatten(n, ''
    ␣␣␣␣␣n␣␣␣␣␣s₀
               ss'') = "s₀\nss₁"


So, for example, this multi-line literal:

    ''
    foo
    bar''

... is the same as this double-quoted literal:

    "foo\nbar"

Then the `to-double-quotes` judgement combines `indent` with `flatten`:

    to-double-quotes(s₀) = s₁

... where

* `s₀` (the input) is a multi-line literal
* `s₁` (the output) is a double-quoted literal

```
indent(s₀) = p   flatten(length(p), s₀) = s₁
────────────────────────────────────────────
to-double-quotes(s₀) = s₁
```

The `to-double-quotes` judgment represents the logic for desugaring a
multi-line literal to a double-quoted literal at parse time.
