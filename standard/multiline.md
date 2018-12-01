# Multi-line literal semantics

Multi-line literals are syntactic sugar for double-quoted literals.  For
example, this expression:

    λ(x : Text) → ''
      ${x}
          bar
        foo
        ''

... is syntactic sugar for this expression:

    λ(x : Text) → "${x}\n    bar\n  foo\n  "

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

Multi-line literals provide support for automatically stripping leading spaces
from each line.  For example, this expression:

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

... but NOT the same as this expression:

      ''
    foo
    bar
      ''

The first three examples are equivalent to:

    "foo\nbar\n"

... whereas the fourth example is equivalent to:

    "foo\nbar\n  "

This feature only strips spaces (i.e. `\u0020`) and not any other form of
whitespace.  For example, leading tabs are not stripped in this way.

Stripping leading whitespace requires first computing the size of the smallest
indent using the `indent` judgment:

    indent(s) = n

... where

* `s` (the input) is a multi-line literal
* `n` (the output) is the length of the smallest indent

There is always at least one line in a multi-line literal, which is the line
preceding the final pair of single quotes:


    ────────────────────  ; The "s" denotes the end of the leading spaces, which
    indent(''             ; can be terminated by any of the following:
    ␣␣␣n␣␣␣s'') = n       ; * a non-space character
                          ; * an interpolated expression
                          ; * the end of the multi-line literal (i.e. '')
                          ;
                          ; "␣␣␣n␣␣␣" is a short-hand for "n spaces"


The grammar enforces this by requiring a mandatory newline after the opening
single quotes.

A multi-line literal with more than one line takes the minimum indent over all
lines:


    indent(''                ; Find the minimum indent for the remainder of the
           ss'') = m         ; literal

    ───────────────────────
    indent(''
    ␣␣␣n␣␣␣s
           ss'') = min(m,n)  ;


## Desugaring

Once you can compute the leading indent, you can convert the multi-line literal
into a double-quoted literal using the `flatten` judgment, which strips the
leading indent and converts escape codes:

    flatten(n, s₀) = s₁

... where

* `n` (the input) is the number of leading spaces to strip from each line
* `s₀` (the input) is a multi-line literal
* `s₁` (the output) is a double-quoted literal

There is always at least one line in a multi-line literal:


    re-escape("s₀") = "s₁"
    ────────────────────     ; The "s" denotes everything after the first `n`
    flatten(n, ''            ; spaces, which could be even more spaces if
    ␣␣␣␣␣n␣␣␣␣␣s₀'') = "s₁"  ; not all lines shared the same indent


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


    indent(s₀) = n   flatten(n, s₀) = s₁
    ────────────────────────────────────
    to-double-quotes(s₀) = s₁


The `to-double-quotes` judgment represents the logic for desugaring a
multi-line literal to a double-quoted literal at parse time.
