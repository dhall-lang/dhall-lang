# Multi-line literal semantics

```haskell
{-| This module implements the logic for desugaring single-quoted @Text@
    literals
-}
module Multiline
    ( -- * Desugaring
      toDoubleQuotes
    ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Prelude hiding (lines, unlines)
import Syntax (TextLiteral(..))

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Semigroup     as Semigroup
import qualified Data.Text          as Text
```

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
   but without interpolated expressions
* `s₁` (the output) is a double-quoted literal with double-quote escape
   sequences but without interpolated expressions

```haskell
-- | Interpret single-quoted escape sequences
reEscape :: Text -> Text
```

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
    re-escape("c₀ss₀…") = "\c₀ss₁…"


For all other characters, re-escape leaves them unmodified:


    re-escape("ss₀…") = "ss₁…"
    ──────────────────────────────  ; c₀ is a character that does not need to
    re-escape("c₀ss₀…") = "c₀ss₁…"  ; be escaped


Note that the double-quoted literal passed to `re-escape` may contain
interpolated expressions, which this case handles:


    re-escape("ss₀…") = "ss₁…"
    ──────────────────────────────────
    re-escape("${t}ss₀…") = "${t}ss₁…"


```haskell
reEscape =
      Text.replace "''${" "${"
    . Text.replace "'''"  "''"
    -- Note that `reEscape` does not escape `"`, `$`, or `\` because the
    -- `TextLiteral` type stores `Text` values in their unescaped forms.
```

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

Stripping leading whitespace requires first computing the largest shared prefix
using the `indent` judgment:

    indent(ss) = p

... where

* `ss` (the input) is a multi-line literal
* `p` (the output) is the largest shared prefix (a string of only `\u0020` and
   `\u0009`)

```haskell
-- | Computes the leading indentation that should be stripped from each line
indent :: TextLiteral -> Text
```

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


```haskell
{- The Haskell logic does not closely follow the standard here because there is
   a simpler way to implement this when we're not constrained by the language of
   natural deduction

   The key trick is to implement the `lines` and `unlines` utilities which split
   and join `TextLiteral`s on line boundaries, respectively.  The majority of
   the implementation complexity is in the `lines` function and once you've
   implemented that then everything else follows pretty easily.
-}

indent textLiteral =
    foldr1 lcip (fmap toPrefix (removeEmpty (lines textLiteral)))
  where
    toPrefix (Chunks           []  z) = Text.takeWhile prefixCharacter z
    toPrefix (Chunks ((x, _) : _ ) _) = Text.takeWhile prefixCharacter x

-- | Removes all lines that are blank, except for the last line
removeEmpty :: NonEmpty TextLiteral -> NonEmpty TextLiteral
removeEmpty ls = prepend (filter (not . isEmpty) initLines) (pure lastLine)
  where
    initLines = NonEmpty.init ls
    lastLine  = NonEmpty.last ls

    isEmpty (Chunks [] "") = True
    isEmpty  _             = False

-- | Only spaces and tabs can be stripped from leading indentation
prefixCharacter :: Char -> Bool
prefixCharacter c = c == ' ' || c == '\t'

-- | Return the longest common prefix
lcip :: Text -> Text -> Text
lcip x y = case Text.commonPrefixes x y of
    Nothing             -> ""
    Just (prefix, _, _) -> prefix

{-| Split a `TextLiteral` on newline boundaries to create a list of
    `TextLiteral`s (one for each line, not including the newline)
-}
lines :: TextLiteral -> NonEmpty TextLiteral
lines = loop mempty
  where
    loop currentLine (Chunks [] z) =
        (currentLine <> headLine) :| tailLines
      where
        headLine :| tailLines = fmap toChunk (lines_ z)

    loop currentLine (Chunks ((x, y) : xys) z) =
        case lines_ x of
            _ :| [] ->
                loop (currentLine <> Chunks [(x, y)] "") (Chunks xys z)

            l0 :| l1 : ls ->
                let ls' = l1 :| ls

                in  NonEmpty.cons
                        (currentLine <> toChunk l0)
                        (prepend
                            (fmap toChunk (NonEmpty.init ls'))
                            (loop
                                (Chunks [(NonEmpty.last ls', y)] "")
                                (Chunks xys z)
                            )
                        )

    -- Like `lines` for plain `Text` values
    lines_ :: Text -> NonEmpty Text
    lines_ text = Semigroup.sconcat (fmap (splitOn "\n") (splitOn "\r\n" text))

-- | Concatenate a list and a non-empty list
prepend :: [a] -> NonEmpty a -> NonEmpty a
prepend      []        ys = ys
prepend (x : xs) (y :| ys)= x :| (xs <> (y : ys))

{-| `Text.splitOn` currently always returns a non-empty list, but the type does
    not express that, so this is a type-safe wrapper that enforces that the
    result is non-empty (even if the upstream implementation changes in
    behavior)
-}
splitOn :: Text -> Text -> NonEmpty Text
splitOn needle haystack =
    case Text.splitOn needle haystack of
        l : ls -> l  :| ls
        []     -> "" :| []

-- | Promote a plain (uninterpolated) `Text` value to a `TextLiteral`
toChunk :: Text -> TextLiteral
toChunk text = Chunks [] text
```

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

```haskell
{-| Strip a fixed number of characters from each line and interpret escape
    sequences
-}
flatten :: Int -> TextLiteral -> TextLiteral
```

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


```haskell
flatten indentLength textLiteral =
    escape (unlines (fmap stripPrefix (lines textLiteral)))
  where
    stripPrefix (Chunks [] z) =
        Chunks [] (Text.drop indentLength z)
    stripPrefix (Chunks ((x, y) : xys) z) =
        Chunks ((Text.drop indentLength x, y) : xys) z

-- | Escape each `Text` segment of a `TextLiteral`
escape :: TextLiteral -> TextLiteral
escape (Chunks xys z) = Chunks xys' z'
  where
    xys' = do
        (x, y) <- xys
        return (reEscape x, y)

    z' = reEscape z

{-| This is the inverse of `lines`, which joins `TextLiteral`s back together
    by intercalating newline characters
-}
unlines :: NonEmpty TextLiteral -> TextLiteral
unlines = foldr1 join
  where
    join l r = l <> toChunk "\n" <> r
```

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

```haskell
{-| The top-level utility that converts a single-quoted `TextLiteral` into the
    equivalent double-quoted `TextLiteral` by stripping leading indentation and
    fixing all escape sequences
-}
toDoubleQuotes :: TextLiteral -> TextLiteral
```

```
indent(s₀) = p   flatten(length(p), s₀) = s₁
────────────────────────────────────────────
to-double-quotes(s₀) = s₁
```

```haskell
toDoubleQuotes s₀ = s₁
  where
    p = indent s₀

    s₁ = flatten (Text.length p) s₀
```

The `to-double-quotes` judgment represents the logic for desugaring a
multi-line literal to a double-quoted literal at parse time.
