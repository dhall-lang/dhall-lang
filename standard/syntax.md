# Syntax

The following notation is a simplified version of the syntax found in
`./dhall.abnf`.  This simplified notation is used for all of the following
judgments:

```
m, n = 0 / 1 + n  ; Natural numbers

d = ±n            ; Integers

x, y              ; Variables

; Mnemonics for the most commonly used labels:
;
; Terms are lowercase:
;
;     a    = input term whose type is "A"
;     b    = output term whose type is "B"
;     f    = "f"unction
;     l, r = "l"eft and "r"ight term that share the same type
;     e    = term whose type is "E"
;     t    = term whose type is "T"
;     u    = term whose type is "U"
;
; Types are uppercase:
;
;     A  = type of the input term "a"
;     B  = type of the output term "b"
;     E  = type of the term "e"
;     T  = type of the term "t"
;     U  = type of the term "u"
;
; Constants that are `Type`, `Kind`, or `Sort` are lowercase:
;
;     c = "c"onstant
;     i = function's "i"nput type
;     o = function's "o"utput type
;
; Similar terms are distinguished by subscripts like `a₀`, `a₁`, …
;
; A term that represents zero or more values or key-value pairs ends with `s…`,
; such as `as…`
;
; Note that these are only informal mnemonics.  Dhall is a pure type system,
; which means that many places in the syntax permit terms, types, kinds, and
; sorts. The typing judgments are the authoritative rules for what expressions
, are permitted and forbidden.
a, b, f, l, r, e, t, u, A, B, E, T, U, c, i, o
  = x@n                               ; Identifier
                                      ; (`x` is short-hand for `x@0`)
  / λ(x : A) → b                      ; Anonymous function
  / ∀(x : A) → B                      ; Function type
                                      ; (`A → B` is short-hand for `∀(_ : A) → B`)
  / let x : A = a in b                ; Let expression with type annotation
  / let x     = a in b                ; Let expression without type annotation
  / if t then l else r                ; if-then-else expression
  / merge t u : T                     ; Union elimination with type annotation
  / merge t u                         ; Union elimination
  / [] : T                            ; Empty list literals with type annotation
  / [ t, ts… ]                        ; Non-empty list literals
  / t : T                             ; Type annotation
  / l || r                            ; Boolean or
  / l + r                             ; Natural addition
  / l ++ r                            ; Text append
  / l # r                             ; List append
  / l && r                            ; Boolean and
  / l ∧ r                             ; Recursive record merge
  / l ⫽ r                             ; Non-recursive right-biased record merge
  / l ⩓ r                             ; Recursive record type merge
  / l * r                             ; Natural multiplication
  / l == r                            ; Boolean equality
  / l != r                            ; Boolean inequality
  / l === r                           ; Equivalence (using ASCII to avoid
                                      ; confusion with the equivalence judgment)
  / f a                               ; Function application
  / t.x                               ; Field selection
  / t.{ xs… }                         ; Field projection
  / t.(s)                             ; Field projection by type
  / T::r                              ; Record completion
  / assert : T                        ; Assert judgemental equality
  / n.n                               ; Double-precision floating point literal
  / n                                 ; Natural number literal
  / ±n                                ; Integer literal
  / "s"                               ; Uninterpolated text literal
  / "s${t}ss…"                        ; Interpolated text literal
  / {}                                ; Empty record type
  / { x : T, xs… }                    ; Non-empty record type
  / {=}                               ; Empty record literal
  / { x = t, xs… }                    ; Non-empty record literal
  / <>                                ; Empty union type
  / < x : T | xs… >                   ; Union type with at least one non-empty
                                      ; alternative
  / < x | xs… >                       ; Union type with at least one empty
                                      ; alternative
  / missing                           ; Identity for import alternatives,
                                      ; will always fail to resolve
  / l ? r                             ; Alternative imports resolution
  / https://authority directory file  ; URL import
  / path file                         ; Absolute file path import
  / . path file                       ; Relative file path import
  / .. path file                      ; Relative file path import
  / ~ path file                       ; Home-anchored file path import
  / env:x                             ; Environment variable import
  / Some a                            ; Constructor for a present Optional value

                                      ; Reserved identifiers for builtins
  / Natural/build                     ; Natural introduction
  / Natural/fold                      ; Natural elimination
  / Natural/isZero                    ; Test if zero
  / Natural/even                      ; Test if even
  / Natural/odd                       ; Test if odd
  / Natural/toInteger                 ; Convert Natural to Integer
  / Natural/show                      ; Convert Natural to Text representation
  / Natural/subtract                  ; Perform truncated subtraction on two Naturals
  / Integer/toDouble                  ; Convert Integer to Double
  / Integer/show                      ; Convert Integer to Text representation
  / Integer/negate                    ; Invert sign of Integers, with positive
                                      ; values becoming negative and vice-versa
  / Integer/clamp                     ; Convert Integer to Natural by clamping
                                      ; negative values to zero
  / Double/show                       ; Convert Double to Text representation
  / List/build                        ; List introduction
  / List/fold                         ; List elimination
  / List/length                       ; Length of list
  / List/head                         ; First element of list
  / List/last                         ; Last element of list
  / List/indexed                      ; Tag elements with index
  / List/reverse                      ; Reverse list
  / Optional/build                    ; Optional introduction
  / Optional/fold                     ; Optional elimination
  / Text/show                         ; Convert Text to its own representation
  / Bool                              ; Bool type
  / Optional                          ; Optional type
  / Natural                           ; Natural type
  / Integer                           ; Integer type
  / Double                            ; Double type
  / Text                              ; Text type
  / List                              ; List type
  / True                              ; True term
  / False                             ; False term
  / None                              ; Absent Optional value
  / Type                              ; Type of terms
  / Kind                              ; Type of types
  / Sort                              ; Type of kinds
```

```haskell
module Syntax
    ( module Syntax
    , Natural
    , Text
    , Lens.transform
    ) where

import Control.Lens (Plated)
import Data.Text (Text)
import Data.Data (Data)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import qualified Control.Lens as Lens

data Expression
    = Variable Text Natural
    | Lambda Text Expression Expression
    | Pi Text Expression Expression
    | Application Expression Expression
    deriving stock (Data, Generic)

instance Plated Expression
```
