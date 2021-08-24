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
  / toMap t : T                       ; Conversion to a map with type annotation
  / toMap t                           ; Conversion to a map
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
  / e with k.ks… = v                  ; Nested record update
  / n.n                               ; Double-precision floating point literal
  / n                                 ; Natural number literal
  / ±n                                ; Integer literal
  / "s"                               ; Uninterpolated text literal
  / "s${t}ss…"                        ; Interpolated text literal
  / YYYY-MM-DD                        ; Date
  / hh:mm:ss                          ; Time
  / ±HH:MM                            ; Time zone
  / {}                                ; Empty record type
  / { k : T, ks… }                    ; Non-empty record type
  / {=}                               ; Empty record literal
  / { k = t, ks… }                    ; Non-empty record literal
  / <>                                ; Empty union type
  / < k : T | ks… >                   ; Union type with at least one non-empty
                                      ; alternative
  / < k | ks… >                       ; Union type with at least one empty
                                      ; alternative
  / missing                           ; Identity for import alternatives,
                                      ; will always fail to resolve
  / l ? r                             ; Alternative imports resolution
  / https://authority directory file  ; URL import
  / directory file                    ; Absolute file path import
  / . directory file                  ; Relative file path import
  / .. directory file                 ; Relative file path import
  / ~ directory file                  ; Home-anchored file path import
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
  / Text/show                         ; Convert Text to its own representation
  / Text/replace                      ; Replace a section of a Text literal
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
{-| This module contains the data types used to represent the syntax tree for
    a Dhall expression
-}

module Syntax
    ( -- * Types
      Expression(..)
    , Pattern(..)
    , Operator(..)
    , TextLiteral(..)
    , Builtin(..)
    , Constant(..)
    , ImportMode(..)
    , ImportType(..)
    , URL(..)
    , Scheme(..)
    , FilePrefix(..)
    , File(..)

      -- * Re-exports
    , Natural
    , Text
    ) where

import Crypto.Hash (Digest, SHA256)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Numeric.Natural (Natural)

import qualified Data.Time as Time

-- | Top-level type representing a Dhall expression
data Expression
    = Variable Text Natural
      -- ^ > x@n
    | Lambda Text Expression Expression
      -- ^ > λ(x : A) → b
    | Forall Text Expression Expression
      -- ^ > ∀(x : A) → B
    | Let Text (Maybe Expression) Expression Expression
      -- ^ > let x : A = a in b
      --   > let x     = a in b
    | LetPattern Pattern Expression Expression
      -- ^ see @Pattern@ for the different types of patterns
      --   > let { x = b, … } = a in z
    | If Expression Expression Expression
      -- ^ > if t then l else r
    | Merge Expression Expression (Maybe Expression)
      -- ^ > merge t u : T
      -- ^ > merge t u
    | ToMap Expression (Maybe Expression)
      -- ^ > toMap t : T
      -- ^ > toMap t
    | EmptyList Expression
      -- ^ > [] : T
    | NonEmptyList (NonEmpty Expression)
      -- ^ > [ t, ts… ]
    | Annotation Expression Expression
      -- ^ > t : T
    | Operator Expression Operator Expression
      -- ^ > l □ r
    | Application Expression Expression
      -- ^ > f a
    | Field Expression Text
      -- ^ > t.x
    | ProjectByLabels Expression [Text]
      -- ^ > t.{ xs… }
    | ProjectByType Expression Expression
      -- ^ > t.(s)
    | Completion Expression Expression
      -- ^ > T::r
    | Assert Expression
      -- ^ > assert : T
    | With Expression (NonEmpty Text) Expression
      -- ^ > e with k.ks… = v
    | DoubleLiteral Double
      -- ^ > n.n
    | NaturalLiteral Natural
      -- ^ > n
    | IntegerLiteral Integer
      -- ^ > ±n
    | TextLiteral TextLiteral
      -- ^ > "s"
      --   > "s${t}ss…"
    | DateLiteral Time.Day
    | TimeLiteral
        Time.TimeOfDay
        Int
        -- ^ Precision
    | TimeZoneLiteral Time.TimeZone
    | RecordType [(Text, Expression)]
      -- ^ > {}
      --   > { k : T, ks… }
    | RecordLiteral [(Text, Expression)]
      -- ^ > {=}
      --   > { k = t, ks… }
    | UnionType [(Text, Maybe Expression)]
      -- ^ > <>
      --   > < k : T | ks… >
      --   > < k | ks… >
    | Import ImportType ImportMode (Maybe (Digest SHA256))
    | Some Expression
      -- ^ > Some s
    | Builtin Builtin
    | Constant Constant
    deriving (Show)

data Pattern
    = PVariable Text
      -- ^ > x
    | PRecord [(Text, Pattern)]
      -- ^ > { x = y,  }
    | PAnnotation Pattern Expression
      -- ^ > x : T
    deriving (Show)

-- | Associative binary operators
data Operator
    = Or                  -- ^ > ||
    | Plus                -- ^ > +
    | TextAppend          -- ^ > ++
    | ListAppend          -- ^ > #
    | And                 -- ^ > &&
    | CombineRecordTerms  -- ^ > ∧
    | Prefer              -- ^ > ⫽
    | CombineRecordTypes  -- ^ > ⩓
    | Times               -- ^ > *
    | Equal               -- ^ > ==
    | NotEqual            -- ^ > !=
    | Equivalent          -- ^ > ===
    | Alternative         -- ^ > ?
    deriving (Show)

{-| Data structure used to represent an interpolated @Text@ literal

    A @Text@ literal without any interpolations has an empty list.  For example,
    the @Text@ literal @\"foo\"@ is represented as:

    > TextLiteral [] "foo"

    A @Text@ literal with interpolations has one list element per interpolation.
    For example, the @Text@ literal @\"foo${x}bar${y}baz\"@ is represented as:

    > TextLiteral [("foo", Variable "x" 0), ("bar", Variable "y" 0)] "baz"
-}
data TextLiteral = Chunks [(Text, Expression)] Text
    deriving (Show)

-- | This instance comes in handy for implementing @Text@-related operations
instance Semigroup TextLiteral where
    Chunks xys₀ z₀ <> Chunks [] z₁ =
        Chunks xys₀ (z₀ <> z₁)
    Chunks xys₀ z₀ <> Chunks ((x₁, y₁) : xys₁) z₁ =
        Chunks (xys₀ <> ((z₀ <> x₁, y₁) : xys₁)) z₁

-- | This instance comes in handy for implementing @Text@-related operations
instance Monoid TextLiteral where
    mempty = Chunks [] ""

-- | Builtin values
data Builtin
    = NaturalBuild
    | NaturalFold
    | NaturalIsZero
    | NaturalEven
    | NaturalOdd
    | NaturalToInteger
    | NaturalShow
    | NaturalSubtract
    | IntegerToDouble
    | IntegerShow
    | IntegerNegate
    | IntegerClamp
    | DoubleShow
    | ListBuild
    | ListFold
    | ListLength
    | ListHead
    | ListLast
    | ListIndexed
    | ListReverse
    | TextShow
    | TextReplace
    | Bool
    | Optional
    | Natural
    | Integer
    | Double
    | Text
    | List
    | True
    | False
    | None
    | Date
    | Time
    | TimeZone
    deriving (Show)

-- | Type-checking constants
data Constant
    = Type
    | Kind
    | Sort
    deriving (Eq, Ord, Show)

-- | How to interpret the path to the import
data ImportMode
    = Code      -- ^ The default behavior: import the path as code to interpret
    | RawText   -- ^ @as Text@: import the path as raw text
    | Location  -- ^ @as Location@: don't import and instead represent the path
                --   as a Dhall expression
    deriving (Show)

-- | Where to locate the import
data ImportType
    = Missing
        -- ^ > missing
    | Remote URL (Maybe Expression)
        -- ^ > https://authority directory file using headers
    | Path FilePrefix File
        -- ^ > /directory/file
        --   > ./directory/file
        --   > ../directory/file
        --   > ~/directory/file
    | Env Text
        -- ^ > env:x
    deriving (Show)

-- | Structured representation of an HTTP(S) URL
data URL = URL
    { scheme    :: Scheme
    , authority :: Text
    , path      :: File
    , query     :: Maybe Text
    }
    deriving (Show)

-- | The URL scheme
data Scheme
    = HTTP  -- ^ > http:\/\/
    | HTTPS -- ^ > https:\/\/
    deriving (Show)

-- | The anchor for a local filepath
data FilePrefix
    = Absolute  -- ^ @/@, an absolute path
    | Here      -- ^ @.@, a path relative to the current working directory
    | Parent    -- ^ @..@, a path relative to the parent working directory
    | Home      -- ^ @~@, a path relative to the user's home directory
    deriving (Show)

{-| Structured representation of a file path

    Note that the directory path components are stored in reverse order,
    meaning that the path @/foo\/bar\/baz@ is represented as:

    > File{ directory = [ "bar", "foo" ], file = "baz" }
-}
data File = File
    { directory :: [Text]  -- ^ Directory path components (in reverse order)
    , file :: Text         -- ^ File name
    }
    deriving (Show)
```
